open Core_kernel.Std
open Regular.Std
open Bap.Std
open Or_error
open Format

module Dis = Disasm_expert.Basic

type t = {
  arch      : arch;
  segments  : Image.segment Table.t;
  brancher  : Brancher.t;
  insn_map  : (mem * Dis.full_insn) Addr.Map.t;
  insn_rcfg : Superset_rcfg.t;
} [@@deriving fields]

let bad_of_arch arch = 
  Addr.of_int ~width:(Size.in_bits @@ Arch.addr_size arch) 0

let bad_of_addr addr =
  Addr.of_int ~width:(Addr.bitwidth addr) 0

let get_bad superset = 
  bad_of_arch superset.arch

let find superset addr = 
  Map.find superset.insn_map addr

let rebuild ?insn_map ?insn_rcfg superset =
  let insn_map = Option.value insn_map ~default:superset.insn_map in
  let insn_rcfg = Option.value insn_rcfg ~default:superset.insn_rcfg in
  {
    arch      = superset.arch;
    brancher  = superset.brancher;
    segments  = superset.segments;
    insn_map  = insn_map;
    insn_rcfg = insn_rcfg;
  }

let remove superset addr = 
  let insn_map = Map.remove superset.insn_map addr in
  Superset_rcfg.G.remove_vertex superset.insn_rcfg addr;
  rebuild ~insn_map superset

let add superset mem insn =
  let addr = Memory.min_addr mem in
  Superset_rcfg.G.add_vertex superset.insn_rcfg addr;
  let insn_map = Map.add superset.insn_map ~key:addr ~data:(mem, insn) in
  rebuild ~insn_map superset

let replace superset mem insn = 
  let addr = Memory.min_addr mem in
  let superset = remove superset addr in
  add superset mem insn

let format_cfg ?format superset =
  let format = Option.value format ~default:Format.std_formatter in
  Superset_rcfg.Gml.print format superset.insn_rcfg

(* TODO iter, fold, format to_string *)
let cfg_to_string superset = 
  let format = Format.str_formatter in
  format_cfg ~format superset;
  Format.flush_str_formatter ()

let insns_to_string superset = 
  Sexp.to_string @@ Addr.Map.sexp_of_t 
    (Tuple2.sexp_of_t Memory.sexp_of_t
       Dis.Insn.sexp_of_t)
    superset.insn_map

let next_chunk leftovers mem ~addr =
  let next_addr = Addr.succ addr in
  match Map.find leftovers next_addr with
  | Some(view) -> Ok(view)
  | None -> Memory.view ~from:next_addr mem

let run_seq dis mem = 
  let open Seq.Generator in 
  let leftovers = Addr.Map.empty in
  let rec disasm leftovers cur_mem = 
    let elem, leftovers = match Dis.insn_of_mem dis cur_mem with
      | Ok (m, insn, lo) ->
        (match lo with
         | `finished -> (m, insn), leftovers
         | `left (lo_mem) -> 
           let leftovers = Map.remove leftovers 
               Memory.(min_addr cur_mem) in
           let leftovers = Map.add leftovers 
               Memory.(min_addr lo_mem) lo_mem in
           (m, insn), leftovers)
      | Error _ -> (cur_mem, None), leftovers in
    yield elem >>= fun () ->
    match next_chunk leftovers mem ~addr:(Memory.min_addr cur_mem) with
    | Ok next -> disasm leftovers next
    | Error _ -> return () in
  run (disasm leftovers mem)

let run dis ~accu ~f mem =
  Seq.fold ~init:accu ~f:(fun x y -> f y x) (run_seq dis mem)

let disasm ?(backend="llvm") ~accu ~f arch mem =
  Dis.with_disasm ~backend (Arch.to_string arch) ~f:(fun d -> Ok(run d ~accu ~f mem))

let lift_insn lift_fn (mem,insn) =
  let lift_fn = lift_fn mem in
  let insn = Option.map insn ~f:lift_fn in
  Option.map insn ~f:(fun bil -> (mem, bil |> ok_exn))

let lift arch insns =
  let module Target = (val target_of_arch arch) in
  let lifter = Target.lift in
  let lifted_superset = Addr.Map.empty in
  List.fold insns ~init:lifted_superset
    ~f:(fun lifted_superset (mem, insn) -> 
        match lift_insn lifter (mem, insn) with
        | Some (mem, bil) -> 
          Map.add lifted_superset ~key:(Memory.min_addr mem)
            ~data:(bil, Memory.length mem)
        | None -> lifted_superset
      )

module With_exn = struct
  let disasm ?backend ~accu ~f arch mem = 
    disasm ?backend ~accu ~f arch mem
end

let memmap_all ?backend arch mem =
  let filter_add elem memmap =
    let (mem, insn) = elem in 
    Option.value_map insn ~default:memmap
      ~f:(Memmap.add memmap mem) in
  With_exn.disasm ?backend ~accu:Memmap.empty ~f:filter_add arch mem

let raw_superset_to_map ?insn_map raw_superset = 
  print_endline "superset_to_map";
  let insn_map = Option.value insn_map ~default:Addr.Map.empty in
  let insn_map = List.fold_left ~init:insn_map raw_superset
      ~f:(fun insn_map (mem, insn) ->
          let addr = (Memory.min_addr mem) in
          match insn with
          | Some(insn) -> 
            Addr.Map.add insn_map addr (mem, insn)
          | None -> insn_map
        ) in
  printf "raw_superset_to_map length %d\n" Addr.Map.(length insn_map);
  insn_map

let update_with_mem ?backend superset mem =
  printf "superset_cfg_of_mem length %d\n" Memory.(length mem);
  disasm ?backend ~accu:[] ~f:List.cons superset.arch mem >>|
  fun raw_superset -> (
    let superset_rcfg = superset.insn_rcfg in
    let brancher = superset.brancher in
    let insn_map = superset.insn_map in
    (* raw_superset has an option because it is fresh out of mem, *)
    (* insn_map does not because had been filtered before *)
    let insn_map = raw_superset_to_map ~insn_map raw_superset in
    let insn_rcfg = Superset_rcfg.rcfg_of_raw_superset
        ~superset_rcfg brancher raw_superset in
    rebuild ~insn_map ~insn_rcfg superset
  )

let with_img ~accu ~backend img ~f = 
  let segments = Table.to_sequence @@ Image.segments img in
  Seq.fold segments ~init:accu ~f:(fun accu (mem, segment) ->
      if Image.Segment.is_executable segment then
        (f ~accu ~backend mem |> ok_exn)
      else accu 
    )

let superset_of_img ~backend img =
  print_endline "superset_of_img";
  let arch = Image.arch img in
  let brancher = Brancher.of_bil arch in
  let superset = {
    arch          = arch;
    insn_rcfg     = Superset_rcfg.G.create ();
    insn_map      = Addr.Map.empty;
    brancher      = brancher;
    segments      = Image.segments img;
  } in
  with_img ~accu:superset ~backend img
    ~f:(fun ~accu ~backend mem -> 
        update_with_mem ~backend accu mem
      )

let superset_disasm_of_file ~backend binary = 
  let img  = Common.img_of_filename binary in
  superset_of_img ~backend img
