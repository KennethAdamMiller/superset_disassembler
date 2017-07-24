open Core_kernel.Std
open Regular.Std
open Bap.Std
open Or_error
open Format

module Dis = Disasm_expert.Basic

type 'a t = {
  arch      : arch;
  segments  : Image.segment Table.t;
  brancher  : Brancher.t;
  data      : 'a;
  insn_rcfg : Superset_rcfg.t;
} [@@deriving fields]

let contains_addr superset addr = 
  let segments = Table.to_sequence superset.segments in
  Seq.fold segments ~init:false ~f:(fun status (mem, segment) ->
      status || Memory.contains mem addr)

let bad_of_arch arch = 
  Addr.of_int ~width:(Size.in_bits @@ Arch.addr_size arch) 0

let bad_of_addr addr =
  Addr.of_int ~width:(Addr.bitwidth addr) 0

let get_bad superset = 
  bad_of_arch superset.arch

let mark_bad superset addr = 
  Superset_rcfg.G.add_edge superset.insn_rcfg (get_bad superset) addr

let get_data superset = superset.data

let len_at superset at = 
  let insn_map = get_data superset in
  match Map.find insn_map at with
  | None -> 0
  | Some(mem, _) -> Memory.length mem

let create ?insn_rcfg arch data =
  let insn_rcfg = Option.value insn_rcfg 
      ~default:(Superset_rcfg.G.create ()) in
  {
    arch = arch;
    segments = Table.empty;
    brancher = Brancher.of_bil arch;
    data = data;
    insn_rcfg = insn_rcfg;
  }

let rebuild ?data ?insn_rcfg superset =
  let data = Option.value data ~default:superset.data in
  let insn_rcfg = Option.value insn_rcfg ~default:superset.insn_rcfg in
  {
    arch      = superset.arch;
    brancher  = superset.brancher;
    segments  = superset.segments;
    data      = data;
    insn_rcfg = insn_rcfg;
  }

let drop superset =
  rebuild ~data:() superset

let remove superset addr = 
  Superset_rcfg.G.remove_vertex superset.insn_rcfg addr;
  rebuild superset

let add superset mem insn =
  let addr = Memory.min_addr mem in
  Superset_rcfg.G.add_vertex superset.insn_rcfg addr;
  rebuild superset

let replace superset mem insn = 
  let addr = Memory.min_addr mem in
  let superset = remove superset addr in
  add superset mem insn

let format_cfg ?format superset =
  let format = Option.value format ~default:Format.std_formatter in
  Superset_rcfg.Gml.print format superset.insn_rcfg

let cfg_to_string superset = 
  let format = Format.str_formatter in
  format_cfg ~format superset;
  Format.flush_str_formatter ()


type op_trie = 
  | Finish of bil
  | Continue of op_trie Op.Table.t

let lifted_map = Int.Table.create ()
let is_pc_relevant insn = 
  Dis.Insn.is insn Kind.(`May_affect_control_flow)

let find_cached insn = 
  match Hashtbl.find lifted_map (Dis.Insn.code insn) with
  | None -> None
  | Some optrie -> 
    let ops = (Dis.Insn.ops insn) in
    let rec find_bil idx optrie = 
      let op =  Array.get ops idx in
      match Hashtbl.find optrie op with
      | Some(Finish(cached_bil)) -> Some(cached_bil)
      | Some (Continue (optrie)) ->
        find_bil (idx+1) optrie
      | None -> None in
    find_bil 0 optrie

let trie_of_op_array ?op_tbl ops bil = 
  let op_tbl = Option.value op_tbl ~default:(Op.Table.create ()) in
  let _ = Array.foldi ~init:op_tbl ops ~f:(fun idx cur_tbl op -> 
      if idx < Array.(length ops) then (
        let new_tbl = Op.Table.create () in
        Pervasives.ignore 
          (Op.Table.add cur_tbl ~key:op ~data:(Continue(new_tbl)));
        new_tbl
      ) else (
        Pervasives.ignore 
          (Op.Table.add cur_tbl ~key:op ~data:(Finish(bil)));
        cur_tbl
      )
    ) in
  op_tbl

let store_lifted insn bil = 
  if not (is_pc_relevant insn) then (
    Hashtbl.change lifted_map (Dis.Insn.code insn) 
      ~f:(fun t ->
          let ops = Dis.Insn.ops insn in
          match t with
          | None -> 
            Some(trie_of_op_array ops bil)
          | Some(op_tbl) ->
            Some(trie_of_op_array ~op_tbl ops bil)
        )
  )

let next_chunk mem ~addr =
  let next_addr = Addr.succ addr in
  Memory.view ~from:next_addr mem

let run_seq dis mem = 
  let open Seq.Generator in 
  let rec disasm cur_mem = 
    let elem = match Dis.insn_of_mem dis cur_mem with
      | Ok (m, insn, _) -> (m, insn)
      | Error _ -> (cur_mem, None) in
    yield elem >>= fun () ->
    match next_chunk mem ~addr:(Memory.min_addr cur_mem) with
    | Ok next -> disasm next
    | Error _ -> return () in
  run (disasm mem)

let run dis ~accu ~f mem =
  Seq.fold ~init:accu ~f:(fun x y -> f y x) (run_seq dis mem)

let disasm ?(backend="llvm") ~accu ~f arch mem =
  Dis.with_disasm ~backend (Arch.to_string arch) ~f:(fun d -> Ok(run d ~accu ~f mem))

let lift_insn lift_fn (mem,insn) =
  match insn with
  | None -> None
  | Some(insn) -> 
    match find_cached insn with
    | Some cached_bil -> 
      Some(mem, cached_bil)
    | None -> 
      match lift_fn mem insn with 
      | Ok(lifted) -> 
        store_lifted insn lifted;
        Some(mem, lifted)
      | _ -> None 

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
    disasm ?backend ~accu ~f arch mem |> ok_exn
end

let memmap_all ?backend arch mem =
  let filter_add elem memmap =
    let (mem, insn) = elem in 
    Option.value_map insn ~default:memmap
      ~f:(Memmap.add memmap mem) in
  With_exn.disasm ?backend ~accu:Memmap.empty ~f:filter_add arch mem

let add_to_map insn_map (mem, insn) =
  let addr = (Memory.min_addr mem) in
  Addr.Map.add insn_map addr (mem, insn)

let sexp_of_mem mem = 
  let endianness = Memory.endian mem in
  let maddr = Memory.min_addr mem in
  let bstr_mem = Memory.to_string mem in
  Tuple3.sexp_of_t 
    Addr.sexp_of_endian
    Addr.sexp_of_t
    String.sexp_of_t (endianness, maddr, bstr_mem)

let mem_of_sexp sexp_mem =
  let (endianness, maddr, mem) = 
    Tuple3.t_of_sexp
      Addr.endian_of_sexp
      Addr.t_of_sexp
      String.t_of_sexp sexp_mem in
  let mem = Bigstring.of_string mem in
  Memory.create endianness maddr mem |> ok_exn

let insn_map_to_string insn_map =
  Sexp.to_string @@ Addr.Map.sexp_of_t 
    (fun (mem, _) -> sexp_of_mem mem) insn_map

let insn_map_of_string map_str = 
  let map_sexp = Sexp.of_string map_str in
  Addr.Map.t_of_sexp (fun m -> mem_of_sexp m, None) map_sexp

let meta_of_string meta_str = 
  let sexp_meta = Sexp.of_string meta_str in
  Arch.t_of_sexp sexp_meta

let meta_to_string superset = 
  Sexp.to_string (Arch.sexp_of_t superset.arch)

let raw_superset_to_map ?insn_map raw_superset = 
  let insn_map = Option.value insn_map ~default:Addr.Map.empty in
  let insn_map = List.fold_left ~init:insn_map raw_superset
      ~f:add_to_map in
  insn_map

let update_with_mem ?backend ?f superset mem =
  let superset_rcfg = superset.insn_rcfg in
  let update = Option.value f ~default:(fun (m, i) a -> a) in
  let f (mem, insn) accu =
    Superset_rcfg.add ~superset_rcfg mem insn;
    update (mem, insn) accu in
  disasm ?backend ~accu:superset ~f superset.arch mem

let with_img ~accu ~backend img ~f = 
  let segments = Table.to_sequence @@ Image.segments img in
  Seq.fold segments ~init:accu ~f:(fun accu (mem, segment) ->
      if Image.Segment.is_executable segment then
        (f ~accu ~backend mem |> ok_exn)
      else accu 
    )

let superset_of_img ~data ?f ~backend img =
  let arch = Image.arch img in
  let brancher = Brancher.of_bil arch in
  let superset = {
    data          = data;
    arch          = arch;
    insn_rcfg     = Superset_rcfg.G.create ();
    brancher      = brancher;
    segments      = Image.segments img;
  } in
  with_img ~accu:superset ~backend img
    ~f:(fun ~accu ~backend mem -> 
        update_with_mem ~backend accu mem ?f
      )

let superset_disasm_of_file ~backend ~data ?f binary = 
  let img  = Common.img_of_filename binary in
  superset_of_img ~data ~backend img ?f
