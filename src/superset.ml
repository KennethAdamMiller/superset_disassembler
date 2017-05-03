open Core_kernel.Std
open Regular.Std
open Bap.Std
open Or_error

module Dis = Disasm_expert.Basic

type t = {
  img       : Image.t;
  brancher  : Brancher.t;
  insn_map  : (mem * Dis.full_insn) Addr.Map.t;
  insn_rcfg : Superset_rcfg.t;
}


(* TODO *)
(* empty *)
(* merge_map, merge_rcfg *)

let prev_chunk mem ~addr =
  let prev_addr = Addr.pred addr in
  Memory.view ~from:prev_addr mem

let run dis ~accu ~f mem =
  let rec disasm accu cur_mem =
    let elem = match Dis.insn_of_mem dis cur_mem with
      | Ok (m, insn, _) -> m, insn
      | Error _ -> cur_mem, None in
    match prev_chunk mem ~addr:(Memory.min_addr cur_mem) with
    | Ok next -> disasm (f elem accu) next
    | Error _ -> (f elem accu) in
  Memory.view mem ~from:(Memory.max_addr mem) >>| disasm accu

let disasm ?(backend="llvm") ~accu ~f arch mem =
  Dis.with_disasm ~backend (Arch.to_string arch) ~f:(fun d -> run d ~accu ~f mem)

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
    disasm ?backend ~accu ~f arch mem |> ok_exn
end

let memmap_all ?backend arch mem =
  let filter_add elem memmap =
    let (mem, insn) = elem in 
    Option.value_map insn ~default:memmap
      ~f:(Memmap.add memmap mem) in
  With_exn.disasm ?backend ~accu:Memmap.empty ~f:filter_add arch mem

(* component returns the set of jump points and destinations *)
type indirections = (addr * edge) list Addr.Table.t
let all_indirections brancher superset =
  let dests = Addr.Table.create () in
  List.iter superset ~f:(function
      | _,None -> ()
      | mem,Some insn ->
        Brancher.resolve brancher mem insn |> List.iter ~f:(function
            | None,_ -> ()
            | Some addr,kind ->
              Hashtbl.add_multi dests ~key:(Memory.min_addr mem)
                ~data:(addr,kind)));
  dests

type barriers = addr Hash_set.t
let barriers_of_dests dests =
  let barriers = Addr.Hash_set.create () in
  let mark_barrier = Hash_set.add barriers in
  Hashtbl.iteri dests ~f:(fun ~key:src ~data:dsts -> match dsts with
      | _ :: _ :: _ ->
        List.iter dsts ~f:(fun (addr,_) -> mark_barrier addr)
      | _ -> ());
  barriers

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

(* TODO refactor superset_cfg_of_mem to use with_mem *)
let update_with_mem ?backend superset mem =
  printf "superset_cfg_of_mem length %d\n" Memory.(length mem);
  let arch = Image.arch superset.img in
  disasm ?backend ~accu:[] ~f:List.cons arch mem >>|
  fun raw_superset -> (
    let superset_rcfg = superset.insn_rcfg in
    let brancher = superset.brancher in
    let insn_map = superset.insn_map in
    (* raw_superset has an option because it is fresh out of mem, *)
    (* insn_map does not because had been filtered before *)
    let insn_map = raw_superset_to_map ~insn_map raw_superset in
    let insn_rcfg = Superset_rcfg.rcfg_of_raw_superset
        ~superset_rcfg brancher raw_superset in
    { 
      img       = superset.img;
      brancher  = superset.brancher;
      insn_map  = insn_map;
      insn_rcfg = insn_rcfg;
    }
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
  let brancher = Brancher.of_bil @@ Image.arch img in
  let superset = {
    img           = img;
    insn_rcfg     = Superset_rcfg.G.create ();
    insn_map      = Addr.Map.empty;
    brancher      = brancher;
  } in
  with_img ~accu:superset ~backend img
    ~f:(fun ~accu ~backend mem -> 
        update_with_mem ~backend accu mem
      )

let superset_disasm_of_file ~backend binary = 
  let img  = Common.img_of_filename binary in
  superset_of_img ~backend img
