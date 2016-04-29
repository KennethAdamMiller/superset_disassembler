open Core_kernel.Std
open Regular.Std
open Bap.Std
open Or_error.Monad_infix
open Graph
module Dis = Disasm_expert.Basic
module Cfg = Graphs.Cfg

module G = Persistent.Digraph.ConcreteBidirectional(struct 
    type t = Addr.t 
    let compare = Addr.compare
    let hash = Addr.hash
    let equal = Addr.equal
  end)

module Dfs = Traverse.Dfs(G)

let prev_chunk mem ~addr =
  let prev_addr = Addr.pred addr in
  Memory.view ~from:prev_addr mem

let targ_in_mem gmem_min gmem_max addr =
  Addr.(addr >= gmem_min && addr <= gmem_max)

let is_exec_ok gmem_min gmem_max lmem  =
  Addr.(Memory.min_addr lmem >= gmem_min) &&
  Addr.(Memory.max_addr lmem <= gmem_max)

let static_successors brancher ~min ~max mem insn =
  if is_exec_ok min max mem
  then Brancher.resolve brancher mem insn
  else [None, `Fall]

let bad_of_arch arch = 
  G.V.create (Addr.of_int
                ~width:(Size.in_bits @@ Arch.addr_size arch) 0)

let insert_all ?superset_cfg ?brancher shingles gmem arch =
  (* depth first search for all that bad points to *)
  let brancher = Option.value brancher ~default:(Brancher.of_bil arch)
  in
  let superset_cfg = Option.value superset_cfg ~default:G.empty in
  let min,max = Memory.(min_addr gmem, max_addr gmem) in
  let target_in_mem = targ_in_mem min max in
  (* TODO touches non-mem *)
  let bad = bad_of_arch arch in
  let accesses_non_mem insn = false in
  let get_targets = static_successors brancher ~min ~max in
  let is_non_code addr insn = 
    not (target_in_mem addr) ||
    accesses_non_mem insn in
  let add_shingles superset_cfg shingles =
    List.fold shingles ~init:superset_cfg ~f:(fun superset_cfg (mem, insn) ->
        let src = Memory.min_addr mem in
        match insn with
        | Some(insn) ->
          let targets = get_targets mem insn in
          List.fold targets ~init:superset_cfg
            ~f:(fun superset_cfg (target,_) ->
                match target with 
                | Some(target) -> 
                  if is_non_code target insn then
                    G.add_edge superset_cfg bad src
                  else 
                    G.add_edge superset_cfg target src
                | _ -> superset_cfg
              )
        | None -> 
          G.add_edge superset_cfg bad src
      )
  in bad, add_shingles superset_cfg shingles

let sheer superset_cfg arch =
  let bad = bad_of_arch arch in
  let to_drop = Addr.Hash_set.create () in
  Dfs.prefix_component (Hash_set.add to_drop) superset_cfg bad;
  let sheered_cfg = Hash_set.fold to_drop ~init:superset_cfg
      ~f:(fun superset_cfg data_point
           -> G.remove_vertex superset_cfg data_point) in
  G.remove_vertex sheered_cfg bad

(* component returns the set of jump points and destinations *)
let dests_of_shingles brancher shingles =
  let dests = Addr.Table.create () in
  Seq.iter shingles ~f:(function
      | _,None -> ()
      | mem,Some insn ->
        Brancher.resolve brancher mem insn |> List.iter ~f:(function
            | None,_ -> ()
            | Some addr,kind ->
              Hashtbl.add_multi dests ~key:(Memory.min_addr mem)
                ~data:(addr,kind)));
  dests

let find_barriers dests =
  let barriers = Addr.Hash_set.create () in
  let mark_barrier = Hash_set.add barriers in
  Hashtbl.iteri dests ~f:(fun ~key:src ~data:dsts -> match dsts with
      | _ :: _ :: _ ->
        List.iter dsts ~f:(fun (addr,_) -> mark_barrier addr)
      | _ -> ());
  barriers

let lift lift (mem,insn) =
  match insn with
  | None -> None
  | Some insn ->
    let bil = match lift mem insn with
      | Ok bil -> Some bil
      | Error _ -> None in
    Some (mem,Insn.of_basic ?bil insn)

(*let run ?superset_cfg ?brancher shingles arch mem =
  let superset_cfg = Option.value superset_cfg ~default:G.empty in
  let module Target = (val target_of_arch arch) in
  let lifter = Target.lift in
  sheer superset_cfg mem arch
  let dests  = dests_of_shingles brancher shingles in
    let nodes = Addr.Table.create () in
    let barriers = find_barriers dests in
    let is_barrier mem = Hash_set.mem barriers (Memory.min_addr mem) in
    let insert_node rev_insns cfg =
    let blk = Block.create mem (List.rev rev_insns) in
    Hashtbl.set nodes ~key:(Block.addr blk) ~data:blk;
    Cfg.Node.insert blk cfg in
    let node = Hashtbl.find nodes in
    let cfg,rest =
    Seq.filter_map shingles ~f:(lift lifter) |>
    Seq.fold ~init:(superset_cfg,[]) ~f:(fun (cfg,insns) (mem,insn) ->
        if is_barrier mem then match insns with
          | [] -> cfg, [mem, insn]
          | insns -> insert_node ((mem,insn) :: insns) cfg, []
        else cfg, (mem,insn) :: insns) in
    let cfg = match rest with
    | [] -> cfg
    | insns -> insert_node insns cfg in
    Hashtbl.fold dests ~init:cfg ~f:(fun ~key:addr ~data:dests cfg ->
      match node addr with
      | None -> cfg
      | Some src ->
        List.fold dests ~init:cfg ~f:(fun cfg (addr,kind) ->
            match node addr with
            | None -> cfg
            | Some dst ->
              let e = Cfg.Edge.create src dst kind in
              Cfg.Edge.insert e cfg))
*)

module Conservative = struct
  open Disasm_expert.Basic

  let run dis mem =
    let rec disasm accu cur_mem =
      let elem = match Dis.insn_of_mem dis cur_mem with
        | Ok (m, insn, _) -> m, insn
        | Error _ -> cur_mem, None in
      match prev_chunk mem ~addr:(Memory.min_addr cur_mem) with
      | Ok next -> disasm (elem :: accu) next
      | Error _ -> elem :: accu in
    match Memory.view mem ~from:(Memory.max_addr mem) with
    | Ok cur_mem -> Ok (disasm [] cur_mem)
    | Error err -> Error err

  let disasm ?(backend="llvm") arch mem =
    with_disasm ~backend (Arch.to_string arch) ~f:(fun d -> run d mem)

  module With_exn = struct
    let disasm ?backend arch mem = disasm ?backend arch mem |> ok_exn
  end
end

let superset_of ?superset_cfg ?brancher ?backend arch mem =
  Conservative.disasm ?backend arch mem >>| fun insns ->
  insert_all ?superset_cfg ?brancher insns mem arch

let disasm ?superset_cfg ?brancher ?backend arch mem =
  superset_of ?superset_cfg ?brancher ?backend arch mem >>|
  fun  (bad,superset_cfg) -> sheer superset_cfg arch

module With_exn = struct
  let disasm ?brancher ?backend arch mem =
    disasm ?brancher ?backend arch mem |> ok_exn
end
