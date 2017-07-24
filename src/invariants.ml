open Bap.Std
open Core_kernel.Std

let enforce_exclusivity insn_delta data_delta =
  let insns_in_data = 
    Hash_set.fold ~init:[] data_delta ~f:(fun violators data -> 
        if Hash_set.mem insn_delta data then
          data :: violators
        else violators
      ) in
  let datas_in_insn = 
    Hash_set.fold ~init:[] insn_delta ~f:(fun violators insn -> 
        if Hash_set.mem data_delta insn then
          insn :: violators
        else violators
      ) in
  (insns_in_data, datas_in_insn)

let mark_nonexclusive superset insn_delta data_delta ~mark =
  let (data_violators, insn_violators) =
    enforce_exclusivity insn_delta data_delta in
  List.iter data_violators ~f:mark;
  List.iter insn_violators ~f:mark

(* Unfortunately, I can't build this with functional programming in *)
(* mind, because the ocamlgraph function (fold) required to do so is *)
(* missing from the DFS module. *) 
let tag_layer_violations superset = 
  let open Superset in
  let insn_rcfg = superset.insn_rcfg in
  let insn_map = Superset.get_data superset in
  (* TODO below, this should be migrated to superset or superset_rcfg *)
  let with_data_of_insn at ~f =
    let len = match Map.find insn_map at with
      | Some(mem, _) -> Memory.length mem 
      | None -> 0 in
    let opt_data_addrs = Superset_rcfg.seq_of_addr_range 
        at len in
    let () = Seq.iter
        ~f opt_data_addrs in () in
  let add_data_of_insn dataset at = 
    with_data_of_insn at ~f:(Hash_set.add dataset)
  in
  let remove_data_of_insn dataset at =
    with_data_of_insn at ~f:(Hash_set.remove dataset)
  in
  (* TODO above, this should be migrated to superset or
     superset_rcfg *)
  let conflicts = Superset_rcfg.find_all_conflicts insn_map in
  let entries = Decision_tree_set.entries_of_cfg insn_rcfg in
  let tails = Decision_tree_set.tails_of_conflicts
      conflicts insn_rcfg entries in
  let options = Map.fold tails ~init:Addr.Set.empty ~f:
      (fun ~key ~data options -> 
         List.fold ~init:options data ~f:Set.add) in
  let is_option addr = 
    Set.mem options addr in
  let insns = Addr.Hash_set.create () in
  let datas = Addr.Hash_set.create () in
  let pre deltas addr = 
    add_data_of_insn datas addr;
    Hash_set.add insns addr  in  
  let post deltas addr =
    Hash_set.remove insns addr;
    remove_data_of_insn datas addr in
  let tag_violators deltas addr = 
    if Hash_set.mem datas addr then
      Superset.mark_bad superset addr;
    match Map.find deltas addr with
    | Some (insn_delta, data_delta) -> 
      Hash_set.iter insn_delta ~f:(fun insn -> 
          let inbound = Superset_rcfg.G.pred insn_rcfg insn in
          List.iter inbound ~f:(fun src -> 
              if Hash_set.mem data_delta src then (
                Superset.mark_bad superset insn;
              ) else if Hash_set.mem datas src then (
                Superset.mark_bad superset insn;
              )
            );
        );
      mark_nonexclusive superset insn_delta data_delta
        ~mark:(Superset.mark_bad superset)
    | None -> ();
  in
  let post deltas addr = 
    tag_violators deltas addr;
    post deltas addr in
  Decision_tree_set.visit_with_deltas 
    ~is_option ~pre ~post superset entries
