open Bap.Std
open Core_kernel.Std

(* Unfortunately, I can't build this with functional programming in *)
(* mind, because the ocamlgraph function (fold) required to do so is *)
(* missing from the DFS module. *)
let tag_cross_layer_jmps superset decision_trees =
  let open Superset in
  let insn_map = Superset.get_data superset in
  let insns = Addr.Hash_set.create () in
  let datas = Addr.Hash_set.create () in
  List.iter (Map.keys insn_map) ~f:(Hash_set.add insns);
  Seq.iter (Superset_rcfg.range_seq insn_map) ~f:(Hash_set.add datas);
  let module G = Superset_rcfg.G in
  let tag_violators decision_tree deltas addr = 
    match Map.find deltas addr with
    | Some(insn_delta, data_delta) -> 
      Hash_set.iter data_delta ~f:(fun data -> 
          if Map.mem deltas data then (
            Superset.mark_bad superset addr
          )
        )
    | None -> ()
  in
  List.iter decision_trees ~f:(fun decision_tree -> 
      let pre = 
        tag_violators decision_tree in
      Decision_tree_set.visit_with_deltas 
        superset decision_tree ~pre
    )

let tag_min_tree_violations superset min_tree = 
  let tag_violators deltas addr = ()
  in
  Decision_tree_set.visit_with_deltas superset min_tree 
    ~pre:tag_violators 
