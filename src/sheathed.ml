open Core_kernel.Std
open Bap.Std
open Superset_rcfg
open Common
open Graphlib.Std
open Graph

let exits_of_cfg insn_cfg component = 
  Set.fold component ~init:Addr.Set.empty ~f:(fun potential_exits addr -> 
      Superset_rcfg.G.fold_pred (fun ancestor potential_exits ->
          if not Set.(mem component ancestor) then
            Set.add potential_exits ancestor
          else potential_exits
        ) insn_cfg addr potential_exits
    )

let parents_of_insns insn_cfg component = 
  Set.fold component ~init:Addr.Set.empty ~f:(fun potential_parents addr -> 
      Superset_rcfg.G.fold_succ (fun ancestor potential_parents ->
          if not Set.(mem component ancestor) then
            Set.add potential_parents ancestor
          else potential_parents
        ) insn_cfg addr potential_parents
    )

let filter_components ?(min_size=20) insn_cfg components = 
  List.fold_left components ~init:Addr.Set.empty
    ~f:(fun keep  component ->
        let component = Addr.Set.of_list component in
        if Set.length component > min_size then
          Addr.Set.(union keep component)
        else
          keep
      )

let tag_loop_contradictions ?(min_size=20) superset = 
  print_endline "Sheathed.tag";
  let open Superset in
  let insn_rcfg = superset.insn_rcfg in
  let insn_map = superset.insn_map in
  let keep = filter_components insn_rcfg @@ 
    StrongComponents.scc_list insn_rcfg in
  (* Here we have to be careful; we only want to find instructions
     that occur within a loop that produce a self-contradiction *)
  let parents = parents_of_insns insn_rcfg keep in
  let to_remove = Superset_rcfg.conflicts_within_insns insn_map keep in
  let to_remove = Set.inter to_remove parents in
  let to_remove = Set.diff to_remove keep in
  printf "tagged %d contradictions to remove\n" Set.(length to_remove);
  let bad = get_bad superset in
  Set.iter to_remove ~f:(G.add_edge insn_rcfg bad);
  rebuild ~insn_map ~insn_rcfg superset

let default_tags = [tag_loop_contradictions]

let tagged_disasm_of_file ?(backend="llvm") bin =
  let superset = Trim.tagged_disasm_of_file ~backend bin in
  tag_loop_contradictions superset

let trimmed_disasm_of_file ?(backend="llvm") bin =
  let superset = tagged_disasm_of_file ~backend bin in
  Trim.trim superset

let sheaths_of_file ?(backend="llvm") bin = 
  let superset = tagged_disasm_of_file ~backend bin in
  superset, Decision_tree_set.decision_trees_of_superset superset

let trimmed_sheaths_of_file ?(backend="llvm") bin =
  let superset = Trim.trim (tagged_disasm_of_file ~backend bin) in
  superset, Decision_tree_set.decision_trees_of_superset superset

(* TODO test the below functions *)
let iter_decision_set ?(backend="llvm") bin ~f = 
  let superset, decision_trees = trimmed_sheaths_of_file ~backend bin in
  List.iter decision_trees ~f

let fold_decision_set ~init ?(backend="llvm") bin ~f =
  let superset, decision_trees =
    trimmed_sheaths_of_file ~backend bin in
  List.fold decision_trees ~init ~f

