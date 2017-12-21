open Core_kernel.Std
open Bap.Std
open Superset_risg
open Common
open Graphlib.Std
open Graph


let parents_of_insns insn_isg component = 
  Set.fold component ~init:Addr.Set.empty ~f:(fun potential_parents addr -> 
      Superset_risg.G.fold_succ (fun ancestor potential_parents ->
          if not Set.(mem component ancestor) then
            Set.add potential_parents ancestor
          else potential_parents
        ) insn_isg addr potential_parents
    )

let filter_components ?(min_size=20) components = 
  List.fold_left components ~init:Addr.Set.empty
    ~f:(fun keep  component ->
        let component = Addr.Set.of_list component in
        if Set.length component > min_size then
          Addr.Set.(union keep component)
        else
          keep
      )

let tag_loop_contradictions ?(min_size=20) superset = 
  let insn_risg = Superset.get_graph superset in
  let insn_map = Superset.get_data superset in
  let keep = filter_components ~min_size @@ 
    StrongComponents.scc_list insn_risg in
  (* Here we have to be careful; we only want to find instructions
     that occur within a loop that produce a self-contradiction *)
  let parents = parents_of_insns insn_risg keep in
  let to_remove = 
    Superset_risg.conflicts_within_insns insn_map keep in
  let to_remove = Set.inter to_remove parents in
  let to_remove = Set.diff to_remove keep in
  printf "tagged %d contradictions of %d parents of %d to keep\n" 
    Set.(length to_remove)
    Set.(length parents)
    Set.(length keep);
  let bad = Superset.get_bad superset in
  Set.iter to_remove ~f:(G.add_edge insn_risg bad);
  Superset.rebuild ~data:insn_map ~insn_risg superset

let default_tags = [tag_loop_contradictions]

let tagged_disasm_of_file ?(backend="llvm") bin =
  let data = Addr.Map.empty in
  let superset = Trim.tagged_disasm_of_file 
      ~f:[Trim.add_to_map] ~data ~backend bin in
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

