open Core_kernel.Std
open Bap.Std
open Superset_rcfg
open Common
open Graphlib.Std
open Graph

let parents_of_insns insn_cfg component = 
  Set.fold component ~init:Addr.Set.empty ~f:(fun potential_exits addr -> 
      Superset_rcfg.G.fold_succ (fun ancestor potential_exits ->
          if not Set.(mem component ancestor) then
            Set.add potential_exits ancestor
          else potential_exits
        ) insn_cfg addr potential_exits
    )

let filter_components insn_cfg components = 
  List.fold_left components ~init:Addr.Set.empty
    ~f:(fun keep  component ->
        let component = Addr.Set.of_list component in
        if Set.length component > 20 then
          Addr.Set.(union keep component)
        else
          keep
      )

let tag_loop_contradictions superset = 
  print_endline "Sheathed.trim";
  let open Superset in
  let cfg = superset.insn_rcfg in
  let insn_map = superset.insn_map in
  let arch = Image.arch superset.img in
  let keep = filter_components cfg @@ StrongComponents.scc_list cfg in
  (* Here we have to be careful; we only want to find instructions
     that occur within a loop that produce a self-contradiction *)
  let parents = parents_of_insns cfg keep in
  let to_remove = Superset_rcfg.conflicts_within_insns insn_map keep in
  let to_remove = Set.inter to_remove parents in
  let to_remove = Set.diff to_remove keep in
  printf "to_remove size: %d\n" Set.(length to_remove);
  let bad = Trim.bad_of_arch arch in
  Set.iter to_remove ~f:(G.add_edge cfg bad);
  { 
    insn_map = insn_map;
    insn_rcfg = cfg;
    brancher = superset.brancher;
    img = superset.img;
  }

let default_tags = [tag_loop_contradictions]

let tagged_disasm_of_file ?(backend="llvm") bin =
  let superset = Trim.tagged_disasm_of_file ~backend bin in
  tag_loop_contradictions superset

let trimmed_disasm_of_file ?(backend="llvm") bin =
  let superset = tagged_disasm_of_file ~backend bin in
  Trim.trim superset

let sheaths_of_file ?(backend="llvm") bin = 
  let superset = tagged_disasm_of_file ~backend bin in
  superset, Sheath_tree_set.decision_trees_of_superset superset

let trimmed_sheaths_of_file ?(backend="llvm") bin =
  let superset = Trim.trim (tagged_disasm_of_file ~backend bin) in
  superset, Sheath_tree_set.decision_trees_of_superset superset

(* TODO test the below functions *)
let iter_decision_set ?(backend="llvm") bin ~f = 
  let superset, decision_trees = trimmed_sheaths_of_file ~backend bin in
  List.iter decision_trees ~f

let fold_decision_set ~init ?(backend="llvm") bin ~f =
  let superset, decision_trees =
    trimmed_sheaths_of_file ~backend bin in
  List.fold decision_trees ~init ~f

