open Core_kernel.Std
open Bap.Std
open Superset_rcfg
open Superset

let is_branch superset addr = 
  let insn_rcfg = superset.insn_rcfg in
  Superset_rcfg.G.in_degree insn_rcfg addr > 1

(*let is_join superset addr =
  let insn_rcfg = superset.insn_rcfg in
  let parents = Superset_rcfg.G.succ insn_rcfg addr in
  let branch_parent = List.fold_left parents ~init:false
      ~f:(fun status pbranch -> status || (is_branch superset pbranch)) in
  if branch_parent then
    let has_route = 
      has_route && Superset_rcfg.G.out_degree insn_rcfg addr > 1
  else false *)

let tag_loop_exits superset = 
  let components = StrongComponents.scc_list superset.insn_rcfg in
  let components = Sheathed.filter_components components in
  Set.fold ~init:Addr.Set.empty components ~f:(fun loop_tags addr -> 
      let inbound = G.pred superset.insn_rcfg addr in
      List.fold ~init:loop_tags inbound 
        ~f:(fun loop_tags x -> 
            if not Set.(mem components x) then 
              Set.add loop_tags x
            else loop_tags
          ))

let tag_loop_entries superset = 
  let components = StrongComponents.scc_list superset.insn_rcfg in
  List.fold_left ~init:Addr.Set.empty components 
    ~f:(fun tops component -> 
        if List.(length component) >= 20 then
          match component with
          | addr :: tail -> 
            let top = 
              List.fold_left ~init:addr tail ~f:Addr.min in
            Set.add tops top
          | _ -> tops
        else tops
      )

(* The objective here is to tag grammar structures while traversing *)
(* topologically in such a manner that we can converge the *)
(* probability of recognizing an intended sequence by the *)
(* compiler. After we've hit some recognition threshold, we begin *)
(* traversing forward from some activation point whereby we trim *)
(* occlusive instructions. To recognize grammars, we have several *)
(* means: one, loops are strongly connected components, and if *)
(* sequences must branch at some point only to reify at a common *)
(* point, expressing a path by which they can finally rejoin. *)
let tag superset = 
  let loop_exits = tag_loop_exits superset in
  let loop_entries = tag_loop_entries superset in
  let insn_map = Superset.get_data superset in
  let num_components, discrete_components =
    DiscreteComponents.components superset.insn_rcfg in
  printf "Grammar.trim num_components: %d...\n" num_components;
  let component_of_addr addr component_ordering = 
    let component_num = discrete_components addr in
    let component_order = Map.find component_ordering component_num in
    Option.value component_order ~default:[] in
  let replace_component addr component_ordering =
    let component_num = discrete_components addr in
    let component_order = component_of_addr addr component_ordering in
    let component_order = addr :: component_order in
    Map.add component_ordering component_num component_order in
  let do_group addr ordering = 
    if is_branch superset addr then
      replace_component addr ordering
    else if Set.mem loop_entries addr then
      replace_component addr ordering
    else 
      ordering in
  let cfg = Oper.mirror superset.insn_rcfg in
  let total = ref 0 in
  let component_order = Topological.fold do_group
      superset.insn_rcfg Int.Map.empty in
  let bad = get_bad superset in
  let dfs_mark addr = 
    let mark_competitors addr = 
      let competitors = conflicts_within_insn_at
          insn_map addr in
      total := !total + Set.(length competitors);
      Set.iter competitors (G.add_edge superset.insn_rcfg bad) in
    Dfs.prefix_component mark_competitors cfg addr in
  let threshold = 5 in
  (* We don't have to reverse the list because it's backwards from rcfg *)
  List.iter (Map.data component_order) ~f:(fun component -> 
      (* We only have to grab the threshold node, since all later are *)
      (* in the same component and successors *)
      match List.nth component threshold with
      | Some(addr) -> dfs_mark addr
      | None -> ()
    );
  printf "Total added by grammar: %d\n" !total;
  superset


let tag_by_traversal superset min_tree =
  let deferred = Addr.Hash_set.create () in
  let insn_rcfg = superset.insn_rcfg in
  let insn_map = Superset.get_data superset in
  let conflicts = Superset_rcfg.find_all_conflicts insn_map in
  let entries = Decision_tree_set.entries_of_cfg insn_rcfg in
  let tails = Decision_tree_set.tails_of_conflicts
      conflicts insn_rcfg entries in
  let options = Map.fold tails ~init:Addr.Set.empty ~f:
      (fun ~key ~data options -> 
         List.fold ~init:options data ~f:Set.add) in
  let is_option addr = 
    Set.mem options addr in
  (* need to create a sequence of non-fall through edges *)
  let insns = Addr.Hash_set.create () in
  let pre deltas addr = 
    Hash_set.add insns addr  in  
  let post deltas addr =
    Hash_set.remove insns addr in
  let tag_violators deltas addr = 
    match Map.find deltas addr with
    | Some (insn_delta, data_delta) -> 
      (* look for edges between insns that  *)
      Hash_set.iter insn_delta ~f:(fun insn -> 
          let inbound = Superset_rcfg.G.pred insn_rcfg insn in
          List.iter inbound ~f:(fun src -> 
              (* are there edges that are in insns, 
                 but not in the insn_delta? *)
              ()
            );
        );
    | None -> ();
  in
  let post deltas addr = 
    tag_violators deltas addr;
    post deltas addr in
  Decision_tree_set.visit_with_deltas 
    ~is_option ~pre ~post superset min_tree

let trimmed_disasm_of_file ?(backend="llvm") bin =
  let superset, trees = Sheathed.sheaths_of_file ~backend bin in
  Trim.trim (tag superset), trees
