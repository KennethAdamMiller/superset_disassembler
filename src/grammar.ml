open Core_kernel.Std
open Bap.Std
open Superset_rcfg
open Superset

(* TODO could use an alternative branch identification technique:
   1) find every node with two predecessors in the risg
   2) build a map of non-fall through edges, from target to source
   3) determine the predecessor that is a fall through, and visit from
   it downward check the map of these non-fall through edges from
   source to target, and a hash set of visited nodes. While traveling,
   several things may occur: 1) it may not ever encounter a successful
   lookup in the map. 2) it may encounter several successful in the
   map 3) it may encounter the mapping sought originally.
   4) Use union find to determine descendant-ancestor relationship,
   and an integer mapping to lookup sets of addresses that are
   connected
   5) when iterating from a different start point, could use the union
   find and integer mapping to know, when encountering
   other-previsited addresses, what grammar items are connected in
   order to union them together.
*)
let identify_branches superset =
  let open Superset in
  let deferred = ref Addr.Map.empty in
  let insn_rcfg = superset.insn_rcfg in
  let entries = Decision_tree_set.entries_of_cfg insn_rcfg in
  (* need to create a sequence of non-fall through edges *)
  let insns = Addr.Hash_set.create () in
  let branches = Addr.Hash_set.create () in
  let tag_branches addr =
    if Superset_rcfg.G.in_degree insn_rcfg addr = 2 then
      let inbound = Superset_rcfg.G.pred insn_rcfg addr in
      List.iter inbound ~f:(fun child -> 
          (* check for edges between instructions that are not
             fall through, but for which  *)
          if Hash_set.mem insns child then
            let ft = Decision_tree_set.fall_through_of superset addr in
            if not Addr.(ft = child) && 
               not Addr.(addr = child) then
              deferred := Map.add !deferred ft (child, addr)
        );
  in
  let confirm_branches addr = 
    match Map.find !deferred addr with
    | Some (child, branch) -> 
      if Hash_set.mem insns child then
        Hash_set.add branches branch
    | None -> ()      
  in
  let pre addr = 
    Hash_set.add insns addr;
    tag_branches addr
  in
  let post addr = 
    Hash_set.remove insns addr in
  Decision_tree_set.visit 
    ~pre ~post superset entries;
  let pre addr =
    Hash_set.add insns addr;
    confirm_branches addr
  in
  Decision_tree_set.visit 
    ~pre ~post superset entries;
  branches

(* The objective here is to tag grammar structures while traversing *)
(* topologically in such a manner that we can converge the *)
(* probability of recognizing an intended sequence by the *)
(* compiler. After we've hit some recognition threshold, we begin *)
(* traversing forward from some activation point whereby we trim *)
(* occlusive instructions. To recognize grammars, we have several *)
(* means: one, loops are strongly connected components, and if *)
(* sequences must branch at some point only to reify at a common *)
(* point, expressing a path by which they can finally rejoin. *)
(* TODO it's not so good that we have to detect that a function is *)
(* the starting point. It would be better if the visit function *)
(* managed that for us. *)
let tag_by_traversal ?(threshold=10) superset =
  let superset = Trim.trim superset in 
  let superset = Invariants.tag_branch_violations superset in
  let superset = Trim.trim superset in 
  let open Superset in
  let branches = identify_branches superset in
  (* TODO should delete this printf *)
  let entries = Decision_tree_set.entries_of_cfg superset.insn_rcfg in
  printf "detected %d if structures \n" 
    (Hash_set.length branches);
  let cur_total = ref 0 in
  let positives = ref [] in
  let entry = ref None in
  let tp_entries = Addr.Hash_set.create () in
  (* In the case that our current starting point, entry, is none, set *)
  (* it to being the address of the lambda parameter, addr. Then, set *)
  (* the current total number of recognized grammar items to zero, *)
  (* as well as the positives since we're starting over *)
  (* TODO it may be the case that, since some branches are not *)
  (* diamond that re-ordering the traversals to be total over a *)
  (* function may damage the ability of the convergence to operate correctly. *)
  let pre addr = 
    if Option.is_none !entry then (
      entry := Some(addr);
      cur_total := 0;
      positives := [];
    );
    if Hash_set.mem branches addr then (
      cur_total := !cur_total + 1;
      positives := addr :: !positives;
      if !cur_total >= threshold then
        (* TODO could terminate traversal here *)
        let open Option in 
        ignore (List.nth !positives threshold >>| 
                (fun convergent_point ->
                   Hash_set.add tp_entries convergent_point));
    )
  in
  (* TODO, post does not take into account that increments may occur *)
  (* across layers and keep those isolated *)
  let post addr = 
    (match !entry with
     | Some e -> 
       if Addr.(e = addr) then entry := None
     | None -> ());
    if Hash_set.mem branches addr then (
      cur_total := !cur_total - 1;
      match !positives with
      | _ :: remaining -> positives := remaining
      | [] -> ();
    )
  in
  Decision_tree_set.visit 
    ~pre ~post superset entries;
  printf "marked %d convergences\n" (Hash_set.length tp_entries);
  let visited = Addr.Hash_set.create () in
  let insn_rcfg = superset.insn_rcfg in
  (* TODO should move the fragment below into traversal, modularize *)
  (* the operation from mark_bad to allow occlusion space counting *)
  let insn_cfg  = Superset_rcfg.Oper.mirror insn_rcfg in
  Hash_set.iter tp_entries ~f:(fun tp_entry -> 
      if not (Hash_set.mem visited tp_entry) then
        Superset_rcfg.Dfs.prefix_component (fun tp -> 
            let mark_bad addr = 
              if Superset_rcfg.G.mem_vertex insn_rcfg addr then
                Superset.mark_bad superset addr in
            Superset.with_data_of_insn superset tp ~f:mark_bad;
            Hash_set.add visited tp;
          ) insn_cfg tp_entry;
    );
  superset
