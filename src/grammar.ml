open Core_kernel.Std
open Bap.Std
open Superset_rcfg
open Superset

let fall_through_of superset addr =
  let len = Superset.len_at superset addr in
  Addr.(addr ++ len)

let is_fall_through superset parent child = 
  let ft = fall_through_of superset parent in
  Addr.(child = ft)


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
            let ft = fall_through_of superset addr in
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
let tag_by_traversal superset =
  let superset = Invariants.tag_layer_violations superset in
  let superset = Trim.trim superset in
  let open Superset in
  let branches = identify_branches superset in
  printf "detected %d if structures\n" (Hash_set.length branches);
  let entries = Decision_tree_set.entries_of_cfg superset.insn_rcfg in
  let cur_total = ref 0 in
  let threshold = 50 in
  let marked = ref false in
  let entry = ref None in
  let pre addr = 
    if Option.is_none !entry then (
      entry := Some(addr);
      cur_total := 0;
      marked := false
    )
  in
  let tp_entries = Addr.Hash_set.create () in
  (* TODO, post does not take into account that increments may occur *)
  (* across layers and keep those isolated *)
  let post addr = 
    (match !entry with
     | Some e -> 
       if Addr.(e = addr) then (
         entry := None
       )
     | None -> ());
    if Hash_set.mem branches addr then
      cur_total := !cur_total + 1;
    if !cur_total >= threshold && not (!marked) then (
      marked := true;
      Hash_set.add tp_entries addr;
    );
  in
  Decision_tree_set.visit 
    ~pre ~post superset entries;
  printf "marked %d convergences\n" (Hash_set.length tp_entries);
  let visited = Addr.Hash_set.create () in
  let insn_rcfg = superset.insn_rcfg in
  let insn_cfg  = Superset_rcfg.Oper.mirror insn_rcfg in
  Hash_set.iter tp_entries ~f:(fun tp_entry -> 
      if not (Hash_set.mem visited tp_entry) then
        Superset_rcfg.Dfs.prefix_component (fun tp -> 
            Superset.with_data_of_insn superset tp 
              ~f:(Superset.mark_bad superset);
            Hash_set.add visited tp;
          ) insn_cfg tp_entry;
    );
  superset
