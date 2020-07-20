open Core_kernel
open Bap.Std

let identify_branches superset =
  let deferred = ref Addr.Map.empty in
  let entries = Superset.entries_of_isg superset in
  (* need to create a sequence of non-fall through edges *)
  let insns = Addr.Hash_set.create () in
  let branches = Addr.Hash_set.create () in
  let tag_branches addr =
    if Superset.is_branch superset addr then
      let inbound = Superset.ISG.descendants superset addr in
      List.iter inbound ~f:(fun child -> 
          (* check for edges between instructions that are not
             fall through, but for which  *)
          if Hash_set.mem insns child then
            let ft = Superset.fall_through_of superset addr in
            if not Addr.(ft = child) && 
               not Addr.(addr = child) then
              deferred := Map.set !deferred ft (child, addr)
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
  Traverse.visit 
    ~pre ~post superset entries;
  let pre addr =
    Hash_set.add insns addr;
    confirm_branches addr
  in
  Traverse.visit 
    ~pre ~post superset entries;
  branches

let increment_map_at m ?(x=1) addr =
  m := Map.update !m addr
      ~f:(fun hits -> Option.value_map hits ~default:1
             ~f:(fun hits -> hits +x));
  Option.value ~default:x Map.(find !m addr)

let linear_branch_sweep superset entries =
  let jmp_hit_cnt = ref Addr.Map.empty in
  let update_hit_count = increment_map_at jmp_hit_cnt in
  let pre jmps targets addr =
    if Set.mem targets addr then (
      ignore (update_hit_count addr);
    );
    match Map.find jmps addr  with
    | Some(branch) ->
      ignore (update_hit_count branch);
    | None -> ();
  in
  let post _ _ _ = () in
  let superset = Traverse.visit_by_block superset ~pre ~post entries in
  let final_jmps = Addr.Hash_set.create () in
  Map.iteri !jmp_hit_cnt ~f:(fun ~key ~data  -> 
      let jmp_addr = key in
      let cnt = data in
      if cnt = 2 then
        Hash_set.add final_jmps jmp_addr;
    );
  final_jmps

(* TODO threshold needs to be acquired, and this function belongs in
 * a different module, and descendent traversal needs to be
 * refactored *)
let tag_callsites visited ?callsites superset =
  let callsites = Option.value callsites 
      ~default:(Superset.get_callsites ~threshold:6 superset) in
  Hash_set.iter callsites ~f:(fun callsite ->
      Superset.with_descendents_at ~visited
        ?post:None ?pre:None superset callsite;
    );
  superset
  
(* The objective here is to tag grammar structures while traversing *)
(* topologically in such a manner that we can converge the *)
(* probability of recognizing an intended sequence by the *)
(* compiler. After we've hit some recognition threshold, we begin *)
(* traversing forward from some activation point whereby we trim *)
(* occlusive instructions. To recognize grammars, we have several *)
(* means: one, loops are strongly connected components, and if *)
(* sequences must branch at some point only to reify at a common *)
(* point, expressing a path by which they can finally rejoin. *)
let tag_by_traversal ?(threshold=8) superset =
  let visited = Addr.Hash_set.create () in
  let callsites = Superset.get_callsites ~threshold:6 superset in
  let superset = tag_callsites visited ~callsites superset in
  let superset = Invariants.tag_layer_violations superset in
  let superset = Invariants.tag_branch_violations superset in
  let entries = Superset.entries_of_isg superset in
  let branches = Superset.get_branches superset in
  (*let branches = identify_branches superset in*)
  (*let branches = linear_branch_sweep superset entries in*)
  let cur_total = ref 0 in
  let positives = ref [] in
  let entry = ref None in
  let tps = Addr.Hash_set.create () in
  (* In the case that our current starting point, entry, is none, set *)
  (* it to being the address of the lambda parameter, addr. Then, set *)
  (* the current total number of recognized grammar items to zero, *)
  (* as well as the positives since we're starting over *)
  let pre addr = 
    if Option.is_none !entry then (
      entry := Some(addr);
      cur_total := 0;
      positives := [];
    );
    if Hash_set.mem branches addr || Hash_set.mem callsites addr then (
      cur_total := !cur_total + 1;
      positives := addr :: !positives;
      if !cur_total >= threshold then (
        let open Option in 
        ignore (List.nth !positives threshold >>| 
                (fun convergent_point ->
                   Hash_set.add tps convergent_point));
      ) 
    ) in
  let post addr =
    entry := Option.value_map !entry ~default:!entry
        ~f:(fun e -> if e = addr then None else Some(e));
    if Hash_set.mem branches addr || Hash_set.mem callsites addr then (
      cur_total := !cur_total - 1;
      match !positives with
      | _ :: remaining -> positives := remaining
      | [] -> ();
    ) in
  Traverse.visit
    ~pre ~post superset entries;
  let visited = Addr.Hash_set.create () in
  Hash_set.iter tps ~f:(fun tp -> 
      if not (Hash_set.mem visited tp) then (
        Superset.with_descendents_at superset tp ~pre:(fun tp -> 
            let mark_bad addr =
              if Superset.ISG.mem_vertex superset addr then (
                Superset.Core.mark_bad superset addr
              ) in
            Superset.Occlusion.with_data_of_insn superset tp ~f:mark_bad;
            Hash_set.add visited tp;
          ) ;
      )
    );
  Hash_set.iter visited 
    ~f:(fun tp -> Superset.Core.clear_bad superset tp);
  superset
