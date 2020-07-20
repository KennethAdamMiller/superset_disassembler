open Core_kernel
open Bap.Std
open Graphlib.Std
open Graph

module G = Superset.ISG.G
   
(** The decision set represents a set of potentially inter-dependent
      decision trees and potential requirements of selection at each
      node. Although a graph is used, the actual structure is acyclic.
      The addr -> terminal map expresses relationships between the
      graph with which it is paired with other members of the
      enclosing decision_tree_set *)

let conflicts_of_entries superset entries =
  let visited_entries = Addr.Hash_set.create () in
  Hash_set.fold entries ~init:[] ~f:
    (fun conflicted_entries entry -> 
       if not (Hash_set.mem visited_entries entry) then (
         Hash_set.add visited_entries entry;
         let in_entry_conflicts = 
           Superset.Occlusion.conflicts_within_insn_at superset entry in
         let conflicts = Addr.Hash_set.create () in
         Hash_set.add conflicts entry;
         Set.iter in_entry_conflicts 
           ~f:(fun conflict ->
               (* A conflict that an entry may have may or may not *)
               (* itself be an entry. *)
               if Hash_set.mem entries conflict then (
                 Hash_set.add visited_entries conflict;
                 Hash_set.add conflicts conflict;
               )
             );
         if (Hash_set.length conflicts) > 1 then (
           conflicts :: conflicted_entries
         ) else conflicted_entries
       ) else conflicted_entries
    )

let tails_of_conflicts superset conflicts =
  let possible_tails = Superset.mergers superset in
  (* This tail is the particular instruction
     that is the fall through target of several potential
     competitors. We use this instruction against the
     leaders map because those will be the ones that fall
     through to the tail; the tail can then be associated with
     those that lead into it. *)
  let tails, _ = Set.fold ~init:(Addr.Map.empty, Addr.Set.empty)
      ~f:(fun (tails, added_choices) possible_tail -> 
          (* For each edge from tail, lookup the respective vertex; if it *)
          (* is in the conflicts set, then it gets added to a sheath *)
          (* of choices. *)
          let f sheath poss_conflict =
            let not_added = not (Set.mem added_choices poss_conflict) in
            let is_conflict = Set.mem conflicts poss_conflict in
            let is_connected = Superset.check_connected
                superset possible_tail poss_conflict in
            if not_added && is_conflict && is_connected then
              poss_conflict :: sheath
            else sheath in
          let sheath = List.fold_left
              (Superset.ISG.ancestors superset possible_tail) ~init:[] ~f
          in
          match sheath with
          | [] | _ :: []-> tails, added_choices
          | _ -> 
            let added_choices =
              Set.inter added_choices (Addr.Set.of_list sheath) in
            (Addr.Map.set tails ~key:possible_tail ~data:sheath, added_choices)
        ) possible_tails in 
  tails

let decision_tree_of_entries superset conflicted_entries entries tails =
  let visited = Addr.Hash_set.create () in
  let visited_choices = Addr.Hash_set.create () in
  let add_choices decision_tree current_vert = 
    let unvisited =
      not (Hash_set.mem visited_choices current_vert) in
    if unvisited then
      let possible_tail = current_vert in
      match Addr.Map.find tails possible_tail with
      | Some(sheath) ->
        List.iter sheath ~f:(fun competitor ->
            Hash_set.add visited_choices competitor;
            G.add_edge decision_tree possible_tail
              competitor;
          );
      | _ -> ()
    else ();
  in
  let link_zero decision_tree entry =
    let width = Addr.bitwidth entry in
    let zero = Addr.(of_int ~width 0) in
    G.add_edge decision_tree zero entry
  in
  let f decision_tree entry =
    let width = Addr.bitwidth entry in
    let saved_vert = ref @@
      Addr.of_int ~width 0 in
    let link_choices current_vert =
      add_choices decision_tree entry;
      let contained = G.mem_vertex
          decision_tree current_vert in
      let is_new = Hash_set.mem visited current_vert in
      if contained && is_new then (
        if not @@ G.mem_edge decision_tree !saved_vert
            current_vert then (
          G.add_edge decision_tree !saved_vert
            current_vert;
        );
        saved_vert := current_vert;
      );
      Hash_set.add visited current_vert
    in
    (* Would like to have fold_component; not available in this
       version *)
    Superset.with_ancestors_at superset entry ?post:None ~f:link_choices;
    (*Dfs.prefix_component link_choices insn_isg entry;*)
  in
  let conflicted_trees = 
    List.filter_map conflicted_entries ~f:(fun conflicted ->
        if Hash_set.length conflicted > 0 then
          let decision_tree = G.create () in
          let f entry = 
            if not (Hash_set.mem visited entry) then (
              link_zero decision_tree entry;
              f decision_tree entry) in
          Hash_set.iter conflicted ~f;
          Some(decision_tree)
        else None
      ) in
  Hash_set.fold entries ~init:conflicted_trees 
    ~f:(fun all_trees entry ->
        if not (Hash_set.mem visited entry) then
          let decision_tree = G.create () in
          f decision_tree entry;
          if G.nb_vertex decision_tree > 0 then
            decision_tree :: all_trees
          else all_trees
        else (all_trees)
      )


(** Accepts a per instruction control flow graph, and a map from addr *)
(** to (mem, insn) *)
let decision_trees_of_superset superset = 
  (* Here, for each vertex, look up the insn from the map and *)
  (* identify conflicts. *)
  let conflicts = Superset.Occlusion.find_all_conflicts superset in
  (* entries variable:
     We want to know the superset of all nodes that could be the
     terminating point that would otherwise be the return instruction
     of a function. *)
  let entries = Superset.entries_of_isg superset in  
  (*
     we need to keep track of the subset of potential choices 
     that fall in line with the normal control flow graph, and
     leave the discovery of overlapping redirection to a 
     second pass, in order that when we do a map over all 
     instructions to check for conflicts, we know which are tails
     in order to properly construct the sheath type.
  *)
  let tails = tails_of_conflicts superset conflicts in
  (* It may be that some entries are accidental indirections that *)
  (* happen to preside at the intended entry. These must map to to an *)
  (* entirely distinct interpretation. *)
  let conflicted_entries = conflicts_of_entries superset entries in
  (* For each of the potentially conflicting entries, construct a *)
  (* decision tree. *)
  let decision_trees = decision_tree_of_entries
      superset conflicted_entries entries tails in
  decision_trees

let insn_is_option superset addr = 
  let open Superset in
  let len = Superset.Inspection.len_at superset addr in
  let bound = Addr.(addr ++ len) in
  let previous = Superset.ISG.descendants superset addr in
  List.fold ~init:false previous ~f:(fun current descedant -> 
      if not current then
        let further = Superset.ISG.ancestors superset descedant in
        List.fold ~init:current further ~f:(fun current opt -> 
            if not current then
              if Addr.(addr <= opt) && Addr.(opt < bound) then
                true
              else false
            else current
          )
      else current
    )

let calculate_deltas superset ?entries is_option = 
  let entries = Option.value entries 
      ~default:(Superset.entries_of_isg superset) in
  let add_data_of_insn dataset at = 
    Superset.Occlusion.with_data_of_insn
      superset at ~f:(Hash_set.add dataset)
  in
  let deltas = ref Addr.Map.empty in
  let delta = ref None in
  let make_deltas addr =
    let insns, datas = 
      match !delta with
      | Some (insns, datas) -> (insns, datas) 
      | None ->     
        let insns = Addr.Hash_set.create () in
        let datas = Addr.Hash_set.create () in
        delta := Some(insns, datas);
        insns, datas in
    if is_option addr then (
      deltas := Addr.Map.set !deltas addr (insns, datas);
      delta := None
    ) else (
      add_data_of_insn datas addr;
      Hash_set.add insns addr;
    )
    (* else if is in entries then store the delta in the deltas map *)
  in
  let visited = Addr.Hash_set.create () in
  Hash_set.iter entries 
    ~f:(Superset.with_ancestors_at
          ~visited ~post:make_deltas ?f:None superset);
  !deltas
