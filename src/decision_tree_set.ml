open Core_kernel.Std
open Bap.Std
open Graphlib.Std
open Graph

type 'a decision = 
  | None_keep_all
  | One of 'a
  | Several of 'a list

(** The decision set represents a set of potentially inter-dependent
      decision trees and potential requirements of selection at each
      node. Although a graph is used, the actual structure is acyclic.
      The addr -> terminal map expresses relationships between the
      graph with which it is paired with other members of the
      enclosing decision_tree_set *)

let mergers_of_cfg insn_cfg = 
  Superset_rcfg.G.fold_vertex (fun addr mergers ->
      if Superset_rcfg.G.out_degree insn_cfg addr > 1 then
        Addr.Set.add mergers addr
      else mergers) insn_cfg Addr.Set.empty

let entries_of_cfg insn_cfg = 
  Superset_rcfg.G.fold_vertex (fun addr accu ->
      if Superset_rcfg.G.in_degree insn_cfg addr  = 0 &&
         Superset_rcfg.G.out_degree insn_cfg addr > 0
      then
        (Hash_set.add accu addr; accu)
      else accu)
    insn_cfg (Addr.Hash_set.create ())

let conflicts_of_entries entries insn_map =
  let visited_entries = Addr.Hash_set.create () in
  Hash_set.fold entries ~init:[] ~f:
    (fun conflicted_entries entry -> 
       if not (Hash_set.mem visited_entries entry) then (
         let in_entry_conflicts = 
           Superset_rcfg.conflicts_within_insn_at insn_map entry in
         let conflicts = Addr.Hash_set.create () in
         Hash_set.add visited_entries entry;          
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
         Hash_set.add conflicts entry;
         if (Hash_set.length conflicts) > 1 then (
           conflicts :: conflicted_entries
         ) else conflicted_entries
       ) else conflicted_entries
    )

let tails_of_conflicts conflicts insn_cfg entries = 
  let possible_tails = mergers_of_cfg insn_cfg in
  (* we iterate over the basic blocks, reversing them in order to
     find the tails first because our cfg is in reverse, and also
     because in the case of basic blocks, we always group 
     competitors with a common tail by their entire instruction 
     lineage due to leader list. 
     This tail is the particular instruction
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
            let is_connected = 
              match Superset_rcfg.G.find_all_edges
                      insn_cfg possible_tail poss_conflict with
              | [] -> false | _ -> true in
            if not_added && is_conflict && is_connected then
              poss_conflict :: sheath
            else sheath in
          let sheath = List.fold_left
              (Superset_rcfg.G.succ insn_cfg possible_tail) ~init:[] ~f
          in
          match sheath with
          | [] | _ :: []-> tails, added_choices
          | _ -> 
            let added_choices =
              Set.inter added_choices (Addr.Set.of_list sheath) in
            (Addr.Map.add tails ~key:possible_tail ~data:sheath, added_choices)
        ) possible_tails in 
  tails

let decision_tree_of_entries conflicted_entries entries tails insn_cfg =
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
            Superset_rcfg.G.add_edge decision_tree possible_tail
              competitor;
          );
      | _ -> ()
    else ();
  in
  let link_zero decision_tree entry =
    let width = Addr.bitwidth entry in
    let zero = Addr.(of_int ~width 0) in
    Superset_rcfg.G.add_edge decision_tree zero entry;
  in
  let f decision_tree entry =
    let width = Addr.bitwidth entry in
    let saved_vert = ref @@
      Addr.of_int ~width 0 in
    let link_choices current_vert =
      add_choices decision_tree entry;
      let contained = Superset_rcfg.G.mem_vertex
          decision_tree current_vert in
      let is_new = Hash_set.mem visited current_vert in
      if contained && is_new then (
        if not @@ Superset_rcfg.G.mem_edge decision_tree !saved_vert
            current_vert then (
          Superset_rcfg.G.add_edge decision_tree !saved_vert
            current_vert;
        );
        saved_vert := current_vert;
      );
      Hash_set.add visited current_vert
    in
    (* Would like to have fold_component; not available in this
       version *)
    Superset_rcfg.Dfs.prefix_component link_choices insn_cfg entry;
  in
  let conflicted_trees = 
    List.filter_map conflicted_entries ~f:(fun conflicted ->
        if Hash_set.length conflicted > 0 then
          let decision_tree = Superset_rcfg.G.create () in
          let f entry = 
            link_zero decision_tree entry;
            f decision_tree entry in
          Hash_set.iter conflicted ~f;
          Some(decision_tree)
        else None
      ) in
  Hash_set.fold entries ~init:conflicted_trees 
    ~f:(fun all_trees entry ->
        if not (Hash_set.mem visited entry) then
          let decision_tree = Superset_rcfg.G.create () in
          f decision_tree entry;
          if Superset_rcfg.G.nb_vertex decision_tree > 0 then
            decision_tree :: all_trees
          else all_trees
        else (all_trees)
      )


(** Accepts a per instruction control flow graph, and a map from addr *)
(** to (mem, insn) *)
let decision_trees_of_superset superset = 
  let open Superset in
  let insn_map = Superset.get_data superset in
  let insn_rcfg = superset.insn_rcfg in
  (* Here, for each vertex, look up the insn from the map and *)
  (* identify conflicts. *)
  let conflicts = Superset_rcfg.find_all_conflicts insn_map in
  (* entries variable:
     We want to know the superset of all nodes that could be the
     terminating point that would otherwise be the return instruction
     of a function. *)
  let entries = entries_of_cfg insn_rcfg in  
  (*
     we need to keep track of the subset of potential choices 
     that fall in line with the normal control flow graph, and
     leave the discovery of overlapping redirection to a 
     second pass, in order that when we do a map over all 
     instructions to check for conflicts, we know which are tails
     in order to properly construct the sheath type.
  *)
  let tails = tails_of_conflicts conflicts insn_rcfg entries in
  (* It may be that some entries are accidental indirections that *)
  (* happen to preside at the intended entry. These must map to to an *)
  (* entirely distinct interpretation. *)
  let conflicted_entries = conflicts_of_entries entries insn_map in
  (* For each of the potentially conflicting entries, construct a *)
  (* decision tree. *)
  let decision_trees = decision_tree_of_entries
      conflicted_entries entries tails insn_rcfg in
  decision_trees


let min_spanning_tree superset = 
  let open Superset in
  let edges = Superset_rcfg.Kruskal.spanningtree superset.insn_rcfg in
  let trees = Superset_rcfg.G.create () in
  List.iter edges ~f:(fun edge -> 
      Superset_rcfg.G.add_edge_e trees edge;
    );
  trees

let visit_with_deltas superset decision_tree ~pre =
  let open Superset in
  let insn_map = Superset.get_data superset in
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
  let deltas = ref Addr.Map.empty in
  let delta = ref None in
  let make_deltas decision_tree addr =
    let insns, datas = 
      match !delta with
      | Some (insns, datas) -> (insns, datas) 
      | None ->     
        let insns = Addr.Hash_set.create () in
        let datas = Addr.Hash_set.create () in
        delta := Some(insns, datas);
        insns, datas in
    add_data_of_insn datas addr;
    Hash_set.add insns addr;
    if Superset_rcfg.G.mem_vertex decision_tree addr then (
      deltas := Addr.Map.add !deltas addr (insns, datas);
      delta := None
    )
  in
  let insn_rcfg = superset.insn_rcfg in
  let tree_entries = entries_of_cfg decision_tree in 
  Hash_set.iter tree_entries 
    ~f:(Superset_rcfg.Dfs.postfix_component 
          (make_deltas decision_tree) insn_rcfg);
  let pre addr = 
    pre !deltas addr in
  Hash_set.iter tree_entries 
    ~f:(Superset_rcfg.Dfs.prefix_component pre insn_rcfg)
