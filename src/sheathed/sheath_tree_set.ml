open Core_kernel.Std
open Bap.Std
open Graphlib.Std
open Sheath
open Insn_cfg
open Graph

(* Used for simplifying otherwise complicated cases where the *)
(* overlapping instructions can have a block within another instruction *)
module Block = Leaderlist.Make(Insn_cfg.G)

type decision_tree = (sheath Tree.t)
type decision_tree_set = decision_tree Addr.Map.t

(** The choice set represents a set of potentially inter-dependent
      decision trees and potential requirements of selection at each
      node. Although a graph is used, the actual structure is acyclic.
      The addr -> terminal map expresses relationships between the
      graph with which it is paired with other members of the
      enclosing decision_tree_set *)
type t = decision_tree_set

let leaders_of_blocks blocks = 
  let insn_map = Addr.Hash_set.create () in
  List.iter ~f:(fun block ->
      match block with
      | leader_addr :: _ ->
        Hash_set.add insn_map leader_addr
      | _ -> ()) blocks;
  insn_map

let entries_of_cfg insn_cfg = Insn_cfg.G.fold_vertex (fun addr accu ->
    if Insn_cfg.G.in_degree insn_cfg addr = 0 then
      (Hash_set.add accu addr; accu)
    else accu)
    insn_cfg (Addr.Hash_set.create ())

let conflicts_of_entries entries insn_map =
  let visited_entries = Addr.Hash_set.create () in
  Hash_set.fold entries ~init:[] ~f:
    (fun conflicted_entries entry -> 
       if not (Hash_set.mem visited_entries entry) then (
         let in_entry_conflicts = 
           Insn_cfg.conflicts_within_insn insn_map entry in
         if (Hash_set.length in_entry_conflicts) > 0 then (
           Hash_set.add visited_entries entry; 
           let conflicts = Addr.Hash_set.create () in
           Hash_set.iter in_entry_conflicts 
             ~f:(fun conflict ->
                 (* A conflict that an entry may have may or may not *)
                 (* itself be an entry. *)
                 Hash_set.add visited_entries entry;
                 if Hash_set.mem entries conflict then (
                   Hash_set.add conflicts conflict;
                 )
               );
           Hash_set.add conflicts entry;
           conflicts :: conflicted_entries
         ) else conflicted_entries
       ) else conflicted_entries
    ) 


let decision_tree_of_entries conflicted_entries tails insn_cfg =
  let visited_choices = Addr.Hash_set.create () in
  List.map conflicted_entries ~f:(fun entries ->
      let decision_tree = Insn_cfg.G.create () in
      Hash_set.iter entries
        ~f:(fun entry -> 
            let add_choices = 
              (fun current_vert -> 
                 let unvisited =
                   not (Hash_set.mem visited_choices current_vert) in
                 if unvisited then
                   let possible_tail = current_vert in
                   match Addr.Map.find tails possible_tail with
                   | Some(sheath) ->
                     List.iter sheath ~f:(fun competitor ->
                         Hash_set.add visited_choices competitor;
                         Insn_cfg.G.add_edge decision_tree possible_tail
                           competitor;
                       );
                   | _ -> ()
                 else ();
              ) in
            (* TODO need to keep track of egress nodes along the *)
            (* way by storing them in edges *)
            Insn_cfg.Dfs.prefix_component add_choices insn_cfg entry;
            let width = 8*(Addr.size_in_bytes entry) in
            let saved_vert = ref @@
              Addr.of_int ~width 0 in
            let link_choices = 
              (fun current_vert -> 
                 let contained = Insn_cfg.G.mem_vertex
                     decision_tree current_vert in
                 let is_new = not @@ Addr.(current_vert = !saved_vert) in
                 if contained && is_new then (
                   if not @@ Insn_cfg.G.mem_edge decision_tree !saved_vert
                       current_vert then
                     Insn_cfg.G.add_edge decision_tree !saved_vert
                       current_vert;
                   saved_vert := current_vert;
                 )
              ) in
            (* Would like to have fold_component; version not available *)
            Insn_cfg.Dfs.prefix_component link_choices insn_cfg entry;
          );
      decision_tree
    )

let tails_of_conflicts conflicts insn_cfg entries = Hash_set.fold entries ~init:Addr.Map.empty
    ~f:(fun accu entry -> 
        let blocks = Block.leader_lists insn_cfg entry in
        let leaders = leaders_of_blocks blocks in
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
        let tails = List.fold ~init:Addr.Map.empty 
            ~f:(fun tails block -> 
                let rblock = List.rev block in 
                match rblock with 
                | tail::_ ->
                  (* For each edge from tail, lookup the respective vertex; if it *)
                  (* is in the conflicts set, then it gets added to a sheath of choices. *)
                  let f = (fun poss_conflict sheath -> 
                      let is_conflict = Hash_set.mem conflicts poss_conflict in
                      let is_leader = Hash_set.mem leaders poss_conflict in
                      let is_connected = 
                        match Insn_cfg.G.find_all_edges
                                insn_cfg tail poss_conflict with
                        | [] -> false | _ -> true in
                      if is_conflict && is_leader && is_connected then
                        poss_conflict :: sheath
                      else sheath) in
                  let sheath = Insn_cfg.G.fold_succ
                      f insn_cfg tail [] in
                  (match sheath with
                   | [] -> tails
                   | sheath -> 
                     Addr.Map.add tails ~key:tail ~data:sheath)
                | _ -> tails) blocks in
        tails
      ) 

(** Accepts a per instruction control flow graph, and a map from addr *)
(** to (insn, mem) *)
let decision_trees_of_shingles insn_cfg insn_map = 
  (* Here, for each vertex, look up the insn from the map and *)
  (* identify conflicts. *)
  let conflicts = Insn_cfg.find_all_conflicts insn_map insn_cfg in
  (* entries variable:
     We want to know the superset of all nodes that could be the
     terminating point that would otherwise be the return instruction
     of a function. *)
  let entries = entries_of_cfg insn_cfg in  
  (*
     we need to keep track of the subset of potential choices 
     that fall in line with the normal control flow graph, and
     leave the discovery of overlapping redirection to a 
     second pass, in order that when we do a map over all 
     instructions to check for conflicts, we know which are tails
     in order to properly construct the sheath type.
  *)
  let tails = tails_of_conflicts conflicts insn_cfg entries in
  (* It may be that some entries are accidental indirections that *)
  (* happen to preside at the intended entry. These must map to to an *)
  (* entirely distinct interpretation. *)
  let conflicted_entries = conflicts_of_entries entries insn_map in
  (* For each of the potentially conflicting entries, construct a *)
  (* decision tree. *)
  let decision_trees = decision_tree_of_entries
      conflicted_entries tails insn_cfg in
  decision_trees
