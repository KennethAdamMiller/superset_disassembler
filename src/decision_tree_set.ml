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

let is_entry insn_isg addr = 
  Superset_rcfg.G.in_degree insn_isg addr  = 0 &&
  Superset_rcfg.G.out_degree insn_isg addr > 0

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
         Hash_set.add visited_entries entry;
         let in_entry_conflicts = 
           Superset_rcfg.conflicts_within_insn_at insn_map entry in
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

let iter_component ?visited ?(pre=fun _ -> ()) ?(post=fun _ -> ()) g v =
  let visited = Option.value visited 
      ~default:(Addr.Hash_set.create ()) in
  let rec visit v =
    Hash_set.add visited v;
    pre v;
    Superset_rcfg.G.iter_succ
      (fun w -> if not (Hash_set.mem visited w) then visit w) g v;
    post v
  in visit v

let calculate_deltas superset ?entries is_option = 
  let open Superset in
  let entries = Option.value entries 
      ~default:(entries_of_cfg superset.insn_rcfg) in
  let add_data_of_insn dataset at = 
    with_data_of_insn superset at ~f:(Hash_set.add dataset)
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
      deltas := Addr.Map.add !deltas addr (insns, datas);
      delta := None
    ) else (
      add_data_of_insn datas addr;
      Hash_set.add insns addr;
    )
    (* else if is in entries then store the delta in the deltas map *)
  in
  let insn_rcfg = superset.insn_rcfg in
  (* TOOD should be able to refactor this to use common hash set *)
  let visited = Addr.Hash_set.create () in
  Hash_set.iter entries 
    ~f:(iter_component ~visited ~post:make_deltas insn_rcfg);
  !deltas

(* TODO Need to find some way to cache results *)
(* TODO could split this into tail_options *)
let insn_is_option superset addr = 
  let open Superset in
  let len = Superset.len_at superset addr in
  let bound = Addr.(addr ++ len) in
  let insn_rcfg = superset.insn_rcfg in
  let previous = Superset_rcfg.G.pred insn_rcfg addr in
  List.fold ~init:false previous ~f:(fun current descedant -> 
      if not current then
        let further = Superset_rcfg.G.succ insn_rcfg descedant in
        List.fold ~init:current further ~f:(fun current opt -> 
            if not current then
              if Addr.(addr <= opt) && Addr.(opt < bound) then
                true
              else false
            else current
          )
      else current
    )

let collect_target_entries visited insn_risg insn_isg addr = 
  let target_entries = Addr.Hash_set.create () in
  let pre t = 
    if is_entry insn_risg t then
      Hash_set.add target_entries t in
  let post _ = () in
  iter_component ~visited ~pre ~post insn_isg addr;
  target_entries

let activate_descendants active insn_isg addr = 
  let pre t = 
    Hash_set.add active t in
  let post _ = () in
  iter_component ~visited:active ~pre ~post insn_isg addr

(* TODO probably the error is in the fact that ancestors may be *)
(* occlusive. Should add a check on dispatch. *)
(* TODO should just calculate entries *)
let visit ~pre ~post superset entries =
  let open Superset in
  let visited = Addr.Hash_set.create () in
  let pre addr = 
    Hash_set.add visited addr;
    pre addr in
  let insn_rcfg = superset.insn_rcfg in
  Hash_set.iter entries ~f:(fun addr -> 
      if not (Hash_set.mem visited addr) then
        iter_component ~visited ~pre ~post insn_rcfg addr
    )

let visit_with_deltas ?pre ?post ~is_option superset entries =
  let pre = Option.value pre ~default:(fun _ _ -> ()) in
  let post = Option.value post ~default:(fun _ _ -> ()) in
  let deltas = ref (calculate_deltas superset ~entries is_option) in
  let pre addr = 
    pre !deltas addr in
  let post addr = 
    post !deltas addr;
    deltas := Map.remove !deltas addr
  in
  visit ~pre ~post superset entries

(* TODO this should be moved to traverse *)
let fall_through_of superset addr =
  let len = Superset.len_at superset addr in
  Addr.(addr ++ len)

(* TODO this should be moved to traverse *)
let is_fall_through superset parent child = 
  let ft = fall_through_of superset parent in
  (* TODO should check for edge *)
  Addr.(child = ft)

(* TODO: linear scan for decision set 
   1) move every non-fall through edge into a map, target to source
   2) do dfs from every return point, keeping common insn & data hash,
   sets and referencing the map to identify false jumps and if statements
   3) if a particular addr is in the map:
      1) look up the target (jump instruction), and check if it is in
   the current insn set or data set.
      2) if it is in the insn set, then mark it in another map to
   maintain for grammar recognition. 
      3) if it is in the data set, mark it as bad
   4) prune the bad
   5) Using the data structure of diamond if statements built in 3.2),
   do another traversal where it is referenced upon returning in order
   to recognize convergence
*)
let traverse ~pre ~post superset entries = 
  let open Superset in
  let insn_risg = superset.insn_rcfg in
  let insn_isg = Superset_rcfg.Oper.mirror superset.insn_rcfg in
  let jmps = Superset_rcfg.G.fold_edges (fun src target jmps -> 
      if not (is_fall_through superset target src) then
        Map.add jmps target src
      else jmps
    ) insn_risg Addr.Map.empty in
  let visited = Addr.Hash_set.create () in
  let active = Addr.Hash_set.create () in
  let pre addr = 
    Hash_set.add visited addr;
    pre addr in
  let insn_rcfg = superset.insn_rcfg in
  Hash_set.iter entries ~f:(fun addr -> 
      if not (Hash_set.mem visited addr) then
        iter_component ~visited ~pre ~post insn_rcfg addr
    )

