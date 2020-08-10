open Core_kernel
open Bap.Std

(** A clean wrapper around raw superset that does some management of
    visited nodes for efficiency behind the scenes. *)
let visit ?visited ~pre ~post superset entries =
  let visited = Option.value visited
      ~default:(Addr.Hash_set.create ()) in
  let pre addr =
    Hash_set.add visited addr;
    pre addr in
  Hash_set.iter entries ~f:(fun addr ->
      if not (Hash_set.mem visited addr) then
        Superset.with_ancestors_at superset ~visited ~pre ~post addr
    )

(** A delta from decision trees is constructed and passed to the
    visitor functions during a visit. *)
let visit_with_deltas ?pre ?post ~is_option superset entries =
  let pre = Option.value pre ~default:(fun _ _ -> ()) in
  let post = Option.value post ~default:(fun _ _ -> ()) in
  let deltas = ref (Decision_trees.calculate_deltas
                      superset ~entries ~is_option) in
  let pre addr = 
    pre !deltas addr in
  let post addr = 
    post !deltas addr;
    deltas := Map.remove !deltas addr
  in
  visit ~pre ~post superset entries

(** This traversal unlinks all non-branch jumps, collects every
    entry, which now includes newly unlinked blocks in order to
    provide a separation over traversal. *)
(* TODO I think that this can be removed.  *)
let visit_by_block superset
    ?(pre=(fun _ _ _ -> ())) ?(post=(fun _ _ _ -> ())) entries = 
  let (jmps,targets) = Superset.ISG.fold_edges superset
      (fun src target (jmps,targets) -> 
         let is_branch = Superset.is_branch superset target in
         let is_jmp_edge = not (Superset.is_fall_through superset src target) in
         if is_branch && is_jmp_edge then
           (Map.set jmps src target, Set.add targets target)
         else (jmps, targets)
      ) (Addr.Map.empty,Addr.Set.empty) in
  (*let loop_addrs = Superset_risg.get_loop_addrs insn_risg in
    let jmps = Set.fold loop_addrs ~init:jmps ~f:(fun jmps addr -> 
      match Map.find jmps addr with
      | Some(j) -> 
    if Set.mem loop_addrs j then
          Map.remove jmps j
        else jmps
      | None -> jmps
    ) in*)
  let superset = 
    Map.fold jmps ~init:superset ~f:(fun ~key ~data superset -> 
        Superset.ISG.unlink superset key data;
      ) in
  let entries = Superset.entries_of_isg superset in
  let visited = Addr.Hash_set.create () in
  let rec visit v =
    Hash_set.add visited v;
    pre jmps targets v;
    let f w =
      if not (Hash_set.mem visited w) then
        visit w
      else pre jmps targets w in
    let ancs = Superset.ISG.ancestors superset v in
    List.iter ancs ~f;
    post jmps targets v;
  in 
  Hash_set.iter entries ~f:visit;
  Map.fold jmps ~init:superset ~f:(fun ~key ~data superset -> 
      Superset.ISG.link superset key data;
    )

