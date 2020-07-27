open Core_kernel
open Bap.Std

let parents_of_insns superset component = 
  Set.fold component ~init:Addr.Set.empty ~f:(fun potential_parents addr -> 
      List.fold (Superset.ISG.ancestors superset addr)
        ~init:potential_parents
        ~f:(fun potential_parents ancestor ->
            if not Set.(mem component ancestor) then
              Set.add potential_parents ancestor
            else potential_parents
          ) 
    )

let addrs_of_loops loops =
  List.fold_left loops ~init:Addr.Set.empty
    ~f:(fun keep loop ->
      Addr.Set.(union keep (of_list loop))
    )
  
let filter_loops ?(min_size=20) loops =
  let loops =
    List.filter loops ~f:(fun l -> List.length l > min_size) in
  addrs_of_loops loops

let addrs_of_filtered_loops ?(min_size=20) superset =
  filter_loops ~min_size @@ Superset.ISG.raw_loops superset

(** In the body of a loop, instructions fall through eventually to
    themselves, which amounts to effectively a trigger of an
    invariant. But the level at which invariants operate is too fine
    grained to see the consequence propagated from conflicts that are
    potentially in loops that are many instructions long. This
    function cleanses the bodies of instructions that occur in loops
    of a minimum size. *)
let tag_loop_contradictions ?(min_size=20) superset =
  let keep = addrs_of_filtered_loops ~min_size superset in
  (* Here we have to be careful; we only want to find instructions
     that occur within a loop that produce a self-contradiction *)
  let parents = parents_of_insns superset keep in
  let to_remove = 
    Superset.Occlusion.conflicts_within_insns superset keep in
  let to_remove = Set.inter to_remove parents in
  let to_remove = Set.diff to_remove keep in
  Set.iter to_remove ~f:(Superset.Core.mark_bad superset);
  superset

let default_tags = [tag_loop_contradictions]
