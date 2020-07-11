open Core_kernel
open Bap.Std
open Superset_risg
open Graphlib.Std
open Graph


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

let filter_components ?(min_size=20) components = 
  List.fold_left components ~init:Addr.Set.empty
    ~f:(fun keep  component ->
        let component = Addr.Set.of_list component in
        if Set.length component > min_size then
          Addr.Set.(union keep component)
        else
          keep
      )

let raw_loops superset = 
  Superset.ISG.with_graph superset ~f:(fun insn_risg -> 
      Superset_risg.StrongComponents.scc_list insn_risg)

let filtered_loops ?(min_size=20) superset =
  filter_components ~min_size @@ raw_loops superset


let tag_loop_contradictions ?(min_size=20) superset =
  let keep = filtered_loops ~min_size superset in
  (* Here we have to be careful; we only want to find instructions
     that occur within a loop that produce a self-contradiction *)
  let parents = parents_of_insns superset keep in
  let to_remove = 
    Superset.Occlusion.conflicts_within_insns superset keep in
  let to_remove = Set.inter to_remove parents in
  let to_remove = Set.diff to_remove keep in
  printf "tagged %d contradictions of %d parents of %d to keep\n" 
    Set.(length to_remove)
    Set.(length parents)
    Set.(length keep);
  Set.iter to_remove ~f:(Superset.Core.mark_bad superset);
  superset

let default_tags = [tag_loop_contradictions]
