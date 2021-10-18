open Core_kernel
open Bap.Std

let iterate rounds f superset =
  let (superset) = f superset in
  let rec do_analysis round superset = 
    if round = rounds then superset else
      let (superset) = f superset in
      let superset = Trim.run superset in
      do_analysis (round+1) superset in
  do_analysis 1 superset

let protect superset f =
  let visited = Addr.Hash_set.create () in
  let callsites = Heuristics.get_callsites ~threshold:0 superset in
  let superset = Heuristics.tag_callsites visited ~callsites superset in
  (* A visited address is expected to be a true positive, but under
   * some unexpectedly complicated possibilities at edge corner
   * cases, they could get marked bad by belonging on the edge. *)
  let r = f superset in 
  Superset.Core.clear_each superset visited;
  r
  
let converge superset heuristics feature_pmap =
  let superset = 
    Heuristics.with_featureset heuristics superset
      ~init:(superset)
      ~f:(fun fname feature (superset) ->
        let superset =
          Trim.run @@ feature superset in
        superset
      ) in
  let superset = Trim.run superset in
  let cache = Addr.Hash_set.create () in
  List.iter Map.(keys feature_pmap) ~f:(fun addr -> 
      Traverse.mark_descendent_bodies_at superset ~visited:cache addr
    );
  Trim.run superset
