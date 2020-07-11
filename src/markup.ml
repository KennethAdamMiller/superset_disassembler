open Bap.Std
open Core_kernel

let mark_threshold_with_pmap ?visited ?datas superset pmap threshold = 
  let visited = Option.value visited 
      ~default:(Addr.Hash_set.create ()) in
  let datas = Option.value datas
      ~default:(Addr.Hash_set.create ()) in
  Map.iteri pmap ~f:(fun ~key ~data ->
      let addr = key in
      let p = data in
      if p > threshold then (
        if Superset.Core.mem superset addr then
          Superset.mark_descendent_bodies_at
            ~datas ~visited superset addr;
      )
    )

let mark_tps superset visited = 
  (*if Superset_risg.G.mem_vertex insn_risg bad then*)
  Hash_set.iter visited 
    ~f:(fun tp -> 
        if Superset.Core.mem superset tp then
          Superset.Core.clear_bad superset tp)

let collect_bad superset =
  let visited = Addr.Hash_set.create () in
  let _ = Superset.with_bad superset ~visited 
      ~pre:(fun _ _ -> ()) ~post:(fun _ _ -> ()) () in
  visited

(* TODO need to do more to make sure that those are actually
   retained, since it may be the case that a descendant identifies as
   bad as well.
   1) may be able to tack this onto trim, so that we disable it at
   retain points. would make sense if the superset type was extensible.
*)
let enforce_uncertain superset visited datas pmap =
  Map.iteri !pmap ~f:(fun ~key ~data -> 
      let addr = key in
      let prob = data in      
      let mark_good addr =
        Superset.Core.clear_bad superset addr in
      if prob >= 0.85 then
        mark_good addr
    )

(* TODO this should just be a modular implicit on the iter function *)
let check_convergence superset visited =
  Hash_set.iter visited ~f:(fun tp -> 
      Superset.Core.clear_bad superset tp
    )

