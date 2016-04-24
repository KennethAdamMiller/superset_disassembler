open Core_kernel.Std
open Regular.Std
open Bap.Std
open Shingle

module Interval = struct
  type t = { start : addr; finish : addr}
  let compare interval1 interval2 = 
    let c1 = Addr.compare interval1.finish interval2.start in
    let c2 = Addr.compare interval1.start  interval2.finish in
    if c1 < 0 then c1 else if c2 > 0 then c2 else 0
  let sexp_of_t interv = 
    Array.sexp_of_t Addr.sexp_of_t [| interv.start ; interv.finish; |]
  let create ~min:start ~max:finish = { start ; finish }
  let start interval = interval.start
  let finish interval = interval.finish
end

module Interpretation = struct
  type t = shingle Addr.Map.t
  let empty () = Addr.Map.empty
  let agrees interp1 interp2 = 
    match (Map.min_elt interp1), (Map.min_elt interp2),
          (Map.max_elt interp1), (Map.max_elt interp2)
    with
    | Some (min_i1,_), Some(min_i2,_), Some(max_i1,_), Some(max_i2,_) -> 
      let low_bound = Addr.max min_i1 min_i2 in
      let upper_bound = Addr.min max_i1 max_i2 in
      let seq1 = Map.to_sequence interp1 ~order:`Increasing_key 
          ~keys_greater_or_equal_to:low_bound 
          ~keys_less_or_equal_to:upper_bound in
      let seq2 = Map.to_sequence interp2 ~order:`Increasing_key
          ~keys_greater_or_equal_to:low_bound 
          ~keys_less_or_equal_to:upper_bound in
      let rec check_elems seq1 seq2 = 
        match (Seq.next seq1), (Seq.next seq2) with 
        | Some ((key1,val1),remaining1), Some ((key2,val2),remaining2) ->
          let result = compare key1 key2 in
          if result = 0 then
            check_elems remaining1 remaining2
          else false
        | _, _ -> true in
      check_elems seq1 seq2
    | _, _, _, _ -> false
  let merge interp1 interp2 = 
    Map.merge interp1 interp2 ~f:(fun ~key value ->
        match value with 
        | `Left(v) -> Some(v)
        | `Right(v) -> Some(v)
        | `Both(v1,v2) -> Some(v1)
      )
    (* TODO fold, rev_map, add *)
end

type span = { interval : Interval.t; interp : Interpretation.t }

let compare span1 span2 = 
  if (Interval.compare span1.interval span2.interval) = 0 then
    Interpretation.agrees span1.interp span2.interp
  else true

(* TODO check for fit *)
let try_extend span interpretation = 
  match Map.min_elt interpretation, Map.max_elt interpretation with
  | Some(other_min,_), Some(other_max,_) ->
    let interval = Interval.create
        ~min:(Addr.min (Interval.start span.interval) other_min) 
        ~max:(Addr.max (Interval.finish span.interval) other_max) in
    let interp = Interpretation.merge span.interp interpretation in
    Some { interval ; interp }
  | _,_ -> None

