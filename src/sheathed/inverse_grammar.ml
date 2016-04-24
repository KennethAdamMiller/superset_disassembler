open Core_kernel.Std
open Bap.Std
open Sheath
open Span
open Shingle

module C = Comparator.Make(Span.Interval)

type 'a coalesced = (Span.Interval.t, 'a, C.comparator_witness) Map.t

type grammar = Undecided of sheath
      	     | Basic_block of Span.Interpretation.t
             | Coalesced_group of grammar coalesced
             | If_coalesced of grammar
             | If_else_coalesced of grammar
             | Loop_coalesced of grammar

let intersections = Avltree.find ~compare:Span.Interval.compare

let insert span interval interpretation =
  Avltree.add span ~compare:Span.Interval.compare 
    ~key:interval ~data:interpretation ~replace:false

let empty () = Map.empty

let agrees ?(f=fun x y -> true) span1 span2 = 
  let seq1 = Map.to_sequence span1 ~order:`Increasing_key in
  let seq2 = Map.to_sequence span2 ~order:`Increasing_key in
  let rec check seq1 seq2 = 
    match (Seq.next seq1), (Seq.next seq2) with 
    | Some ((key1,val1),remaining1), Some ((key2,val2),remaining2) ->
      let result = Span.Interval.compare key1 key2 in
      if result > 0 then
        check seq1 remaining2
      else if result < 0 then
        check remaining1 seq2
      else f val1 val2 
    | _, _ -> true
  in check seq1 seq2

(* TODO there is some predecessor of one of the spans that is in the other 
   let fits_cleanly current_grammar cfg member interpretation = *)

(* return grammar option/Or_error *)
let try_coalesce cfg current interpretation = 
  match current with
  | Undecided (sheath) -> 
    let { overlap; members } = sheath in
    let sheath = Seq.fold ~init:(Seq.of_list members)
        ~f:(fun accu member ->
            (* match Sheath.classify member with *)
            if fits_cleanly current cfg member interpretation then
              Span.try_extend member interpretation
            else member
          ) in
    Undecided (sheath) (* TODO maybe it didn't get added; return None *)
  | _ -> interpretation
(*  | Basic_block (shingles) ->
    | Contiguous_block (grammar_map) ->
    | If_coalesced (grammar) ->
    | If_else_coalesced (grammar) ->
    | Loop_coalesced (grammar) ->
*)
