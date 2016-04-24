open Core_kernel.Std
open Bap.Std
open Inverse_grammar
open Sheath
open Span

module ChoiceSet = struct
  type t = grammar list
  let empty () = []
  let set_top choices grammar =
    grammar :: choices
  let conflicts_seq choices interval =
    Seq.filter_map (Seq.of_list choices) ~f:(fun span ->
        Span.intersections span interval 
      )
  let insert choices meminterval interpretation =
    let added = ref false in
    Seq.unfold ~init:(Seq.of_list choices)
      ~f:(fun spans -> 
          match Seq.next spans with
          | Some (span, remaining) ->
            if !added then
              Some (span, remaining)
            else
              let t = 
                Span.insert span meminterval interpretation ~added in
              Some (t, remaining)
          | None -> None
        ) |> Seq.to_list
    (* TODO *)
    (* let demote choices i *)
    (* let select choices i *)
    (* let depth i   *)
end


module Implication = struct
  type implication = No_implication
                   | Choices of ChoiceSet.t (* this or that? *)
                   | Orphan_sheath_loss of sheath
                   | Orphan_grammar_loss of (int * grammar)
                   | Orphan_grammar_gain of (int * grammar)
                   | CFG_grammar_loss of (int * grammar)
                   | CFG_grammar_gain of (int * grammar)
  type yield = { add : implication; drop : implication }
  (* TODO
     val calculate_yield : grammar -> span -> yield
     val empty : unit -> implication
     val add_target_of : implication -> insn -> addr*)
end
