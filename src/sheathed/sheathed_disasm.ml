open Bap.Std
open Shingle
open Sheath_choice
open Span

(* identify sheath members, indirection
   tails is a radix set with demarkers telling the point of common tail beginning; 
    the ancestors of this point are sheath members *)
(* indirections is a radix set of addresses of jump instructions *)
let disasm cfg superset =
  let tails = Interpretation.empty in
  let indirections = Interpretation.empty in
  let rec locate_components tails indirections base i =
    if i==0 then (tails, indirections) else
      match superset.(i) with
      | Possibly_code len -> 
        let imm_ancestor_count = G.static_ancestors_of cfg i |> Seq.length in
        let rem_ancestor_count = G.remote_ancestors_of cfg i |> Seq.length in
        let tails = if imm_ancestor_count > 1 then
	    Interpretation.add tails Addr.(base++i)
          else tails in
        let indirections = 
          if insn_at i |> is_indirection then
	    Interpretation.add indirections Addr.(base++i)
          else indirections in
        locate_components tails indirections base (i-1)
      | _ -> locate_components tails indirections base (i-1)
  in
  (**********************)
  (* for each tail, maximally extend the span that represents each member 
     then classify the member before inserting it into the choice set *)
  let rec calculate_sheath_members cfg choices tails = 
    Interpretation.rev_seq tails |> Seq.iter ~f:(fun tail -> 
        Seq.iter (G.predecessors tail) ~f:(fun member -> 
	    ChoiceSet.insert choices @@ 
	    Sheath.classify @@ 
	    G.extend_sheath_member cfg member
	  )
      )
  in
  (**********************)
  (* Insert all zero interpretation depth spans of into choice set *)
  let rec insert_zero_depth choices i =  (* TODO could have ChoiceSet.promote accept a span *)
    if i > 0 then
      if ChoiceSet.depth choices i <= 1 then
        let choices = ChoiceSet.select choices i in
        insert_zero_depth choices (i-1)
      else
        insert_zero_depth choices (i-1)
    else choices
  in
  (**********************)
  let group_grammars choices indirections =
    Interpretation.fold ~init:choices indirections ~f:(fun choices jmp ->
        (* predecessor exploration is unconditional *)
        let pred_block = BasicBlock.recognize_predecessor jmp in
        (* Iterate over targets, and attempt to add them; 
           this covers cases of call, conditional indirection 
           and static jump targets  *)
        let (implications,choices) = Seq.fold ~accu:(Implications.empty, choices)
	    (Indirection.targets_of jmp ++ Indirection.successors_of jmp) 
	    ~f:(fun (implication,choices) targ -> (Implications.add_target_of implication jmp targ 
					             (BasicBlock.recognize_successor targ)
			                          ))
        in ChoiceSet.set_top choices igrammars)
  in
  (**********************)
  (* determine implications *)
  let calculate_implications choices = 
    Seq.iter (ChoiceSet.conflicts_seq choices) ~f:(fun conflicts ->
        Seq.iter conflicts ~f:(fun conflict_opt -> 
            if ChoiceSet.fits_cleanly choices conflict_opt then
              let current_grammar = ChoiceSet.coalesced choices in
              match InverseGrammar.try_coalesce G current_grammar conflict_opt with
              | Some (new_grammar) -> ChoiceSet.set_top new_grammar (* TODO possibly defer this until all choices have been considered *)
              | None -> Implications.calculate_yield current_grammar conflict_opt   (* TODO retain yield in implications *)
              (* TODO is yield guaranteed to be properly calculated yet? *)
            else (* TODO what else can be done here? Cardinality
                    check? *)
              choices
          )) in
  (**********************)
  (* Use the choice set to iterate through overlapping 
     regions where any possible reinterpretation of indirection has occurred *)
  let false_indirection_recognition choices indirections = 
    Seq.fold ~accu:choices (Interpretation.seq indirections) ~f:(fun indirection ->
        if ChoiceSet.depth indirection > 1 then
          ChoiceSet.demote indirection
        else choices
      )
  in
  (**********************)
  let (tails, indirections) =
    locate_components tails indirections base (Span.len superset) in
  let choices = calculate_sheath_members cfg choices tails in
  let choices = insert_zero_depth choices max_addr in
  let choices = group_grammars choices indirections in
  let choices = calculate_implications choices in
  let choices = false_indirection_recognition choices indirections in
  ChoiceSet.finalize choices
