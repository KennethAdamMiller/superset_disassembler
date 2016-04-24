open Bap.Std
open Core_kernel.Std
open Shingle
open Span
open Disasm_expert.Basic

type stack_mod = 
    Zero_sum_offset
  | Non_zero_sum_offset of (int) (* size *)
type base_mod = 
    Zero_sum_offset
  | Non_zero_sum_offset of (int) (* size *)
type convention = Convention_violation of string (* convention name *)
                | Convention_conformant of string (* convention name *)
type tags = stack_mod * base_mod * convention list

type member_interpretation = span
type sheath_member =   
  | ExtReferenced of member_interpretation * tags
  | Orphan of member_interpretation * tags

type sheath = { overlap : Span.Interval.t; 
                members : sheath_member list }

(* TODO *)
(*let classify raw_member = 
  let stack = 0 in let base = 0 in
  Map.to_sequence ~order:`Increasing_key raw_member |> 
  Seq.fold ~init:(stack,base,[]) ~f:(fun accu shingle ->
      let (mem, insn_opt) = shingle in
      let offset = Mem.min_addr mem in
      (* lookup min_addr mem in graph, check predecessors *)

    )
*)
