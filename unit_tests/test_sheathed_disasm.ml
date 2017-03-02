open OUnit2
open Core_kernel.Std
open Bap.Std
open Or_error
open Common
open Bap_plugins.Std
open Insn_cfg
module Cfg = Graphs.Cfg

let () = Pervasives.ignore(Plugins.load ())

let arch = Option.value ~default:`x86 @@ Arch.of_string "x86_64";;

let construct_loop insn_map insn_cfg start finish = 
  if Addr.(finish > start) then (
    (* Where we would otherwise connect the nodes from the tail
       condition back up to the loop body entry, here all the edges are
       reversed in the spirit of the disassembled insn_cfg. *)
    Insn_cfg.G.add_edge insn_cfg start finish;
    let rec construct_loop_body insn_map start finish = 
      if not (Addr.equal start finish) then
        let one  = (Addr.of_int 1 ~width:(Addr.size_in_bytes finish)) in
        let two  = (Addr.of_int 2 ~width:(Addr.size_in_bytes finish)) in
        let dist = Addr.max Addr.((finish - start)/two) one in
        let step = Addr.(start + dist) in
        (* Add edge from some intermediate point between the start and
           the finish going from the calculated step toward the parameter
           start, decreasing the distance between the outermost start
           and finish. As this function executes, it creates log(dist)
           nodes, each going from finish to step (reverse as it would
           be a flow in a real binary) before finally reaching the start *)
        Insn_cfg.G.add_edge insn_cfg step start;
        (* Because the algorithm at this point relies on the graph
           and map entirely, it doesn't matter the contents of the memory. *)
        let junk_data = String.create @@ (Addr.to_int dist |> ok_exn) in
        let insn_map = Addr.Map.add insn_map ~key:step
            ~data:(create_memory arch step junk_data |> ok_exn) in
        construct_loop_body insn_map step finish in
    construct_loop_body insn_map start finish
  )

let construct_branch insn_cfg branch_at left right = 
  Insn_cfg.G.add_edge insn_cfg left branch_at;
  Insn_cfg.G.add_edge insn_cfg right branch_at

let construct_entry_conflict insn_map insn_cfg = ()

let construct_tail_conflict tail_addr conflict_count =  ()

let construct_overlay_conflict = ()

let test_conflicts_of_entries test_ctxt = ()

let test_decision_tree_of_entries test_ctxt = ()

let test_tails_of_conflicts test_ctxt = ()

let test_tail_construction test_ctxt = ()

let test_many_tail_competitors test_ctxt = ()

let test_extenuating_tail_competitors test_ctxt = ()

let test_overlay_construction test_ctxt = ()

let test_decision_construction_combinatorics test_ctxt = ()

let test_conflicted_entry_decisions test_ctxt = ()

let test_decision_sets_of_discrete_components test_ctxt = ()

(* need to test decisions of shingles with composable edge cases *)
(* to do this, need some functions that construct sub pieces of the *)
(* graph with respect to edge cases *)

let () =
  let suite = 
    "suite">:::
    [
      "test_tail_construction">:: test_tail_construction;
      "test_many_tail_competitors">:: test_many_tail_competitors;
      "test_extenuating_tail_competitors">::test_extenuating_tail_competitors;
      "test_overlay_construction">:: test_overlay_construction;
      "test_conflicted_entry_decisions" >:: test_conflicted_entry_decisions;
      "test_decision_construction_combinatorics">:: test_decision_construction_combinatorics;
    ] in
  run_test_tt_main suite
;;
