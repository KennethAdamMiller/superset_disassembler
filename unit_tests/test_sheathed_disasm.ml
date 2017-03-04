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

let width = 8*(Arch.size_in_bytes arch);;

let init () = 
  let insn_cfg = Insn_cfg.G.create () in
  let insn_map = Addr.Map.empty in
  insn_map, insn_cfg

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

let construct_entry_conflict insn_map insn_cfg at conflict_len = 
  let junk_data = String.create conflict_len in
  let conflict = Addr.(at ++ conflict_len) in
  Insn_cfg.G.add_edge insn_cfg at conflict;
  Insn_cfg.G.add_edge insn_cfg Addr.(at ++ 1) Addr.(conflict ++ 1);
  let insn_map = Addr.Map.add insn_map ~key:at 
      ~data:(create_memory arch at junk_data |> ok_exn, ()) in
  let insn_map = Addr.Map.add insn_map ~key:Addr.(at ++ 1) 
      ~data:(create_memory arch Addr.(at ++ 1) junk_data
             |> ok_exn, ()) in
  insn_map, insn_cfg

let rec construct_tail_conflict insn_map insn_cfg tail_addr conflict_count =
  if conflict_count > 0 then
    let junk_data = String.create conflict_count in
    let conflict_addr = Addr.(tail_addr ++ conflict_count) in
    Insn_cfg.G.add_edge insn_cfg tail_addr conflict_addr;
    let insn_map = Addr.Map.add insn_map ~key:conflict_addr 
        ~data:(create_memory arch conflict_addr junk_data |> ok_exn, ()) in
    construct_tail_conflict 
      insn_map insn_cfg tail_addr (conflict_count - 1)
  else 
    insn_map, insn_cfg

(* TODO write this to run in a loop, over different conflict lengths *)
(* and at different addresses *)
let test_construct_entry_conflict test_ctxt = 
  let insn_map, insn_cfg = init () in
  let entry = Addr.(of_int ~width 1) in
  let insn_map, insn_cfg =
    construct_entry_conflict insn_map insn_cfg 
      entry 10 in
  let entries = Sheath_tree_set.entries_of_cfg insn_cfg in
  let conflicts = Sheath_tree_set.conflicts_of_entries
      entries insn_map in
  match conflicts with
  | conflict_set :: [] -> 
    assert_equal true (Hash_set.mem conflict_set entry);
    assert_equal true (Hash_set.mem conflict_set @@ Addr.succ entry);
  | _ -> assert_equal true false ~msg:"Should single conflict set";
    List.iter conflicts ~f:(fun conflict ->
        assert_equal ~msg:"expected 2 conflicts" 
          (Hash_set.length conflict) 2)

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
      "test_construct_entry_conflict"
      >:: test_construct_entry_conflict;
      "test_conflicts_of_entries" >:: test_conflicts_of_entries;
      "test_decision_tree_of_entries"
      >:: test_decision_tree_of_entries;
      "test_tails_of_conflicts" >:: test_tails_of_conflicts;
      "test_tail_construction">:: test_tail_construction;
      "test_many_tail_competitors">:: test_many_tail_competitors;
      "test_extenuating_tail_competitors">::test_extenuating_tail_competitors;
      "test_overlay_construction">:: test_overlay_construction;
      "test_decision_construction_combinatorics"
      >:: test_decision_construction_combinatorics;
      "test_conflicted_entry_decisions"
      >:: test_conflicted_entry_decisions;
      "test_decision_sets_of_discrete_components" >:: test_decision_sets_of_discrete_components;      
    ] in
  run_test_tt_main suite
;;
