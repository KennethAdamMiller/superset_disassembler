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
  let insn_map = Addr.Map.add insn_map ~key:conflict
      ~data:(create_memory arch conflict junk_data |> ok_exn, ()) in
  let insn_map = Addr.Map.add insn_map ~key:Addr.(conflict ++1)
      ~data:(create_memory arch Addr.(conflict ++1) junk_data |> ok_exn, ()) in
  let insn_map = Addr.Map.add insn_map ~key:Addr.(at ++ 1) 
      ~data:(create_memory arch Addr.(at ++ 1) junk_data
             |> ok_exn, ()) in
  insn_map, insn_cfg

let construct_tail_conflict insn_map insn_cfg tail_addr conflict_count =
  let tail_data = String.create 1 in
  let insn_map = Addr.Map.add insn_map ~key:tail_addr
      ~data:(create_memory arch tail_addr tail_data |> ok_exn, ()) in
  let rec make_tail_options insn_map insn_cfg tail_addr conflict_count =
    if conflict_count > 0 then
      let junk_data = String.create conflict_count in
      let conflict_addr = Addr.(tail_addr ++ conflict_count) in
      Insn_cfg.G.add_edge insn_cfg tail_addr conflict_addr;
      let insn_map = Addr.Map.add insn_map ~key:conflict_addr 
          ~data:(create_memory arch conflict_addr junk_data |> ok_exn, (
            )) in
      make_tail_options
        insn_map insn_cfg tail_addr (conflict_count - 1)
    else 
      insn_map, insn_cfg
  in make_tail_options insn_map insn_cfg tail_addr conflict_count

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
  | conflict_set :: nil -> 
    assert_equal true (Hash_set.mem conflict_set entry);
    assert_equal true (Hash_set.mem conflict_set @@ Addr.succ entry);
  | _ -> assert_equal true false ~msg:"Should have single conflict set";
    List.iter conflicts ~f:(fun conflict ->
        assert_equal ~msg:"expected 2 conflicts" 
          (Hash_set.length conflict) 2)

let test_tails_of_conflicts test_ctxt =
  let entry = Addr.(of_int ~width 1) in
  let tail = Addr.(of_int ~width 30) in
  let insn_map, insn_cfg = init () in
  let insn_map, insn_cfg =
    construct_entry_conflict insn_map insn_cfg 
      entry 10 in
  let insn_map, insn_cfg = 
    construct_tail_conflict insn_map insn_cfg tail 4 in
  let conflicts = Insn_cfg.find_all_conflicts insn_map insn_cfg in
  let entries = Sheath_tree_set.entries_of_cfg insn_cfg in
  let tails = Sheath_tree_set.tails_of_conflicts
      conflicts insn_cfg entries in
  assert_equal true (Addr.Map.length tails = 1)
    ~msg:"There should be exactly one tail" 

let test_tail_construction test_ctxt = ()

let test_extenuating_tail_competitors test_ctxt = ()

let test_many_tail_competitors test_ctxt = ()

(* TODO construct an entry conflict and ensure 
   that the two are reachable from addr 0.  *)
let test_decision_tree_of_entries test_ctxt =
  let insn_map, insn_cfg = init () in
  let entry = Addr.(of_int ~width 1) in
  let insn_map, insn_cfg =
    construct_entry_conflict insn_map insn_cfg 
      entry 10 in
  let entries = Sheath_tree_set.entries_of_cfg insn_cfg in
  let conflicts = Insn_cfg.find_all_conflicts insn_map insn_cfg in
  let tails = Sheath_tree_set.tails_of_conflicts
      conflicts insn_cfg entries in
  let conflicted_entries = Sheath_tree_set.conflicts_of_entries
      entries insn_map in
  let decision_trees = Sheath_tree_set.decision_tree_of_entries
      conflicted_entries tails insn_cfg in
  assert_equal true @@ (not ((List.length decision_trees) = 0))

(* This test is intended to be only slightly different from the entry *)
(* conflict test, differing by the fact that it will occur inline of *)
(* the insn_cfg. *)
let test_overlay_construction test_ctxt = ()

let test_decision_construction_combinatorics test_ctxt = ()

let test_conflicted_entry_decisions test_ctxt = ()

(* Add two completely discrete cfg to the graph, and assert that two *)
(* decision sets, which are identical in structure are found by the *)
(* decision tree construction algorithm. *)
let test_decision_sets_of_discrete_components test_ctxt = ()

let () =
  let suite = 
    "suite">:::
    [
      "test_construct_entry_conflict"
      >:: test_construct_entry_conflict;
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
