open OUnit2
open Core_kernel.Std
open Bap.Std
open Or_error
open Common
open Bap_plugins.Std
open Superset_risg
open Superset

let () = Pervasives.ignore(Plugins.load ())

let arch = Option.value ~default:`x86_64 @@ Arch.of_string "x86_64";;

let segments = Table.empty

let width = 8*(Arch.size_in_bytes arch);;

let init () = 
  let insn_isg = Superset_risg.G.create () in
  let insn_map = Addr.Map.empty in
  insn_map, insn_isg

let make_params ?(min_addr=0) bytes =
  let arch = Arch.(`x86) in
  let addr_size= Size.in_bits @@ Arch.addr_size arch in
  let min_addr = Addr.of_int addr_size min_addr in
  let memory = create_memory arch min_addr bytes |> ok_exn in
  memory, arch

let check_results sizes expected_results = 
  let sizes = Seq.to_list sizes in
  List.iter2_exn sizes expected_results
    ~f:(fun actual_size expected_size ->
        assert_equal ~msg:((List.to_string ~f:string_of_int sizes)
                           ^ (List.to_string ~f:string_of_int
                                expected_results)) actual_size
          expected_size)  

let superset_to_length_list superset =
  List.map superset ~f:(fun (mem, insn) -> (Memory.length mem))

(* This test affirms that both the order and the inner sequences of a set of bytes
   will be interpreted appropriately by the superset conservative disassembler *)
let test_hits_every_byte test_ctxt =
  let memory, arch = make_params "\x2d\xdd\xc3\x54\x55" in
  let raw_superset = Superset.disasm
      ~accu:[] ~f:List.cons arch memory |> ok_exn in
  let sizes = superset_to_length_list raw_superset in
  let expected_results = List.rev [ 5; 2; 1; 1; 1; ] in
  check_results (Seq.of_list sizes) expected_results

let of_mem arch mem = 
  let brancher = Brancher.of_bil arch in
  let insn_risg = (Superset_risg.G.create ()) in
  let bad = Addr.Hash_set.create () in
  let superset = Superset.Fields.create
      ~arch ~brancher ~img:None ~data:()
      ~insn_map:Addr.Map.empty ~bad ~insn_risg in
  Superset.update_with_mem superset mem

let test_trim test_ctxt =
  let bytes = "\x2d\xdd\xc3\x54\x55" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  let superset = Superset.update_with_mem
      superset mem ~f:Trim.tag in
  let superset = Trim.Default.trim superset in
  (* Only the return opcode ( 0xc3 ) can survive trimming *)
  let msg = Superset.isg_to_string superset in
  (* After refactoring, it may be that some targets that fall outside
     the memory bounds are still in the graph, but this only accounts
     for one edge, so it is negligible. *)
  assert_equal ~msg 1 @@ G.nb_vertex superset.insn_risg

let test_trims_invalid_jump test_ctxt =
  let bytes = "\x55\x54\xE9\xFC\xFF\xFF\xFF" in
  let memory, arch = make_params bytes in
  let superset = of_mem arch memory in
  let superset = Superset.update_with_mem
      superset memory ~f:Trim.tag  in
  let superset = Trim.Default.trim superset in
  let expected_results = [ ] in
  assert_equal ~msg:"lengths unequal"
    (G.nb_vertex superset.insn_risg)
    (List.length expected_results)

let test_addr_map test_ctxt =
  let addr_size = Size.in_bits @@ Arch.addr_size Arch.(`x86_64) in
  let min_addr  = Addr.of_int addr_size 0 in
  let insn_map  = Addr.Map.empty in
  let insn_map  = Addr.Map.add insn_map min_addr () in
  let insn_map  = Addr.Map.add insn_map min_addr () in
  let msg = "expected length to be one" in
  assert_bool msg ((Addr.Map.length insn_map) = 1)

let test_insn_isg test_ctxt = 
  let insn_risg = Superset_risg.G.create () in
  let addr_size = Size.in_bits @@ Arch.addr_size Arch.(`x86_64) in
  let addr  = Addr.of_int addr_size 0 in
  Superset_risg.G.add_vertex insn_risg addr;
  Superset_risg.G.add_vertex insn_risg addr;
  let msg = "expected length to be one" in
  assert_bool msg ((Superset_risg.G.nb_vertex insn_risg) = 1)  

let test_consistent_superset test_ctxt = 
  let memory, arch = make_params "\x55\x54\xE9\xFC\xFF\xFF\xFF" in
  let superset = of_mem arch memory in
  let insn_risg = Superset.get_graph superset in
  let insn_map = Superset.get_map superset in
  let msg = "insn in isg but not in map after shingled of superset" in
  Superset_risg.G.iter_vertex (fun v -> 
      let msg = msg ^ Addr.to_string v in
      assert_bool msg (Map.mem insn_map v))
    insn_risg;
  let msg = "insn in map but not in after shingled of superset" in
  Addr.Map.iteri insn_map (fun ~key ~data -> 
      assert_bool msg 
        (Superset_risg.G.mem_vertex insn_risg key))

let construct_loop insn_map insn_isg start finish = 
  if Addr.(finish > start) then (
    (* Where we would otherwise connect the nodes from the tail
       condition back up to the loop body entry, here all the edges are
       reversed in the spirit of the disassembled insn_isg. *)
    Superset_risg.G.add_edge insn_isg start finish;
    let junk_data = String.create 1 in
    let start_mem = create_memory arch start junk_data |> ok_exn in
    let insn_map = Addr.Map.add insn_map start (start_mem, None) in
    let finish_mem = create_memory arch finish junk_data |> ok_exn in
    let insn_map = Addr.Map.add insn_map finish (finish_mem, None) in
    let one  = (Addr.of_int 1 ~width) in
    let two  = (Addr.of_int 2 ~width) in
    let rec construct_loop_body insn_map start finish = 
      if not (Addr.equal start finish) then
        let dist = Addr.max Addr.((finish - start)/two) one in
        let step = Addr.(start + dist) in
        (* Add edge from some intermediate point between the start and
           the finish going from the calculated step toward the parameter
           start, decreasing the distance between the outermost start
           and finish. As this function executes, it creates log(dist)
           nodes, each going from finish to step (reverse as it would
           be a flow in a real binary) before finally reaching the
           start *)
        Superset_risg.G.add_edge insn_isg step start;
        (* Because the algorithm at this point relies on the graph
           and map entirely, it doesn't matter the contents of the memory. *)
        let junk_data = String.create @@ (Addr.to_int dist |> ok_exn) in
        let insn_map = Addr.Map.add insn_map ~key:start
            ~data:(create_memory arch start junk_data |> ok_exn, None) in
        construct_loop_body insn_map step finish 
      else insn_map, insn_isg in
    construct_loop_body insn_map start finish
  ) else insn_map, insn_isg

let construct_entry_conflict insn_map insn_isg at conflict_len = 
  let junk_data = String.create conflict_len in
  let conflict = Addr.(at ++ conflict_len) in
  Superset_risg.G.add_edge insn_isg at conflict;
  Superset_risg.G.add_edge insn_isg Addr.(at ++ 1) Addr.(conflict ++ 1);
  let insn_map = Addr.Map.add insn_map ~key:at 
      ~data:(create_memory arch at junk_data |> ok_exn, None) in
  let insn_map = Addr.Map.add insn_map ~key:conflict
      ~data:(create_memory arch conflict junk_data |> ok_exn, None) in
  let insn_map = Addr.Map.add insn_map ~key:Addr.(conflict ++1)
      ~data:(create_memory arch Addr.(conflict ++1) junk_data |> ok_exn, None) in
  let insn_map = Addr.Map.add insn_map ~key:Addr.(at ++ 1) 
      ~data:(create_memory arch Addr.(at ++ 1) junk_data |> ok_exn, None) in
  insn_map, insn_isg

let construct_tail_conflict 
    insn_map insn_isg tail_addr conflict_count =
  let orig = 
    if Superset_risg.G.mem_vertex insn_isg tail_addr &&
       Superset_risg.G.out_degree insn_isg tail_addr > 0 then
      let orig = Superset_risg.G.succ insn_isg tail_addr in
      let orig = Addr.Set.of_list orig in orig
    else Addr.Set.empty in
  let insn_map, tail_len = match Addr.Map.find insn_map tail_addr with
    | Some (mem, _) -> insn_map, Memory.length mem
    | None -> 
      let tail_len = 1 in
      let tail_data = String.create tail_len in
      let mem = create_memory arch tail_addr tail_data |> ok_exn in
      let insn_map = Addr.Map.add insn_map ~key:tail_addr
          ~data:(mem, None) in
      insn_map, tail_len in 
  let rec make_tail_options insn_map conflict_count =
    if conflict_count > 0 then
      let junk_data = String.create conflict_count in
      let conflict_addr = Addr.(tail_addr -- conflict_count) in
      Superset_risg.G.add_edge insn_isg tail_addr conflict_addr;
      let insn_map = Addr.Map.add insn_map ~key:conflict_addr 
          ~data:(create_memory arch conflict_addr junk_data |> ok_exn,
                 None) in
      make_tail_options insn_map (conflict_count - 1)
    else 
      insn_map, insn_isg in
  let insn_map, insn_isg = 
    make_tail_options insn_map conflict_count in
  let opts = Superset_risg.G.succ insn_isg tail_addr in
  let opts = Addr.Set.of_list opts in
  let opts = Set.diff opts orig in
  let msg = sprintf
      "expected %d, got %d" conflict_count Set.(length opts) in
  assert_equal ~msg true (Set.(length opts) = conflict_count);
  insn_map, insn_isg

let test_construct_entry_conflict test_ctxt = 
  let rec test_entry_conflict_of_len entry conflict_len = 
    if conflict_len < 2 then
      () 
    else
      let insn_map, insn_isg = init () in
      let insn_map, insn_isg =
        construct_entry_conflict insn_map insn_isg 
          entry conflict_len in
      let entries = Superset_risg.entries_of_isg insn_isg in
      let conflicts = Decision_tree_set.conflicts_of_entries
          entries insn_map in
      (match conflicts with
       | conflict_set :: nil -> 
         assert_equal true (Hash_set.mem conflict_set entry)
           ~msg:(List.to_string ~f:Addr.to_string @@ Hash_set.to_list conflict_set);
         assert_equal true (Hash_set.mem conflict_set @@ Addr.succ entry);
       | _ -> assert_equal true false ~msg:"Should have single conflict set";
         List.iter conflicts ~f:(fun conflict ->
             assert_equal ~msg:"expected 2 conflicts" 
               (Hash_set.length conflict) 2));
      test_entry_conflict_of_len (Addr.succ entry) (conflict_len-1) in
  let conflict_len = 10 in
  let entry = Addr.(of_int ~width 1) in
  test_entry_conflict_of_len entry conflict_len

(* len tests variations on the tail location and range of total *)
(* conflicts for a given tail can still be found by the algorithm *)
let test_tail_construction test_ctxt =
  let rec test_tail_construction_with entry tail conflict_len =
    if conflict_len <= 1 then
      ()
    else
      let insn_map, insn_isg = init () in
      let insn_map, insn_isg = 
        construct_tail_conflict insn_map insn_isg tail conflict_len in
      Map.iteri insn_map ~f:(fun ~key ~data -> 
          assert_equal true @@ Superset_risg.G.mem_vertex insn_isg key;
        );
      Superset_risg.G.iter_vertex (fun vert -> 
          assert_equal true @@ Map.mem insn_map vert;
        ) insn_isg;
      let conflicts = Superset_risg.find_all_conflicts insn_map in
      let tails = Decision_tree_set.tails_of_conflicts
          conflicts insn_isg in
      let num_tails = Addr.Map.length tails in
      let msg = "There should be exactly one tail; there was "
                ^ string_of_int num_tails ^ ", tail at: "
                ^ (Addr.to_string tail) ^ " conflict len="
                ^ (string_of_int conflict_len) in
      assert_equal true (num_tails = 1)
        ~msg;
      test_tail_construction_with 
        entry Addr.(succ tail) (conflict_len-1) in
  let entry = Addr.(of_int ~width 1) in
  let tail = Addr.(of_int ~width 30) in
  let conflict_len = 10 in
  test_tail_construction_with entry tail conflict_len


(* This tests that the tail(s) recognized is differentiable from other *)
(* noise conflicts*)
let test_tails_of_conflicts test_ctxt =
  let entry = Addr.(of_int ~width 1) in
  let tail = Addr.(of_int ~width 30) in
  let insn_map, insn_isg = init () in
  let insn_map, insn_isg =
    construct_entry_conflict insn_map insn_isg 
      entry 10 in
  let insn_map, insn_isg = 
    construct_tail_conflict insn_map insn_isg tail 4 in
  let conflicts = Superset_risg.find_all_conflicts insn_map in
  let tails = Decision_tree_set.tails_of_conflicts
      conflicts insn_isg in
  assert_equal true (Addr.Map.length tails = 1)
    ~msg:"There should be exactly one tail" 

(* This should test that, for each possible number of conflicts, *)
(* there is always as many tails as were created *)
let test_extenuating_tail_competitors test_ctxt =
  let conflict_len = 4 in
  let entry = Addr.(of_int ~width 1) in
  let tail = Addr.(of_int ~width 30) in
  let insn_map, insn_isg = init () in
  let insn_map, insn_isg = 
    construct_tail_conflict insn_map insn_isg tail conflict_len in
  let extenuation_addr = Addr.(entry ++ conflict_len) in
  let insn_map, insn_isg = 
    construct_tail_conflict 
      insn_map insn_isg extenuation_addr conflict_len in  
  let conflicts = Superset_risg.find_all_conflicts insn_map in
  let tails = Decision_tree_set.tails_of_conflicts
      conflicts insn_isg in
  assert_equal true @@ Addr.Map.mem tails tail;
  assert_equal true @@ Addr.Map.mem tails extenuation_addr;
  assert_equal (Addr.Map.length tails) 2

(* construct an entry conflict and ensure 
   that the two are reachable from addr 0.  *)
let test_decision_tree_of_entries test_ctxt =
  let insn_map, insn_isg = init () in
  let zero = Addr.(of_int ~width 0) in
  let entry = Addr.(of_int ~width 1) in
  let insn_map, insn_isg =
    construct_entry_conflict insn_map insn_isg 
      entry 10 in
  let msg = sprintf "expected entry %s to be in isg" 
      (Addr.to_string entry) in
  assert_equal ~msg true (Superset_risg.G.mem_vertex insn_isg entry);
  let entries = Superset_risg.entries_of_isg insn_isg in
  let msg = sprintf "expected entry %s to be in entries" 
      (Addr.to_string entry) in
  assert_equal ~msg true (Hash_set.mem entries entry);
  let msg = sprintf "expected entry %s to be in entries" 
      Addr.(to_string @@ succ entry) in
  assert_equal ~msg true (Hash_set.mem entries Addr.(succ entry));
  let conflicts = Superset_risg.find_all_conflicts insn_map in
  let tails = Decision_tree_set.tails_of_conflicts
      conflicts insn_isg in
  let conflicted_entries = Decision_tree_set.conflicts_of_entries
      entries insn_map in
  let decision_trees = Decision_tree_set.decision_tree_of_entries
      conflicted_entries entries tails insn_isg in
  let expect_entry_msg = "Expect entry in decision_tree" in
  let expect_zero_msg = "Expect zero node in decision tree" in
  let non_empty_tree_msg = "Expect decision tree to be non empty" in
  assert_equal true @@ (not ((List.length decision_trees) = 0));
  List.iter decision_trees ~f:(fun decision_tree ->
      assert_equal ~msg:non_empty_tree_msg 
        true @@ not ((Superset_risg.G.nb_vertex decision_tree)=0);
      assert_equal ~msg:expect_entry_msg 
        true @@ (Superset_risg.G.mem_vertex decision_tree entry);
      assert_equal ~msg:expect_zero_msg 
        true @@ (Superset_risg.G.mem_vertex decision_tree zero);
    )

let test_loop_scc test_ctxt = 
  let insn_map, insn_isg = init () in
  let entry = Addr.(of_int ~width 1) in
  let insn_map, insn_isg = 
    construct_loop insn_map insn_isg entry Addr.(entry ++ 20) in
  let loop_points = Addr.Hash_set.create () in
  Superset_risg.G.iter_vertex (fun vert -> Hash_set.add loop_points vert) insn_isg;
  let scc = Superset_risg.StrongComponents.scc_list insn_isg in
  let scc_points = Addr.Hash_set.create () in
  List.iter scc ~f:(fun scc -> 
      List.iter scc ~f:(fun component_addr -> 
          Hash_set.add scc_points component_addr;
        ));
  let in_loop_not_scc = "Found addr in loop but not in scc" in
  Hash_set.iter loop_points ~f:(fun loop_addr ->
      assert_equal ~msg:in_loop_not_scc true
      @@ Hash_set.mem scc_points loop_addr);
  let in_scc_not_loop = "Found addr in scc not loop" in
  Hash_set.iter scc_points ~f:(fun loop_addr ->
      assert_equal ~msg:in_scc_not_loop true
      @@ Hash_set.mem loop_points loop_addr)

let test_scc test_ctxt =
  let _, insn_isg = init () in
  let zero = Addr.(of_int ~width 0) in
  let entry = Addr.(of_int ~width 1) in
  Superset_risg.G.add_edge insn_isg zero entry;
  let components = Superset_risg.StrongComponents.scc_list insn_isg in
  let components = Sheathed.filter_components components in
  assert_equal ~msg:"found non scc component" 0 (Set.length components)

let test_find_conflicts test_ctxt = 
  let insn_map, insn_isg = init () in
  let in_loop_addr = Addr.(of_int ~width 0x10) in
  let num_conflicts = 6 in
  let insn_map, insn_isg =
    construct_tail_conflict insn_map insn_isg in_loop_addr num_conflicts in
  let conflicts = Superset_risg.find_all_conflicts insn_map in 
  assert_equal Set.(length conflicts) num_conflicts

let test_is_option test_ctxt = 
  let insn_map, insn_isg = init () in
  let in_loop_addr = Addr.(of_int ~width 0x10) in
  let num_conflicts = 6 in
  let insn_map, insn_risg =
    construct_tail_conflict insn_map insn_isg in_loop_addr
      num_conflicts in
  let superset = Superset.create ~insn_risg arch ~insn_map () in
  let is_option = (Decision_tree_set.insn_is_option superset) in
  let deltas = Decision_tree_set.calculate_deltas superset
      is_option in
  let msg = sprintf
      "expected 6 options to be identified, saw %d" 
      Map.(length deltas) in
  assert_bool msg (Map.(length deltas) = num_conflicts)

let test_trim_scc test_ctxt = 
  let insn_map, insn_isg = init () in
  let entry = Addr.(of_int ~width 1) in
  let brancher = Brancher.of_bil arch in
  let insn_map, insn_isg = 
    construct_loop insn_map insn_isg entry Addr.(entry ++ 20) in
  let in_loop_addr = Addr.(of_int ~width 0x10) in
  let loop_points = Addr.Hash_set.create () in
  Superset_risg.G.iter_vertex (Hash_set.add loop_points) insn_isg;
  let insn_map, insn_risg =
    construct_tail_conflict insn_map insn_isg in_loop_addr 3 in
  let conflicts_added = Addr.Hash_set.create () in
  Superset_risg.G.iter_vertex (fun vert -> 
      if not (Hash_set.mem loop_points vert) then 
        Hash_set.add conflicts_added vert) insn_isg;
  let superset =
    Superset.Fields.create ~insn_map ~insn_risg ~arch
      ~bad:Addr.Hash_set.(create ()) ~img:None ~brancher ~data:() in
  let components = Superset_risg.StrongComponents.scc_list insn_isg in
  assert_bool "Should have a component" 
    (List.(length components) > 0);
  let superset = 
    Sheathed.tag_loop_contradictions ~min_size:1 superset in
  assert_bool "should have marked conflict" 
    (0 < Superset.(num_bad superset));
  (* TODO should test to make sure that loop elements do not get
     marked bad *)
  let superset = Trim.Default.trim superset in
  let insn_map = Superset.get_map superset in
  let insn_map = Map.filteri insn_map ~f:(fun ~key ~data ->
      Superset_risg.G.mem_vertex insn_risg key) in
  let superset = Superset.rebuild superset ~insn_map in
  let insn_isg = superset.insn_risg in
  let conflicts_added_str = List.to_string ~f:Addr.to_string @@ 
    Hash_set.to_list conflicts_added in
  let removed_msg = "of conflicts " ^ conflicts_added_str 
                    ^ ", residual conflict present within " in
  Hash_set.iter conflicts_added ~f:(fun addr ->
      let removed_map = removed_msg ^ " insn_map at " ^ Addr.to_string addr in
      assert_equal ~msg:removed_map true
      @@ not (Addr.Map.mem insn_map addr);
      let removed_isg = removed_msg ^ " isg at " ^ Addr.to_string addr in
      assert_equal ~msg:removed_isg true
      @@ not (Superset_risg.G.mem_vertex insn_isg addr);
    );
  let loop_msg = "loop addr should remain during tail trim" in
  Hash_set.iter loop_points ~f:(fun addr -> 
      assert_equal ~msg:loop_msg true @@ 
      Superset_risg.G.mem_vertex insn_isg addr)

(* Establishes, in the case of if structures, how topological *)
(* tranversal works - one time visit only *)
let test_topological_revisit ctxt = 
  let _, insn_risg = init () in
  let width = 32 in
  let start = Addr.of_int 0 ~width in
  let stop = Addr.of_int 2 ~width in
  let rec make_if current stop =
    if not (current = stop) then
      let next = Addr.succ current in
      Superset_risg.G.add_edge insn_risg current next;
      make_if next stop in
  make_if start stop;
  Superset_risg.G.add_edge insn_risg start stop;
  let update_count addr visit_count = 
    match Map.find visit_count addr with
    | Some (count) -> 
      let visit_count = Map.remove visit_count addr in
      Map.add visit_count addr (count+1)
    | None -> Map.add visit_count addr 1 in
  let visit_count = Topological.fold 
      update_count insn_risg Addr.Map.empty in
  Map.iteri visit_count (fun ~key ~data -> assert_equal ~ctxt data 1)

let test_cross_layer_pruning test_ctxt = 
  let insn_map, insn_risg = init () in
  let arch = Arch.(`x86) in
  let addr_size= Size.in_bits @@ Arch.addr_size arch in
  let tail_addr = Addr.of_int addr_size 50 in
  let insn_map, insn_risg =
    construct_tail_conflict insn_map insn_risg tail_addr 2 in
  let conflicts = Superset_risg.find_all_conflicts insn_map in
  let conflict_len = Set.length conflicts in
  assert_bool "should have a conflict" (conflict_len > 0);
  let layer_options = Superset_risg.G.(succ insn_risg tail_addr) in
  assert_equal true (List.(length layer_options) >= 1);
  let _ = List.fold ~init:None layer_options 
      ~f:(fun current next -> 
          let _ = Option.map current ~f:(fun current -> 
              Superset_risg.G.add_edge insn_risg current next
            ) in
          Some(next)
        ) in
  let superset = Superset.create ~insn_risg arch ~insn_map () in
  let entries = Superset_risg.entries_of_isg insn_risg in  
  assert_bool "Should have at least one entry" (Hash_set.(length entries) > 0);
  let tails = Decision_tree_set.tails_of_conflicts
      conflicts insn_risg in
  assert_bool "Should have at least one tail" 
    (Map.(length tails) > 0);
  assert_bool "Should contain tail adder" Map.(mem tails tail_addr);
  let conflicted_entries = Decision_tree_set.conflicts_of_entries
      entries insn_map in
  let conflicted_entry_count = 
    List.fold conflicted_entries ~init:0 
      ~f:(fun total conflict_set -> 
          total + (Hash_set.length conflict_set)) in
  assert_bool "no conflicted entries expected" (conflicted_entry_count = 0);
  let decision_trees = Decision_tree_set.decision_trees_of_superset
      superset in
  assert_bool "should have a decision tree" 
    List.(length decision_trees > 0);
  List.iter decision_trees ~f:(fun dtree -> 
      assert_bool "at least one decision available!" 
        (Superset_risg.G.(nb_vertex dtree) > 0)
    );
  let superset = Invariants.tag_layer_violations superset in
  let superset = Trim.Default.trim superset in
  let insn_risg = superset.insn_risg in
  let num_decisions = List.length 
      (Superset_risg.G.succ insn_risg tail_addr) in
  assert_bool "Expected one of the admissible to have been removed" 
    (num_decisions < 2)

let rec extend_back insn_map insn_isg ?(step=1) addr num =
  let make_link len =
    let dest = Addr.(addr -- len) in
    Superset_risg.G.add_edge insn_isg addr dest;
    let junk_data = String.create len in
    let mem = create_memory arch dest junk_data |> ok_exn in
    let insn_map = Map.add insn_map dest (mem, None) in
    (insn_map, insn_isg) in
  if not (num = 0) then 
    let (insn_map, insn_isg) = make_link step in
    extend_back insn_map insn_isg Addr.(addr -- step) (num - 1) ~step
  else
    insn_map, insn_isg

let make_extended_cross tail_addr =
  let insn_map, insn_risg = init () in
  let insn_map, insn_risg =
    construct_tail_conflict insn_map insn_risg tail_addr 2 in
  let layer_options = Superset_risg.G.(succ insn_risg tail_addr) in
  let insn_map, insn_risg = List.fold ~init:(insn_map, insn_risg)
      layer_options ~f:(fun (insn_map, insn_risg) opt -> 
          extend_back insn_map insn_risg opt 1 ~step:2
        ) in
  let extended_points = 
    Superset_risg.entries_of_isg 
      (Superset_risg.Oper.mirror insn_risg) in
  let _ = Hash_set.fold ~init:None extended_points
      ~f:(fun current next -> 
          let _ = Option.map current ~f:(fun current -> 
              Superset_risg.G.add_edge insn_risg current next
            ) in
          Some(next)
        ) in
  insn_map, insn_risg

let test_calculate_delta test_ctxt = 
  let addr_size= Size.in_bits @@ Arch.addr_size arch in
  let tail_addr = Addr.of_int addr_size 50 in
  let insn_map, insn_risg = make_extended_cross tail_addr in
  let superset = Superset.create ~insn_risg arch insn_map in
  let entries = Superset_risg.entries_of_isg insn_risg in
  let conflicts = Superset_risg.find_all_conflicts insn_map in
  let tails = Decision_tree_set.tails_of_conflicts
      conflicts insn_risg in
  let option_set = List.fold ~init:Addr.Set.empty (Map.data tails)
      ~f:(fun option_set options -> 
          List.fold ~init:option_set options ~f:Addr.Set.add) in
  let is_option = Set.mem option_set in
  let deltas = Decision_tree_set.calculate_deltas 
      superset ~entries is_option in
  let expected_deltas = Superset_risg.G.succ insn_risg tail_addr in
  let num_expected = List.(length expected_deltas) in
  let actual = Map.(length deltas) in
  let msg = sprintf "expected %d deltas, got %d" 
      num_expected actual in
  assert_bool msg (actual = num_expected);
  Map.iteri deltas ~f:(fun ~key ~data -> 
      let addr = key in
      let (insn_delta, data_delta) = data in
      let (data_violators, insn_violators) =
        Invariants.enforce_exclusivity insn_delta data_delta in
      let msg = sprintf "expected deltas at %s to be exclusive"
          Addr.(to_string addr) in
      let exclusive = List.(length data_violators) = 0 &&
                      List.(length insn_violators) = 0 in
      assert_bool msg exclusive;
    );
  List.iter expected_deltas ~f:(fun d_addr ->
      let msg = sprintf "expected %s" Addr.(to_string d_addr) in
      assert_bool msg Map.(mem deltas d_addr);
    )


let test_extended_cross_layer_pruning test_ctxt =
  let addr_size= Size.in_bits @@ Arch.addr_size arch in
  let tail_addr = Addr.of_int addr_size 50 in
  let insn_map, insn_risg = make_extended_cross tail_addr in
  let superset = Superset.create ~insn_risg arch ~insn_map () in
  let decision_trees = Decision_tree_set.decision_trees_of_superset
      superset in
  assert_bool "should have a decision tree" 
    List.(length decision_trees > 0);
  List.iter decision_trees ~f:(fun dtree -> 
      assert_bool "at least one decision available!" 
        (Superset_risg.G.(nb_vertex dtree) > 0)
    );
  let superset = Invariants.tag_layer_violations superset in
  let superset = Trim.Default.trim superset in
  let insn_risg = superset.insn_risg in
  let num_decisions = List.length 
      (Superset_risg.G.succ insn_risg tail_addr) in
  (* TODO make sure that the extensions were removed *)
  assert_bool "Should still have decision accessible" 
    (num_decisions = 2)

let construct_branch insn_map insn_risg branch_at incr = 
  let left = Addr.(branch_at ++ incr) in
  let junk_data = String.create incr in
  let left_mem = create_memory arch left junk_data |> ok_exn in
  let insn_map = Map.add insn_map left (left_mem, None) in
  let right = Addr.(left ++ incr) in
  let right_mem = create_memory arch right junk_data |> ok_exn in
  let insn_map = Map.add insn_map right (right_mem, None) in
  let rejoin = Addr.(right ++ incr) in
  let rejoin_mem = create_memory arch rejoin junk_data |> ok_exn in
  let insn_map = Map.add insn_map rejoin (rejoin_mem, None) in
  Superset_risg.G.add_edge insn_risg left branch_at;
  Superset_risg.G.add_edge insn_risg right branch_at;
  Superset_risg.G.add_edge insn_risg rejoin right;
  Superset_risg.G.add_edge insn_risg rejoin left;
  insn_map, insn_risg

let test_branch_recognition test_ctxt =
  let addr_size= Size.in_bits @@ Arch.addr_size arch in
  let tail_addr = Addr.of_int addr_size 50 in
  let insn_map, insn_risg = init () in
  let insn_map, insn_risg = 
    construct_branch insn_map insn_risg tail_addr 2 in
  let superset = Superset.create ~insn_risg arch insn_map in
  let branches = Grammar.identify_branches superset in
  let msg = sprintf 
      "expect branches to be detected! was %d"
      Hash_set.(length branches) in
  assert_bool msg (Hash_set.(length branches) = 1);
  let msg = "expect exact branch addr to be detected!" in
  assert_bool msg (Hash_set.(mem branches tail_addr));
  ()

(* TODO *)
(* Suppose a layer is multiple instructions long. Between a choice *)
(* and the next ancestor tail, all instructions up and including the *)
(* tail should be in the delta *)
let test_layer_delta_calculation test_ctxt = ()

(* TODO *)
(* A graph that is not strictly a tree has two explorations from a *)
(* given node in order to strike all nodes. There are two entries, *)
(* but the second entry to be explored has a few edges before *)
(* rejoining with the other path. At each node, check to know that a *)
(* node hasn't already been visited in the pre function. If so, then *)
(* remove the edge between those two. *)
let test_dfs_redundancy_elimination test_ctxt = ()

let test_dfs_iter_order test_ctxt = 
  let _, insn_risg = init () in
  let addr_size= Size.in_bits @@ Arch.addr_size arch in
  let start = Addr.of_int addr_size 40 in
  Superset_risg.G.add_edge insn_risg start Addr.(succ start);
  Superset_risg.G.add_edge insn_risg start Addr.(start ++ 2);
  let visit_order = ref [] in
  Superset_risg.Dfs.prefix_component
    (fun v -> visit_order := v :: !visit_order) 
    insn_risg start;
  visit_order := List.rev !visit_order;
  let msg = sprintf "expected addr %s to be first, was %s" 
      Addr.(to_string start)
      (List.to_string ~f:Addr.to_string !visit_order) in
  match !visit_order with 
  | first :: _ -> 
    assert_equal ~msg first start
  | _ -> assert_bool msg false 

(* TODO *)
(* This test is intended to be very much alike the entry *)
(* conflict test, differing by the fact that it will occur inline of *)
(* the insn_isg. The overlay is a scenario in which a discrete *)
(* control flow appears within another. We want to be certain that *)
(* the decision tree set incorporates considerations for picking *)
(* the overlay control flow *)
let test_overlay_construction test_ctxt = ()

(* conflicts should include both the instruction at a data address *)
(* and the instruction whose body it is inside. *)
let test_find_all_conflicts test_ctxt =
  let insn_map, insn_risg = init () in
  let arch = Arch.(`x86) in
  let addr_size= Size.in_bits @@ Arch.addr_size arch in
  let tail_addr = Addr.of_int addr_size 50 in
  let num_conflicts = 2 in
  let insn_map, insn_risg =
    construct_tail_conflict insn_map insn_risg tail_addr num_conflicts in
  let conflicts = Superset_risg.find_all_conflicts insn_map in
  let msg = sprintf "expect %d conflicts" num_conflicts in
  assert_equal ~msg num_conflicts Set.(length conflicts)


(* Establish the idempotency or addition of edges. *)
let test_graph_edge_behavior test_ctxt =
  let _, insn_risg = init () in
  let addr_size= Size.in_bits @@ Arch.addr_size arch in
  let start = Addr.of_int addr_size 50 in
  Superset_risg.G.add_edge insn_risg start Addr.(succ start);
  Superset_risg.G.add_edge insn_risg start Addr.(succ start);
  Superset_risg.G.add_edge insn_risg start Addr.(succ start);
  let edges = Superset_risg.G.find_all_edges 
      insn_risg start Addr.(succ start) in
  let msg = "expect single edge between nodes" in
  assert_equal ~msg List.(length edges) 1



let () =
  let suite = 
    "suite">:::
    [
      "test_hits_every_byte" >:: test_hits_every_byte;
      "test_trim" >:: test_trim;
      "test_trims_invalid_jump" >:: test_trims_invalid_jump;
      "test_addr_map" >:: test_addr_map;
      "test_insn_isg" >:: test_insn_isg;
      "test_consistent_superset" >:: test_consistent_superset;
      "test_construct_entry_conflict" >:: test_construct_entry_conflict;
      "test_tail_construction" >:: test_tail_construction;
      "test_tails_of_conflicts" >:: test_tails_of_conflicts;
      "test_extenuating_tail_competitors" >:: test_extenuating_tail_competitors;
      "test_decision_tree_of_entries" >:: test_decision_tree_of_entries;
      "test_loop_scc" >:: test_loop_scc;
      "test_scc" >:: test_scc;
      "test_find_conflicts" >:: test_find_conflicts;
      "test_is_option" >:: test_is_option;
      "test_trim_scc" >:: test_trim_scc;
      "test_topological_revisit" >:: test_topological_revisit;
      "test_cross_layer_pruning" >:: test_cross_layer_pruning;
      "test_calculate_delta" >:: test_calculate_delta;
      "test_extended_cross_layer_pruning" >:: test_extended_cross_layer_pruning;
      "test_branch_recognition" >:: test_branch_recognition;
      "test_layer_delta_calculation" >:: test_layer_delta_calculation;
      "test_dfs_iter_order" >:: test_dfs_iter_order;
      "test_overlay_construction" >:: test_overlay_construction;
      "test_find_all_conflicts" >:: test_find_all_conflicts;
      "test_graph_edge_behavior" >:: test_graph_edge_behavior;
    ] in
  run_test_tt_main suite
;;
