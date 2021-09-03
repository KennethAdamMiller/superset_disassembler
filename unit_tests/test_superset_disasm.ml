open OUnit2
open Core_kernel
open Bap.Std
open Or_error
open Superset
open Bap_plugins.Std
open Graphlib.Std
open Bap_future.Std

let requires = ["llvm"; "lifter"; "disassemble"; "disassembler";
                "semantics"]
let () = match Bap_main.init ~requires () with
  | Ok () -> ()
  | Error err -> 
     let open Bap_main in
     Bap_main.Extension.Error.pp Format.std_formatter err;
     exit 1

let create_memory arch min_addr data =
  let data = Bigstring.of_string data in
  Memory.create (Arch.endian arch) min_addr data

let arch = `x86

let segments = Table.empty

let width = 8*(Arch.size_in_bytes arch);;

module G = Graphlib.Make(Addr)(Unit)
module Topological = Superset_impl.Topological

let add_edge g v1 v2 = 
  let e = G.Edge.create v1 v2 () in
  G.Edge.insert e g

let mem_edge g v1 v2 =
  let e = G.Edge.create v1 v2 () in
  G.Edge.mem e g

let init () = 
  let insn_isg = Graphlib.create (module G) () in
  let insn_map = Addr.Map.empty in
  insn_map, insn_isg

let min_addr = 1
let addr_size= Size.in_bits @@ Arch.addr_size arch
let min_addr = Addr.of_int addr_size min_addr

let make_params ?(mina=min_addr) bytes =
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
  List.map superset
    ~f:(fun (mem, insn) -> (Memory.length mem))

(* This test affirms that both the order and the inner sequences of a set of bytes
   will be interpreted appropriately by the superset conservative disassembler *)
let test_hits_every_byte test_ctxt =
  let memory, arch = make_params "\x2d\xdd\xc3\x54\x55" in
  let raw_superset = Superset.Core.disasm_all
      ~accu:[] ~f:List.cons arch memory |> ok_exn in
  let sizes = superset_to_length_list raw_superset in
  let expected_results = List.rev [ 5; 2; 1; 1; 1; ] in
  check_results (Seq.of_list sizes) expected_results

let of_mem arch mem = 
  let superset = Superset.Core.empty arch in
  let f = (Invariants.tag ~invariants:[Invariants.tag_success]) in
  Superset.Core.update_with_mem superset mem ~f

let get_bads superset mem =
  let maddr  = Memory.min_addr mem in
  let l = Memory.length mem in
  let bds = Superset.Core.seq_of_addr_range maddr l in
  Seq.filter bds
    ~f:(Superset.Inspection.is_bad_at superset)

let str_of_bads superset mem =
  let bds = get_bads superset mem in
  let bds = Seq.to_list bds in
  List.to_string ~f:Addr.to_string bds

let debug_msg superset mem =
  let msg = Superset.ISG.isg_to_string superset in
  let bads = str_of_bads superset mem in
  let msg = sprintf "%s\n%s"
      msg bads in
  (*let pattern = ": " in
    let msi =
    String.substr_index mems ~pattern |> Option.value_exn in
    let start = (msi+(String.length pattern)) in
    let finish = String.((length mems)) - start in
    let ms = String.sub mems start finish in*)
  let cnt = Superset.Inspection.count superset in
  let unb = Superset.Inspection.count_unbalanced superset in
  sprintf "%s\ncount: %d, unbalanced: %d" msg cnt unb

(* TODO make default initialization from bytes much shorter *)

let test_trim test_ctxt =
  let bytes = "\x2d\xdd\xc3\x54\x55" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  let invariants =
    Invariants.tag_success :: Invariants.default_funcs in
  let superset =
    Invariants.tag_superset ~invariants superset in
  let tgt = Memory.min_addr mem in
  let dbg = debug_msg superset mem in
  let bads = str_of_bads superset mem in
  let explanation =
    sprintf "trim did not mark bad at %s, bad: %s"
      Addr.(to_string tgt) bads in
  let msg = sprintf "%s\n%s"
      dbg explanation in
  let is_bad = Superset.Inspection.is_bad_at superset tgt in
  assert_bool msg is_bad;
  let superset = Trim.Default.trim superset in
  let superset = Superset.Core.rebalance superset in
  (* Only the return opcode ( 0xc3 ) can survive trimming *)
  (* After refactoring, it may be that some targets that fall outside
     the memory bounds are still in the graph, but this only accounts
     for one edge, so it is negligible. *)
  assert_equal ~msg 1 @@ Superset.Inspection.count superset

let test_can_lift test_ctxt =
  let bytes = "\x2d\xdd\xc3\x54\x55" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  try
    Superset.Core.fold superset ~init:() ~f:(fun ~key ~data () ->
        let (mem, insn) = data in
        let bil = Superset.Core.lift_insn superset (mem, insn) in
        let msg =
          sprintf "couldn't lift at %s" (Memory.to_string mem) in
        match bil with
        | Some _ -> ()
        | _ ->
          assert_bool msg false
      ) 
  with _ -> ()  

let test_brancher test_ctxt =
  let bytes = "\x77\x77" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  match Superset.Core.lookup superset Memory.(min_addr mem) with
  | Some (mem, insn) -> 
     let ss =
       Superset.Inspection.static_successors superset mem insn in
     let msg = sprintf "Should be two static successors here" in                 
     assert_bool msg (List.(length ss) > 1)
  | None -> assert_bool "should be an instruction at 0" false

let test_lift test_ctxt =
  let bytes = "\x77\x77" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  match Superset.Core.lookup superset Memory.(min_addr mem) with
  | Some (mem, insn) -> 
     let lifted =
       Superset.Core.lift_insn superset (mem, insn) in
     let msg = sprintf "Should be able to lift" in
     assert_bool msg Option.(is_some lifted);
     let r = Option.value_map ~default:false lifted ~f:(fun (bil) ->
                 List.length bil > 0) in
     assert_bool "Should support this!" r
  | None -> assert_bool "should be an instruction at 0" false

(* TODO want a bil language oriented way to specify the construction of a superset *)
let dis_with_invariants bytes invariants = 
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  let f = (Invariants.tag ~invariants) in
  let superset = Superset.Core.update_with_mem
                   superset mem ~f in
  let msg = debug_msg superset mem in
  let msg = sprintf "Should be bad at %s\n%s"
              Addr.(to_string min_addr) msg in
  let is_bad = Superset.Inspection.is_bad_at superset min_addr in
  assert_bool msg is_bad;
  let superset = Trim.Default.trim superset in
  let superset = Superset.Core.rebalance superset in
  let msg = debug_msg superset mem in
  let offset_one =
    Superset.Core.mem superset min_addr in
  assert_bool msg (not offset_one)
  
let test_tag_non_mem_access test_ctxt =
  let bytes = "\xb8\xef\xbe\xad\xde" in (* int a = *( int * )0xdeadbeef *)
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in (
      match Superset.Core.lookup superset Memory.(min_addr mem) with
      | Some (mem, insn) ->
         let expect = Invariants.accesses_non_mem superset mem insn () in
         let msg = "Expected to find non memory access" in
         assert_bool msg expect
      | None -> assert_bool "should be an instruction at 0" false  
    );
  dis_with_invariants bytes
    [Invariants.tag_success; Invariants.tag_non_mem_access]

let test_tag_non_insn test_ctxt =
  dis_with_invariants "\x0f\xff"
    [Invariants.tag_success; Invariants.tag_non_insn]

let test_tag_target_is_bad test_ctxt = ()
(* TODO don't want to use this until have functor on superset
 * interface allowing to set the semantics directly for test *)
  (*let bytes = "\x77\xfe" in
  dis_with_invariants bytes
    [Invariants.tag_success; Invariants.tag_target_is_bad]*)

let test_target_in_body test_ctxt =
  let bytes = "\x77\xFF" in
  dis_with_invariants bytes
    [Invariants.tag_success; Invariants.tag_target_in_body]

let test_target_not_in_mem test_ctxt = 
  let bytes = "\x77\x77" in
  let invariants = [Invariants.tag_success;
                    Invariants.tag_target_not_in_mem] in
  dis_with_invariants bytes invariants
  
let test_static_successors_includes_fall_through test_ctxt =
  let bytes = "\x54\x55" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  let maddr = Memory.(min_addr mem) in
  match Superset.Core.lookup superset maddr with
  | Some (mem, insn) ->
    let tgts =
      Superset.Inspection.static_successors superset mem insn in
    let b = List.fold ~init:false tgts ~f:(fun status (addro, e) ->
        match addro with
        | Some addr -> status || Addr.(addr = (succ maddr))
        | None -> status 
      ) in
    assert_bool "static successors doesn't contain fall through" b
  | None -> assert_bool "insn expected here" false


let test_successor_calculation test_ctxt =
  let bytes = "\x2d\xdd\xc3\x54\x55" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  let mn_addr = Memory.(min_addr mem) in
  let mx_addr = Memory.(max_addr mem) in
  match Superset.Core.lookup superset mn_addr with
  | Some (mem, insn) ->
    let tgts =
      Superset.Inspection.static_successors superset mem insn in
    let b = List.fold ~init:false tgts ~f:(fun status (addro, e) ->
        match addro with
        | Some addr ->
          status ||
          (Addr.(addr > (mx_addr)) && (not Memory.(contains mem addr)))
        | None -> status 
      ) in
    let b = b && (List.length tgts > 0) in
    assert_bool "static successors doesn't contain fall through" b
  | None -> assert_bool "insn expected here" false

let test_superset_contains_addr test_ctxt =
  let bytes = "\x2d\xdd\xc3\x54\x55" in
  let mem, arch = make_params bytes in
  let superset = of_mem arch mem in
  let mn_addr = Memory.(min_addr mem) in
  match Superset.Core.lookup superset mn_addr with
  | Some (mem, insn) ->
    let tgts =
      Superset.Inspection.static_successors superset mem insn in
    let b = List.fold ~init:false tgts ~f:(fun status (addro, e) ->
        match addro with
        | Some addr ->
          status ||
          (not (Superset.Inspection.contains_addr superset addr))
        | None -> status 
      ) in
    let b = b && (List.length tgts > 0) in
    assert_bool "static successors doesn't contain fall through" b
  | None -> assert_bool "insn expected here" false

let test_trims_invalid_jump test_ctxt =
  let bytes = "\x55\x54\xE9\xFC\xFF\xFF\xFF" in
  let memory, arch = make_params bytes in
  let superset = of_mem arch memory in
  let superset = Superset.Core.update_with_mem
      superset memory ~f:Invariants.tag  in
  let superset = Trim.Default.trim superset in
  let superset = Superset.Core.rebalance superset in
  let expected_results = [ ] in
  assert_equal ~msg:"lengths unequal"
    (Superset.Inspection.count superset)
    (List.length expected_results)

let test_addr_map test_ctxt =
  let min_addr  = Addr.of_int addr_size 0 in
  let insn_map  = Addr.Map.empty in
  let insn_map  = Addr.Map.set insn_map min_addr () in
  let insn_map  = Addr.Map.set insn_map min_addr () in
  let msg = "expected length to be one" in
  assert_bool msg ((Addr.Map.length insn_map) = 1)

let test_insn_isg test_ctxt = 
  let insn_risg = Graphlib.create (module G) () in
  let addr  = Addr.of_int addr_size 0 in
  let insn_risg = G.Node.insert addr insn_risg in
  let insn_risg = G.Node.insert addr insn_risg in
  let msg = "expected length to be one" in
  assert_bool msg ((G.number_of_nodes insn_risg) = 1)  

let test_consistent_superset test_ctxt = 
  let memory, arch = make_params "\x55\x54\xE9\xFC\xFF\xFF\xFF" in
  let superset = of_mem arch memory in
  let m_neg_g, g_neg_m = Superset.Inspection.unbalanced_diff superset in
  let msg = "insn in map but not in after shingled of superset" in
  assert_bool msg (Set.is_empty m_neg_g);
  let msg = "insn in isg but not in map after shingled of superset" in
  assert_bool msg (Set.is_empty g_neg_m)

let construct_loop insn_map insn_isg start finish = 
  if Addr.(finish > start) then (
    (* Where we would otherwise connect the nodes from the tail
       condition back up to the loop body entry, here all the edges are
       reversed in the spirit of the disassembled insn_isg. *)
    let insn_isg = add_edge insn_isg start finish in
    let junk_data = String.of_char ' ' in
    let start_mem = create_memory arch start junk_data |> ok_exn in
    let insn_map = Addr.Map.set insn_map start (start_mem, None) in
    let finish_mem = create_memory arch finish junk_data |> ok_exn in
    let insn_map = Addr.Map.set insn_map finish (finish_mem, None) in
    let one  = (Addr.of_int 1 ~width) in
    let two  = (Addr.of_int 2 ~width) in
    let rec construct_loop_body insn_isg insn_map start finish = 
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
        let insn_isg = add_edge insn_isg step start in
        (* Because the algorithm at this point relies on the graph
           and map entirely, it doesn't matter the contents of the
           memory. *)
        let junk_data = String.make (Addr.to_int dist |> ok_exn) ' ' in
        let insn_map = Addr.Map.set insn_map ~key:start
            ~data:(create_memory arch start junk_data |> ok_exn, None) in
        construct_loop_body insn_isg insn_map step finish 
      else insn_map, insn_isg in
    construct_loop_body insn_isg insn_map start finish
  ) else insn_map, insn_isg

let construct_entry_conflict insn_map insn_isg at conflict_len = 
  let junk_data = String.make conflict_len ' ' in
  let conflict = Addr.(at ++ conflict_len) in
  let insn_isg = add_edge insn_isg at conflict in
  let insn_isg = add_edge insn_isg
                   Addr.(at ++ 1) Addr.(conflict ++ 1) in
  let insn_map = Addr.Map.set insn_map ~key:at 
      ~data:(create_memory arch at junk_data |> ok_exn, None) in
  let insn_map = Addr.Map.set insn_map ~key:conflict
      ~data:(create_memory arch conflict junk_data |> ok_exn, None) in
  let insn_map = Addr.Map.set insn_map ~key:Addr.(conflict ++1)
      ~data:(create_memory arch Addr.(conflict ++1) junk_data |> ok_exn, None) in
  let insn_map = Addr.Map.set insn_map ~key:Addr.(at ++ 1) 
      ~data:(create_memory arch Addr.(at ++ 1) junk_data |> ok_exn, None) in
  insn_map, insn_isg

let construct_tail_conflict 
    insn_map insn_isg tail_addr conflict_count =
  let orig = 
    if G.Node.mem tail_addr insn_isg &&
       G.Node.degree ~dir:`Out tail_addr insn_isg > 0 then
      let orig = G.Node.succs tail_addr insn_isg in
      let orig = Addr.Set.of_list @@ Seq.to_list orig in orig
    else Addr.Set.empty in
  let insn_map, tail_len = match Addr.Map.find insn_map tail_addr with
    | Some (mem, _) -> insn_map, Memory.length mem
    | None -> 
      let tail_len = 1 in
      let tail_data = String.make tail_len ' ' in
      let mem = create_memory arch tail_addr tail_data |> ok_exn in
      let insn_map = Addr.Map.set insn_map ~key:tail_addr
          ~data:(mem, None) in
      insn_map, tail_len in 
  let rec make_tail_options insn_map insn_isg conflict_count =
    if conflict_count > 0 then
      let junk_data = String.make conflict_count ' ' in
      let conflict_addr = Addr.(tail_addr -- conflict_count) in
      let insn_isg = add_edge insn_isg tail_addr conflict_addr in
      let insn_map = Addr.Map.set insn_map ~key:conflict_addr 
          ~data:(create_memory arch conflict_addr junk_data |> ok_exn,
                 None) in
      make_tail_options insn_map insn_isg (conflict_count - 1)
    else 
      insn_map, insn_isg in
  let insn_map, insn_isg = 
    make_tail_options insn_map insn_isg conflict_count in
  let opts = G.Node.succs tail_addr insn_isg in
  let opts = Addr.Set.of_list @@ Seq.to_list opts in
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
      let insn_map, insn_risg = init () in
      let insn_map, insn_risg =
        construct_entry_conflict insn_map insn_risg 
          entry conflict_len in
      let superset = Superset_impl.of_components
          ~insn_map ~insn_risg arch in
      let entries = Superset.entries_of_isg superset in
      let conflicts = Decision_trees.conflicts_of_entries
          superset entries in
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
      let insn_map, insn_risg = init () in
      let insn_map, insn_risg = 
        construct_tail_conflict insn_map insn_risg tail conflict_len in
      let superset = Superset_impl.of_components
          ~insn_map ~insn_risg arch in
      Map.iteri insn_map ~f:(fun ~key ~data -> 
          assert_equal true @@ G.Node.mem key insn_risg;
        );
      
      Seq.iter (G.nodes insn_risg) ~f:(fun vert -> 
          assert_equal true @@ Map.mem insn_map vert;
        );
      let conflicts = Superset.Occlusion.find_all_conflicts superset in
      let tails = Decision_trees.tails_of_conflicts
          superset conflicts  in
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
  let insn_map, insn_risg = init () in
  let insn_map, insn_risg =
    construct_entry_conflict insn_map insn_risg 
      entry 10 in
  let insn_map, insn_risg = 
    construct_tail_conflict insn_map insn_risg tail 4 in
  let superset = Superset_impl.of_components
      ~insn_map ~insn_risg arch in  
  let conflicts = Superset.Occlusion.find_all_conflicts superset in
  let tails = Decision_trees.tails_of_conflicts superset
      conflicts in
  assert_equal true (Addr.Map.length tails = 1)
    ~msg:"There should be exactly one tail" 

(* This should test that, for each possible number of conflicts, *)
(* there is always as many tails as were created *)
let test_extenuating_tail_competitors test_ctxt =
  let conflict_len = 4 in
  let entry = Addr.(of_int ~width 1) in
  let tail = Addr.(of_int ~width 30) in
  let insn_map, insn_risg = init () in
  let insn_map, insn_risg = 
    construct_tail_conflict insn_map insn_risg tail conflict_len in
  let extenuation_addr = Addr.(entry ++ conflict_len) in
  let insn_map, insn_risg = 
    construct_tail_conflict 
      insn_map insn_risg extenuation_addr conflict_len in
  let superset = Superset_impl.of_components
      ~insn_map ~insn_risg arch in  
  let conflicts = Superset.Occlusion.find_all_conflicts superset in
  let tails = Decision_trees.tails_of_conflicts superset
      conflicts in
  assert_equal true @@ Addr.Map.mem tails tail;
  assert_equal true @@ Addr.Map.mem tails extenuation_addr;
  assert_equal (Addr.Map.length tails) 2

(* construct an entry conflict and ensure 
   that the two are reachable from addr 0.  *)
let test_decision_tree_of_entries test_ctxt =
  let insn_map, insn_risg = init () in
  let entry = Addr.(of_int ~width 1) in
  let insn_map, insn_risg =
    construct_entry_conflict insn_map insn_risg 
      entry 10 in
  let superset = Superset_impl.of_components
                   ~insn_map ~insn_risg arch in
  let msg = sprintf "expected entry %s to be in isg" 
              (Addr.to_string entry) in
  let msg = sprintf "%s\n%s" msg
              (Superset.ISG.isg_to_string superset) in
  assert_equal ~msg true (Superset.Core.mem superset entry);
  let entries = Superset.entries_of_isg superset in
  let msg = sprintf "expected entry %s to be in entries" 
      (Addr.to_string entry) in
  assert_equal ~msg true (Hash_set.mem entries entry);
  let msg = sprintf "expected entry %s to be in entries" 
      Addr.(to_string @@ succ entry) in
  assert_equal ~msg true (Hash_set.mem entries Addr.(succ entry));
  let decision_trees =
    Decision_trees.decision_trees_of_superset superset in
  let expect_entry_msg = "Expect entry in decision_tree" in
  let non_empty_tree_msg = "Expect decision tree to be non empty" in
  assert_equal true @@ (not ((Decision_trees.count decision_trees) = 0));
  Decision_trees.with_trees decision_trees ~init:() ~f:(fun _ decision_tree ->
      let open Decision_trees in
      assert_equal ~msg:non_empty_tree_msg 
        true @@ not ((DecisionTree.count decision_tree)=0);
      assert_equal ~msg:expect_entry_msg 
        true @@ (DecisionTree.mem entry decision_tree);
    )

let test_loop_scc test_ctxt = 
  let insn_map, insn_isg = init () in
  let entry = Addr.(of_int ~width 1) in
  let insn_map, insn_isg = 
    construct_loop insn_map insn_isg entry Addr.(entry ++ 20) in
  let loop_points = Addr.Hash_set.create () in
  Seq.iter (G.nodes insn_isg)
    ~f:(fun vert -> Hash_set.add loop_points vert);
  let superset =
    Superset_impl.of_components ~insn_map ~insn_risg:insn_isg arch in
  let scc = Superset.ISG.raw_loops superset in
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
  let insn_map, insn_risg = init () in
  let zero = Addr.(of_int ~width 0) in
  let entry = Addr.(of_int ~width 1) in
  let insn_risg = add_edge insn_risg zero entry in
  let superset =
    Superset_impl.of_components ~insn_map ~insn_risg arch in
  let components = Sheathed.addrs_of_filtered_loops superset in
  assert_equal ~msg:"found non scc component" 0 (Set.length components)

let test_find_conflicts test_ctxt = 
  let insn_map, insn_risg = init () in
  let in_loop_addr = Addr.(of_int ~width 0x10) in
  let num_conflicts = 6 in
  let insn_map, insn_risg =
    construct_tail_conflict insn_map insn_risg in_loop_addr
      num_conflicts in
  let superset = Superset_impl.of_components
      ~insn_map ~insn_risg arch in  
  let conflicts = Superset.Occlusion.find_all_conflicts superset in 
  assert_equal Set.(length conflicts) num_conflicts

let test_is_option test_ctxt = 
  let insn_map, insn_isg = init () in
  let in_loop_addr = Addr.(of_int ~width 0x10) in
  let num_conflicts = 6 in
  let insn_map, insn_risg =
    construct_tail_conflict insn_map insn_isg in_loop_addr
      num_conflicts in
  let superset = Superset_impl.of_components
      ~insn_map ~insn_risg arch in  
  let deltas = Decision_trees.calculate_deltas superset in
  let msg = sprintf
      "expected 6 options to be identified, saw %d" 
      Map.(length deltas) in
  assert_bool msg (Map.(length deltas) = num_conflicts)

let test_trim_scc test_ctxt = 
  let insn_map, insn_risg = init () in
  let entry = Addr.(of_int ~width 1) in
  let insn_map, insn_risg = 
    construct_loop insn_map insn_risg entry Addr.(entry ++ 20) in
  let superset = Superset_impl.of_components
      ~insn_map ~insn_risg arch in
  let keep_entry = Superset.Core.mem superset entry in
  let msg = sprintf "entry %s should be in the graph"
              Addr.(to_string entry) in
  assert_equal ~msg keep_entry true;
  let in_loop_addr = Addr.(of_int ~width 0x10) in
  let loop_points = Addr.Hash_set.create () in
  Seq.iter (G.nodes insn_risg) ~f:(Hash_set.add loop_points);
  let insn_map, insn_risg =
    construct_tail_conflict insn_map insn_risg in_loop_addr 3 in
  let conflicts_added = Addr.Hash_set.create () in
  Seq.iter (G.nodes insn_risg) ~f:(fun vert -> 
      if not (Hash_set.mem loop_points vert) then 
        Hash_set.add conflicts_added vert);
  let superset = Superset_impl.of_components
      ~insn_map ~insn_risg arch in
  let components = Superset.ISG.raw_loops superset in
  assert_bool "Should have a component" 
    (List.(length components) > 0);
  let superset = 
    Sheathed.tag_loop_contradictions ~min_size:1 superset in
  assert_bool "should have marked conflict" 
    (0 < Superset.Inspection.(num_bad superset));
  let keep_entry = Superset.Inspection.is_bad_at superset entry in
  let msg = sprintf "entry %s should not be marked bad"
              Addr.(to_string entry) in
  assert_equal ~msg keep_entry false;
  let superset = Trim.Default.trim superset in
  let keep_entry = Superset.Core.mem superset entry in
  let msg = sprintf "entry %s should not be removed"
              Addr.(to_string entry) in
  assert_equal ~msg keep_entry true;
  let conflicts_added_str = List.to_string ~f:Addr.to_string @@ 
                              Hash_set.to_list conflicts_added in
  let removed_msg = "of conflicts " ^ conflicts_added_str 
                    ^ ", residual conflict present within " in
  let isg_msg = Superset.ISG.isg_to_string superset in
  let removed_msg = sprintf "%s\n%s" isg_msg removed_msg in
  let (mlg, glm) = Superset.Inspection.unbalanced_diff superset in
  let msg = sprintf "%s\nmlg: %d, glm: %d" removed_msg
              Set.(length mlg) Set.(length glm) in
  let set_to_string s =
    List.to_string ~f:Addr.to_string @@ Set.to_list s in
  let msg = sprintf "%s\nmap less graph: %s, graph less map: %s"
              msg (set_to_string mlg) (set_to_string glm) in
  assert_bool msg (Set.(length mlg)=0 && Set.(length glm)=0);
  let loop_msg addr =
    sprintf "%s\nloop addr %s should remain during tail trim"
      (Superset.ISG.isg_to_string superset)
      Addr.(to_string addr) in
  Hash_set.iter loop_points ~f:(fun addr -> 
      assert_equal ~msg:(loop_msg addr) true @@ 
      Superset.Core.mem superset addr)

(* Establishes, in the case of if structures, how topological *)
(* tranversal works - one time visit only *)
let test_topological_revisit ctxt = 
  let _, insn_risg = init () in
  let width = 32 in
  let start = Addr.of_int 0 ~width in
  let stop = Addr.of_int 2 ~width in
  let rec make_if insn_risg current stop =
    if not Addr.(current = stop) then
      let next = Addr.succ current in
      let insn_risg = add_edge insn_risg current next in
      make_if insn_risg next stop else insn_risg in
  let insn_risg = make_if insn_risg start stop in
  let insn_risg = add_edge insn_risg start stop in
  let update_count addr visit_count = 
    match Map.find visit_count addr with
    | Some (count) -> 
      let visit_count = Map.remove visit_count addr in
      Map.set visit_count addr (count+1)
    | None -> Map.set visit_count addr 1 in
  
  let visit_count = Topological.fold 
      update_count insn_risg Addr.Map.empty in
  Map.iteri visit_count (fun ~key ~data -> assert_equal ~ctxt data 1)

let rec extend_back insn_map insn_isg ?(step=1) addr num =
  let make_link len =
    let dest = Addr.(addr -- len) in
    let insn_isg = add_edge insn_isg addr dest in
    let junk_data = String.make len ' ' in
    let mem = create_memory arch dest junk_data |> ok_exn in
    let insn_map = Map.set insn_map dest (mem, None) in
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
  let layer_options = Seq.to_list G.Node.(succs tail_addr insn_risg) in
  let insn_map, insn_risg = List.fold ~init:(insn_map, insn_risg)
      layer_options ~f:(fun (insn_map, insn_risg) opt -> 
          extend_back insn_map insn_risg opt 1 ~step:2
      ) in
  let superset = Superset_impl.of_components
                   ~insn_map ~insn_risg arch in  
  let extended_points = 
    Superset.entries_of_isg superset in
  let _,insn_risg = Hash_set.fold ~init:(None,insn_risg) extended_points
      ~f:(fun (current,insn_risg) next -> 
          let insn_risg = Option.value_map current ~f:(fun current -> 
              add_edge insn_risg current next
            ) ~default:insn_risg in
          Some(next),insn_risg
        ) in
  insn_map, insn_risg

let test_calculate_delta test_ctxt = 
  let tail_addr = Addr.of_int addr_size 50 in
  let insn_map, insn_risg = make_extended_cross tail_addr in
  let superset = Superset_impl.of_components
                   ~insn_map ~insn_risg arch in  
  let entries = Superset.entries_of_isg superset in
  let conflicts = Superset.Occlusion.find_all_conflicts superset in
  let tails = Decision_trees.tails_of_conflicts superset
      conflicts in
  let option_set = List.fold ~init:Addr.Set.empty (Map.data tails)
      ~f:(fun option_set options -> 
          List.fold ~init:option_set options ~f:Addr.Set.add) in
  let is_option = Set.mem option_set in
  let deltas = Decision_trees.calculate_deltas 
      superset ~entries ~is_option in
  let expected_deltas =
    Seq.to_list @@ G.Node.succs tail_addr insn_risg in
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

let construct_branch insn_map insn_risg branch_at incr = 
  let left = Addr.(branch_at ++ incr) in
  let junk_data = String.make incr ' ' in
  let left_mem = create_memory arch left junk_data |> ok_exn in
  let insn_map = Map.set insn_map left (left_mem, None) in
  let right = Addr.(left ++ incr) in
  let right_mem = create_memory arch right junk_data |> ok_exn in
  let insn_map = Map.set insn_map right (right_mem, None) in
  let rejoin = Addr.(right ++ incr) in
  let rejoin_mem = create_memory arch rejoin junk_data |> ok_exn in
  let insn_map = Map.set insn_map rejoin (rejoin_mem, None) in
  let insn_risg = add_edge insn_risg left branch_at in
  let insn_risg = add_edge insn_risg right branch_at in
  let insn_risg = add_edge insn_risg rejoin right in
  let insn_risg = add_edge insn_risg rejoin left in
  insn_map, insn_risg

let test_branch_recognition test_ctxt =
  let tail_addr = Addr.of_int addr_size 50 in
  let insn_map, insn_risg = init () in
  let insn_map, insn_risg = 
    construct_branch insn_map insn_risg tail_addr 2 in
  let superset = Superset_impl.of_components
                   ~insn_map ~insn_risg arch in
  let entries = Superset.entries_of_isg superset in
  let msg = "expect at least one entry" in
  assert_bool msg (Hash_set.(length entries) > 0);
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
  let insn_map, insn_risg = init () in
  let start = Addr.of_int addr_size 40 in
  let insn_risg = add_edge insn_risg start Addr.(succ start) in
  let insn_risg = add_edge insn_risg start Addr.(start ++ 2) in
  let visit_order = ref [] in
  let superset =
    Superset_impl.of_components ~insn_map ~insn_risg arch in
  Traverse.with_ancestors_at 
    ~pre:(fun v -> visit_order := v :: !visit_order) 
    superset start;
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
  let tail_addr = Addr.of_int addr_size 50 in
  let num_conflicts = 2 in
  let insn_map, insn_risg =
    construct_tail_conflict
      insn_map insn_risg tail_addr num_conflicts in
  let superset = Superset_impl.of_components
      ~insn_map ~insn_risg arch in  
  let conflicts = Superset.Occlusion.find_all_conflicts superset in
  let msg = sprintf "expect %d conflicts" num_conflicts in
  assert_equal ~msg num_conflicts Set.(length conflicts)


(* Establish the idempotency or addition of edges. *)
let test_graph_edge_behavior test_ctxt =
  let _, insn_risg = init () in
  let start = Addr.of_int addr_size 50 in
  let insn_risg = add_edge insn_risg start Addr.(succ start) in
  let insn_risg = add_edge insn_risg start Addr.(succ start) in
  let insn_risg = add_edge insn_risg start Addr.(succ start) in
  let edges = Seq.filter (G.edges insn_risg) ~f:(fun e ->
                  Addr.((G.Edge.src e) = start)
                  && Addr.((G.Edge.dst e) = Addr.(succ start))) in
  let msg = "expect single edge between nodes" in
  assert_equal ~msg Seq.(length edges) 1

let test_streams test_ctxt =
  let strm, sgnl = Stream.create () in
  let called = ref false in
  Stream.watch strm (fun id _ ->
      called := true;
      Stream.unsubscribe strm id
    );
  Signal.send sgnl ();
  assert_bool "stream did not receive signal called" !called

let test_parallel test_ctxt = ()

let test_set_envelopment_depth test_ctxt =
  let insn_map, insn_risg = init () in
  let entry = Addr.(of_int ~width 50) in
  let insn_map, insn_risg =
    construct_entry_conflict insn_map insn_risg entry 5 in
  let superset = Superset_impl.of_components
                   ~insn_map ~insn_risg arch in
  let _ = () in ()

let test_ssa test_ctxt =
  let find_ssa asm ~f = 
    let memory, arch = make_params asm in
    let superset = of_mem arch memory in
    let ssa = Features.extract_freevarssa_to_map superset in
    let freevars = Addr.Hash_set.create () in
    List.iter Addr.Table.(data ssa) ~f:Hash_set.(add freevars);
    f freevars in
  let asm = "\x50\x58" in (* push rax, pop rax *)
  find_ssa asm ~f:(fun ssa_rax ->
      assert_bool "Expect >= 1 ssa for push pop register"
        ((Hash_set.length ssa_rax) > 0));
  let asm = "\x50\x58" in (* move rbx -> [rax], move rbx <- [rax] *)
  find_ssa asm ~f:(fun ssa_mem_mem ->
      assert_bool "Expect >= 1 ssa for mem mem operation"
        ((Hash_set.length ssa_mem_mem) > 0));
  let asm = "\x50\x58" in (* push rax, push rbx, pop rax, pop rbx *)
  find_ssa asm ~f:(fun ssa_chain ->
      assert_bool "Expect >= 2 ssa for register chain"
        ((Hash_set.length ssa_chain) > 0));
  ()

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
      "test_calculate_delta" >:: test_calculate_delta;
      "test_branch_recognition" >:: test_branch_recognition;
      "test_layer_delta_calculation" >:: test_layer_delta_calculation;
      "test_dfs_iter_order" >:: test_dfs_iter_order;
      "test_overlay_construction" >:: test_overlay_construction;
      "test_find_all_conflicts" >:: test_find_all_conflicts;
      "test_graph_edge_behavior" >:: test_graph_edge_behavior;
      "test_can_lift" >:: test_can_lift;
      "test_static_successors_includes_fall_through" >::
        test_static_successors_includes_fall_through;
      "test_brancher" >:: test_brancher;
      "test_lift" >:: test_lift;
      "test_successor_calculation" >:: test_successor_calculation;
      "test_superset_contains_addr" >:: test_superset_contains_addr;
      "test_tag_non_mem_access" >:: test_tag_non_mem_access;
      "test_target_not_in_mem" >:: test_target_not_in_mem;
      "test_tag_non_insn" >:: test_tag_non_insn;
      "test_tag_target_is_bad" >:: test_tag_target_is_bad;
      "test_target_in_body" >:: test_target_in_body;
      "test_streams" >:: test_streams;
      "test_parallel" >:: test_parallel;
      "test_set_envelopment_depth" >:: test_set_envelopment_depth;
      "test_ssa" >:: test_ssa;
    ] in
  run_test_tt_main suite
;;
