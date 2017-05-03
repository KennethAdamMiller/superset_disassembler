open OUnit2
open Core_kernel.Std
open Bap.Std
open Or_error
open Common
open Bap_plugins.Std
open Superset_rcfg
open Superset

let () = Pervasives.ignore(Plugins.load ())

let arch = Option.value ~default:`x86 @@ Arch.of_string "x86_64";;

let width = 8*(Arch.size_in_bytes arch);;

let init () = 
  let insn_cfg = Superset_rcfg.G.create () in
  let insn_map = Addr.Map.empty in
  insn_map, insn_cfg

let make_params ?(min_addr=0) bytes =
  let arch = Arch.(`x86) in
  let addr_size= Size.in_bits @@ Arch.addr_size arch in
  let min_addr = Addr.of_int addr_size min_addr in
  let memory = create_memory arch min_addr bytes |> ok_exn in
  memory, arch

let check_results sizes expected_results = 
  let sizes = Seq.to_list sizes in
  (* print_endline (List.to_string sizes ~f:string_of_int);*)
  List.iter2_exn sizes expected_results
    ~f:(fun actual_size expected_size ->
        assert_equal ~msg:((List.to_string ~f:string_of_int sizes)
                           ^ (List.to_string ~f:string_of_int
                                expected_results)) actual_size
          expected_size)  

let shingles_to_length_list shingles = 
  Seq.of_list [G.nb_vertex shingles]

let superset_to_length_list superset =
  List.map superset ~f:(fun (mem, insn) -> (Memory.length mem))

(* This test affirms that both the order and the inner sequences of a set of bytes
   will be interpreted appropriately by the superset conservative disassembler *)
let test_hits_every_byte test_ctxt =
  let memory, arch = make_params "\x2d\xdd\xc3\x54\x55" in
  let shingled_disassembly = Superset.disasm
      ~accu:[] ~f:List.cons arch memory |> ok_exn in
  let sizes = superset_to_length_list shingled_disassembly in
  let expected_results = [ 5; 2; 1; 1; 1; ] in
  check_results (Seq.of_list sizes) expected_results

let test_trim test_ctxt =
  let bytes = "\x2d\xdd\xc3\x54\x55" in
  let memory, arch = make_params bytes in
  let brancher = Brancher.of_bil arch in
  let img, _ = Image.of_string ~backend:"llvm" bytes |> ok_exn in
  Pervasives.ignore (Superset.disasm
                       ~accu:[] ~f:List.cons arch memory >>| fun insns ->
                     let superset_rcfg = Superset_rcfg.rcfg_of_raw_superset
                         brancher insns in
                     let insn_map = Superset.raw_superset_to_map insns in
                     let superset = Superset.{
                         insn_map = insn_map;
                         insn_rcfg = superset_rcfg;
                         brancher = brancher;
                         img = img;
                       } in
                     let superset = Trim.trim superset in
                     (* the above is a byte sequence no compiler would produce. The
                        sheering algorithm would therefore wipe all
                        clean  *)
                     let msg = G.fold_vertex
                         (fun vert  accu -> 
                            accu ^ "\n" ^ (Addr.to_string vert))
                         superset.insn_rcfg "" in
                     assert_equal ~msg 0
                     @@ G.nb_vertex superset.insn_rcfg)

let test_retain_valid_jump test_ctxt = ()

let test_trims_invalid_jump test_ctxt =
  let bytes = "\x55\x54\xE9\xFC\xFF\xFF\xFF" in
  let memory, arch = make_params bytes in
  let img, _ = Image.of_string bytes |> ok_exn in
  let raw_superset =
    Superset.disasm ~accu:[] ~f:List.cons arch memory |> ok_exn in
  let insn_map = Superset.raw_superset_to_map raw_superset in
  let brancher = Brancher.of_bil arch in
  let superset_rcfg = Superset_rcfg.rcfg_of_raw_superset brancher raw_superset in
  let superset = Superset.{
      insn_map = insn_map;
      img = img;
      brancher = brancher;
      insn_rcfg = superset_rcfg;
    } in
  let superset = Trim.trim superset in
  let expected_results = [ ] in
  assert_equal ~msg:"lengths unequal" (G.nb_vertex superset.insn_rcfg)
    (List.length expected_results)

let test_shingles_to_map test_ctxt = ()

let test_cfg_of_shingles test_ctxt = ()

let test_addr_map test_ctxt =
  let addr_size = Size.in_bits @@ Arch.addr_size Arch.(`x86_64) in
  let min_addr  = Addr.of_int addr_size 0 in
  let insn_map  = Addr.Map.empty in
  let insn_map  = Addr.Map.add insn_map min_addr () in
  let insn_map  = Addr.Map.add insn_map min_addr () in
  let msg = "expected length to be one" in
  assert_bool msg ((Addr.Map.length insn_map) = 1)

let test_insn_cfg test_ctxt = 
  let insn_cfg = Superset_rcfg.G.create () in
  let addr_size = Size.in_bits @@ Arch.addr_size Arch.(`x86_64) in
  let addr  = Addr.of_int addr_size 0 in
  Superset_rcfg.G.add_vertex insn_cfg addr;
  Superset_rcfg.G.add_vertex insn_cfg addr;
  let msg = "expected length to be one" in
  assert_bool msg ((Superset_rcfg.G.nb_vertex insn_cfg) = 1)  

let test_shingled_of_superset test_ctxt = 
  let memory, arch = make_params "\x55\x54\xE9\xFC\xFF\xFF\xFF" in
  let brancher = Brancher.of_bil arch in
  let insns = Superset.disasm ~accu:[] ~f:List.cons arch memory
              |> ok_exn in
  let insn_cfg = Superset_rcfg.rcfg_of_raw_superset brancher insns in
  let insn_map = Superset.raw_superset_to_map insns in
  let msg = "insn in cfg but not in map after shingled of superset" in
  Superset_rcfg.G.iter_vertex (fun v -> 
      let msg = msg ^ Addr.to_string v in
      assert_bool msg (Map.mem insn_map v))
    insn_cfg;
  let msg = "insn in map but not in after shingled of superset" in
  Addr.Map.iteri insn_map (fun ~key ~data -> 
      assert_bool msg 
        (Superset_rcfg.G.mem_vertex insn_cfg key))

let test_conflicts_within_insn test_ctxt = ()

let test_find_all_conflicts test_ctxt = ()

let test_retain_cross_segment_references test_ctxt = ()


let construct_loop insn_map insn_cfg start finish = 
  if Addr.(finish > start) then (
    (* Where we would otherwise connect the nodes from the tail
       condition back up to the loop body entry, here all the edges are
       reversed in the spirit of the disassembled insn_cfg. *)
    Superset_rcfg.G.add_edge insn_cfg start finish;
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
        printf "start %s, step %s finish %s, dist %s\n" 
          (Addr.to_string start) 
          (Addr.to_string step)
          (Addr.to_string finish)
          (Addr.to_string dist);
        (* Add edge from some intermediate point between the start and
           the finish going from the calculated step toward the parameter
           start, decreasing the distance between the outermost start
           and finish. As this function executes, it creates log(dist)
           nodes, each going from finish to step (reverse as it would
           be a flow in a real binary) before finally reaching the
           start *)
        Superset_rcfg.G.add_edge insn_cfg step start;
        (* Because the algorithm at this point relies on the graph
           and map entirely, it doesn't matter the contents of the memory. *)
        let junk_data = String.create @@ (Addr.to_int dist |> ok_exn) in
        let insn_map = Addr.Map.add insn_map ~key:start
            ~data:(create_memory arch start junk_data |> ok_exn, None) in
        construct_loop_body insn_map step finish 
      else insn_map, insn_cfg in
    construct_loop_body insn_map start finish
  ) else insn_map, insn_cfg

(* TODO this needs to rejoin at a common address *)
let construct_branch insn_cfg branch_at left right = 
  Superset_rcfg.G.add_edge insn_cfg left branch_at;
  Superset_rcfg.G.add_edge insn_cfg right branch_at

let construct_entry_conflict insn_map insn_cfg at conflict_len = 
  let junk_data = String.create conflict_len in
  let conflict = Addr.(at ++ conflict_len) in
  Superset_rcfg.G.add_edge insn_cfg at conflict;
  Superset_rcfg.G.add_edge insn_cfg Addr.(at ++ 1) Addr.(conflict ++ 1);
  let insn_map = Addr.Map.add insn_map ~key:at 
      ~data:(create_memory arch at junk_data |> ok_exn, None) in
  let insn_map = Addr.Map.add insn_map ~key:conflict
      ~data:(create_memory arch conflict junk_data |> ok_exn, None) in
  let insn_map = Addr.Map.add insn_map ~key:Addr.(conflict ++1)
      ~data:(create_memory arch Addr.(conflict ++1) junk_data |> ok_exn, None) in
  let insn_map = Addr.Map.add insn_map ~key:Addr.(at ++ 1) 
      ~data:(create_memory arch Addr.(at ++ 1) junk_data |> ok_exn, None) in
  insn_map, insn_cfg

let construct_tail_conflict insn_map insn_cfg tail_addr conflict_count =
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
      Superset_rcfg.G.add_edge insn_cfg tail_addr conflict_addr;
      let insn_map = Addr.Map.add insn_map ~key:conflict_addr 
          ~data:(create_memory arch conflict_addr junk_data |> ok_exn,
                 None) in
      make_tail_options insn_map (conflict_count - 1)
    else 
      insn_map, insn_cfg in
  make_tail_options insn_map conflict_count

let test_construct_entry_conflict test_ctxt = 
  let rec test_entry_conflict_of_len entry conflict_len = 
    if conflict_len < 2 then
      () 
    else
      let insn_map, insn_cfg = init () in
      let insn_map, insn_cfg =
        construct_entry_conflict insn_map insn_cfg 
          entry conflict_len in
      let entries = Sheath_tree_set.entries_of_cfg insn_cfg in
      let conflicts = Sheath_tree_set.conflicts_of_entries
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
      let insn_map, insn_cfg = init () in
      let insn_map, insn_cfg = 
        construct_tail_conflict insn_map insn_cfg tail conflict_len in
      Map.iteri insn_map ~f:(fun ~key ~data -> 
          assert_equal true @@ Superset_rcfg.G.mem_vertex insn_cfg key;
        );
      Superset_rcfg.G.iter_vertex (fun vert -> 
          assert_equal true @@ Map.mem insn_map vert;
        ) insn_cfg;
      let entries = Sheath_tree_set.entries_of_cfg insn_cfg in
      let conflicts = Superset_rcfg.find_all_conflicts insn_map insn_cfg in
      let tails = Sheath_tree_set.tails_of_conflicts
          conflicts insn_cfg entries in
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
  let insn_map, insn_cfg = init () in
  let insn_map, insn_cfg =
    construct_entry_conflict insn_map insn_cfg 
      entry 10 in
  let insn_map, insn_cfg = 
    construct_tail_conflict insn_map insn_cfg tail 4 in
  let conflicts = Superset_rcfg.find_all_conflicts insn_map insn_cfg in
  let entries = Sheath_tree_set.entries_of_cfg insn_cfg in
  let tails = Sheath_tree_set.tails_of_conflicts
      conflicts insn_cfg entries in
  assert_equal true (Addr.Map.length tails = 1)
    ~msg:"There should be exactly one tail" 


(* This should test that, for each possible number of conflicts, *)
(* there is always as many tails as were created *)
let test_extenuating_tail_competitors test_ctxt =
  let conflict_len = 4 in
  let entry = Addr.(of_int ~width 1) in
  let tail = Addr.(of_int ~width 30) in
  let insn_map, insn_cfg = init () in
  let insn_map, insn_cfg = 
    construct_tail_conflict insn_map insn_cfg tail conflict_len in
  let extenuation_addr = Addr.(entry ++ conflict_len) in
  let insn_map, insn_cfg = 
    construct_tail_conflict 
      insn_map insn_cfg extenuation_addr conflict_len in  
  let entries = Sheath_tree_set.entries_of_cfg insn_cfg in
  let conflicts = Superset_rcfg.find_all_conflicts insn_map insn_cfg in
  let tails = Sheath_tree_set.tails_of_conflicts
      conflicts insn_cfg entries in
  assert_equal true @@ Addr.Map.mem tails tail;
  assert_equal true @@ Addr.Map.mem tails extenuation_addr;
  assert_equal (Addr.Map.length tails) 2

(* construct an entry conflict and ensure 
   that the two are reachable from addr 0.  *)
let test_decision_tree_of_entries test_ctxt =
  let insn_map, insn_cfg = init () in
  let zero = Addr.(of_int ~width 0) in
  let entry = Addr.(of_int ~width 1) in
  let insn_map, insn_cfg =
    construct_entry_conflict insn_map insn_cfg 
      entry 10 in
  let msg = sprintf "expected entry %s to be in cfg" 
      (Addr.to_string entry) in
  assert_equal ~msg true (Superset_rcfg.G.mem_vertex insn_cfg entry);
  let entries = Sheath_tree_set.entries_of_cfg insn_cfg in
  let msg = sprintf "expected entry %s to be in entries" 
      (Addr.to_string entry) in
  assert_equal ~msg true (Hash_set.mem entries entry);
  let msg = sprintf "expected entry %s to be in entries" 
      Addr.(to_string @@ succ entry) in
  assert_equal ~msg true (Hash_set.mem entries Addr.(succ entry));
  let conflicts = Superset_rcfg.find_all_conflicts insn_map insn_cfg in
  let tails = Sheath_tree_set.tails_of_conflicts
      conflicts insn_cfg entries in
  let conflicted_entries = Sheath_tree_set.conflicts_of_entries
      entries insn_map in
  let decision_trees = Sheath_tree_set.decision_tree_of_entries
      conflicted_entries tails insn_cfg in
  let expect_entry_msg = "Expect entry in decision_tree" in
  let expect_zero_msg = "Expect zero node in decision tree" in
  let non_empty_tree_msg = "Expect decision tree to be non empty" in
  assert_equal true @@ (not ((List.length decision_trees) = 0));
  List.iter decision_trees ~f:(fun decision_tree ->
      assert_equal ~msg:non_empty_tree_msg 
        true @@ not ((Superset_rcfg.G.nb_vertex decision_tree)=0);
      assert_equal ~msg:expect_entry_msg 
        true @@ (Superset_rcfg.G.mem_vertex decision_tree entry);
      assert_equal ~msg:expect_zero_msg 
        true @@ (Superset_rcfg.G.mem_vertex decision_tree zero);
    )

(* This test is intended to be only slightly different from the entry *)
(* conflict test, differing by the fact that it will occur inline of *)
(* the insn_cfg. *)
let test_overlay_construction test_ctxt = ()

let test_loop_scc test_ctxt = 
  let insn_map, insn_cfg = init () in
  let entry = Addr.(of_int ~width 1) in
  let insn_map, insn_cfg = 
    construct_loop insn_map insn_cfg entry Addr.(entry ++ 20) in
  let loop_points = Addr.Hash_set.create () in
  Superset_rcfg.G.iter_vertex (fun vert -> Hash_set.add loop_points vert) insn_cfg;
  let scc = Superset_rcfg.StrongComponents.scc_list insn_cfg in
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
  let _, insn_cfg = init () in
  let zero = Addr.(of_int ~width 0) in
  let entry = Addr.(of_int ~width 1) in
  Superset_rcfg.G.add_edge insn_cfg zero entry;
  let components = Superset_rcfg.StrongComponents.scc_list insn_cfg in
  let components = Sheathed.filter_components insn_cfg components in
  assert_equal ~msg:"found non scc component" 0 (Set.length components)

let test_find_conflicts test_ctxt = 
  let insn_map, insn_cfg = init () in
  let entry = Addr.(of_int ~width 1) in
  let insn_map, insn_cfg = 
    construct_loop insn_map insn_cfg entry Addr.(entry ++ 20) in
  let in_loop_addr = Addr.(of_int ~width 0x10) in
  let loop_points = Addr.Hash_set.create () in
  Superset_rcfg.G.iter_vertex (Hash_set.add loop_points) insn_cfg;
  let insn_map, insn_cfg =
    construct_tail_conflict insn_map insn_cfg in_loop_addr 6 in
  let conflicts_added = Addr.Hash_set.create () in
  Superset_rcfg.G.iter_vertex (fun vert -> 
      if not (Hash_set.mem loop_points vert) then 
        Hash_set.add conflicts_added vert) insn_cfg;
  let conflicts = Superset_rcfg.find_all_conflicts insn_map insn_cfg in 
  Hash_set.iter conflicts_added ~f:(fun addr -> 
      let msg = sprintf "Expected %s to be found" (Addr.to_string addr) in 
      assert_equal ~msg (Set.mem conflicts addr) true
    )

let test_trim_scc test_ctxt = 
  let insn_map, insn_cfg = init () in
  let zero = Addr.(of_int ~width 0) in
  let entry = Addr.(of_int ~width 1) in
  let img, _ = Image.of_string "" |> ok_exn in
  let brancher = Brancher.of_bil arch in
  let insn_map, insn_cfg = 
    construct_loop insn_map insn_cfg entry Addr.(entry ++ 20) in
  let in_loop_addr = Addr.(of_int ~width 0x10) in
  let loop_points = Addr.Hash_set.create () in
  Superset_rcfg.G.iter_vertex (Hash_set.add loop_points) insn_cfg;
  let insn_map, insn_cfg =
    construct_tail_conflict insn_map insn_cfg in_loop_addr 6 in
  let insns = Addr.Map.data insn_map in
  let insn_map = Superset.raw_superset_to_map insns in
  let conflicts_added = Addr.Hash_set.create () in
  Superset_rcfg.G.iter_vertex (fun vert -> 
      if not (Hash_set.mem loop_points vert) then 
        Hash_set.add conflicts_added vert) insn_cfg;
  let superset = {
    insn_map = insn_map;
    insn_rcfg = insn_cfg;
    brancher = brancher;
    img = img;
  } in
  let superset = Trim.trim superset in
  let insn_map = superset.insn_map in
  let insn_cfg = superset.insn_rcfg in
  let conflicts_added_str = List.to_string ~f:Addr.to_string @@ 
    Hash_set.to_list conflicts_added in
  let removed_msg = "of conflicts " ^ conflicts_added_str 
                    ^ ", residual conflict present within" in
  Hash_set.iter conflicts_added ~f:(fun addr -> 
      let removed_cfg = removed_msg ^ " cfg at " ^ Addr.to_string addr in
      assert_equal ~msg:removed_cfg true
      @@ not (Superset_rcfg.G.mem_vertex insn_cfg addr);
      let removed_map = removed_msg ^ " insn_map at " ^ Addr.to_string addr in
      assert_equal ~msg:removed_map true
      @@ not (Addr.Map.mem insn_map addr);
    );
  let loop_msg = "loop addr should remain during tail trim" in
  Hash_set.iter loop_points ~f:(fun addr -> 
      assert_equal ~msg:loop_msg true @@ Superset_rcfg.G.mem_vertex insn_cfg addr);
  let map_non_subset = "addr in map but not in graph" in
  Addr.Map.iteri insn_map ~f:(fun ~key ~data -> 
      assert_equal ~msg:map_non_subset true @@
      Superset_rcfg.G.mem_vertex insn_cfg key
    );
  let graph_non_subset = "addr in graph but not in map, " in
  Superset_rcfg.G.iter_vertex 
    (fun addr -> 
       if not (Addr.equal addr zero) then
         assert_equal ~msg:(graph_non_subset ^ Addr.to_string addr) true
         @@ Addr.Map.mem insn_map addr
    ) insn_cfg


(* This is an extension of the sheering test wherein a double *)
(* interpretation within a strongly connected component has ancestors. *)
let test_extenuating_scc test_ctxt = ()

let test_decision_construction_combinatorics test_ctxt = ()

(* Add two completely discrete cfg to the graph, and assert that two *)
(* decision sets, which are identical in structure are found by the *)
(* decision tree construction algorithm. *)
let test_decision_sets_of_discrete_components test_ctxt = ()


let () =
  let suite = 
    "suite">:::
    [
      "test_hits_every_byte">:: test_hits_every_byte;
      "test_trim" >:: test_trim;
      "test_trims_invalid_jump" >:: test_trims_invalid_jump;
      "test_shingled_of_superset" >:: test_shingled_of_superset;
      "test_addr_map" >:: test_addr_map;
      "test_construct_entry_conflict"
      >:: test_construct_entry_conflict;
      "test_decision_tree_of_entries"
      >:: test_decision_tree_of_entries;
      "test_tails_of_conflicts" >:: test_tails_of_conflicts;
      "test_tail_construction">:: test_tail_construction;
      "test_extenuating_tail_competitors">::test_extenuating_tail_competitors;
      "test_overlay_construction">:: test_overlay_construction;
      "test_decision_sets_of_discrete_components"
      >:: test_decision_sets_of_discrete_components;
      "test_loop_scc" >:: test_loop_scc;
      "test_find_conflicts" >:: test_find_conflicts;
      "test_scc" >:: test_scc;
      "test_trim_scc" >:: test_trim_scc;
      "test_extenuating_scc" >:: test_extenuating_scc;
      "test_decision_construction_combinatorics"
      >:: test_decision_construction_combinatorics;
    ] in
  run_test_tt_main suite
;;
