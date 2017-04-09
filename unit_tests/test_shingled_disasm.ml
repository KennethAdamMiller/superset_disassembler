open OUnit2
open Core_kernel.Std
open Bap.Std
open Or_error
open Common
open Bap_plugins.Std
open Insn_cfg
module Cfg = Graphs.Cfg

let () = Pervasives.ignore(Plugins.load ())

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

let test_sheers test_ctxt =
  let memory, arch = make_params "\x2d\xdd\xc3\x54\x55" in
  Pervasives.ignore (Superset.disasm
                       ~accu:[] ~f:List.cons arch memory >>| fun insns ->
                     let superset_cfg = Shingled.rcfg_of_superset
                         insns memory arch in
                     let insn_map = Shingled.superset_to_map
                         insns Addr.Map.empty superset_cfg in
                     let insn_map, sheered_shingles = Shingled.trim insn_map superset_cfg arch in
                     (* the above is a byte sequence no compiler would produce. The
                        sheering algorithm would therefore wipe all
                        clean  *)
                     let msg = G.fold_vertex
                         (fun vert  accu -> 
                            accu ^ "\n" ^ (Addr.to_string vert))
                         sheered_shingles "" in
                     assert_equal ~msg 0
                     @@ G.nb_vertex sheered_shingles)

let test_retain_valid_jump test_ctxt = ()

let test_sheers_invalid_jump test_ctxt =
  let memory, arch = make_params "\x55\x54\xE9\xFC\xFF\xFF\xFF" in
  let insn_map, sheered_shingles = Shingled.disasm arch memory |> ok_exn in
  let expected_results = [ ] in
  assert_equal ~msg:"lengths unequal" (G.nb_vertex sheered_shingles)
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
  let insn_cfg = Insn_cfg.G.create () in
  let addr_size = Size.in_bits @@ Arch.addr_size Arch.(`x86_64) in
  let addr  = Addr.of_int addr_size 0 in
  Insn_cfg.G.add_vertex insn_cfg addr;
  Insn_cfg.G.add_vertex insn_cfg addr;
  let msg = "expected length to be one" in
  assert_bool msg ((Insn_cfg.G.nb_vertex insn_cfg) = 1)  

let test_shingled_of_superset test_ctxt = 
  let memory, arch = make_params "\x55\x54\xE9\xFC\xFF\xFF\xFF" in
  let insns = Superset.disasm ~accu:[] ~f:List.cons arch memory
              |> ok_exn in
  let insn_cfg = Shingled.rcfg_of_superset insns memory arch in
  let insn_map = Shingled.superset_to_map insns Addr.Map.empty insn_cfg in
  let msg = "insn in cfg but not in map after shingled of superset" in
  Insn_cfg.G.iter_vertex (fun v -> 
      let msg = msg ^ Addr.to_string v in
      assert_bool msg (Map.mem insn_map v))
    insn_cfg;
  let msg = "insn in map but not in after shingled of superset" in
  Addr.Map.iteri insn_map (fun ~key ~data -> 
      assert_bool msg 
        (Insn_cfg.G.mem_vertex insn_cfg key))

let test_conflicts_within_insn test_ctxt = ()

let test_find_all_conflicts test_ctxt = ()

let test_retain_cross_segment_references test_ctxt = ()

let () =
  let suite = 
    "suite">:::
    [
      "test_hits_every_byte">:: test_hits_every_byte;
      "test_sheers" >:: test_sheers;
      "test_sheers_invalid_jump" >:: test_sheers_invalid_jump;
      "test_shingled_of_superset" >:: test_shingled_of_superset;
      "test_addr_map" >:: test_addr_map;
    ] in
  run_test_tt_main suite
;;
