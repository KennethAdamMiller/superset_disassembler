open OUnit2
open Core_kernel.Std
open Bap.Std
open Or_error
open Common
open Bap_plugins.Std
module Dis = Disasm_expert.Basic
module Cfg = Graphs.Cfg

let () = Pervasives.ignore(Plugins.load ())

let make_params bytes =
  let arch = Arch.(`x86) in
  let addr_size= Size.in_bits @@ Arch.addr_size arch in
  let min_addr = Addr.of_int addr_size 0 in
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
  Seq.of_list [Shingled_disasm.G.nb_vertex shingles]

let superset_to_length_list superset =
  List.map superset ~f:(fun (mem, insn) -> (Memory.length mem))

(* This test affirms that both the order and the inner sequences of a set of bytes
   will be interpreted appropriately by the superset conservative disassembler *)
let test_hits_every_byte test_ctxt =
  let memory, arch = make_params "\x2d\xdd\xc3\x54\x55" in
  let shingled_disassembly = Shingled_disasm.Conservative.disasm arch memory |> ok_exn in
  let sizes = superset_to_length_list shingled_disassembly in
  let expected_results = [ 5; 2; 1; 1; 1; ] in
  check_results (Seq.of_list sizes) expected_results

let test_sheers test_ctxt =
  let memory, arch = make_params "\x2d\xdd\xc3\x54\x55" in
  Pervasives.ignore (Shingled_disasm.Conservative.disasm arch memory >>| fun insns ->
                     let (bad, superset_cfg) = Shingled_disasm.insert_all insns memory arch in
                     let sheered_shingles = Shingled_disasm.sheer superset_cfg arch in
                     (* the above is a byte sequence no compiler would produce. The
                        sheering algorithm would therefore wipe all
                        clean  *)
                     let msg = Shingled_disasm.G.fold_vertex
                         (fun vert  accu -> 
                            accu ^ "\n" ^ (Addr.to_string vert))
                         sheered_shingles "" in
                     assert_equal ~msg 0
                     @@ Shingled_disasm.G.nb_vertex sheered_shingles)

let test_sheers_invalid_jump test_ctxt =
  let memory, arch = make_params "\x55\x54\xE9\xFC\xFF\xFF\xFF" in
  let sheered_shingles = Shingled_disasm.disasm arch memory |> ok_exn in
  let expected_results = [ ] in
  assert_equal ~msg:"lengths unequal" (Shingled_disasm.G.nb_vertex sheered_shingles)
    (List.length expected_results)

let () =
  let suite = 
    "suite">:::
    [
      "test_hits_every_byte">:: test_hits_every_byte;
      "test_sheers" >:: test_sheers;
      "test_sheers_invalid_jump" >:: test_sheers_invalid_jump;
    ] in
  run_test_tt_main suite
;;
