open Core_kernel.Std
open Bap.Std
open Insn_cfg
open Common
open Graphlib.Std
open Graph


let sheer cfg insn_map = 
  let keep = StrongComponents.scc_list cfg in
  let to_remove = List.fold ~init:(Addr.Hash_set.create ()) keep ~f:(fun accu component -> 
      List.fold ~init:accu component ~f:(fun to_remove vert -> 
          conflicts_within_insn_at to_remove insn_map vert find_conflicts_with
        )) in
  let (insn_map, _) = Hash_set.fold ~init:(insn_map, None) to_remove
      ~f:(fun (insn_map, bad) vert -> 
          let bad = Option.value bad ~default:(bad_of_addr vert) in
          G.add_edge cfg bad vert;
          (Map.remove insn_map vert, Some (bad))
        ) in
  insn_map

let disasm_file ?(backend="llvm") bin = 
  let img = img_of_filename bin in
  let arch = Image.arch img in
  let insn_map = Addr.Map.empty in
  let superset_cfg = G.create () in
  let (superset, insn_map) = 
    Shingled.with_img ~accu:(superset_cfg, insn_map)
      ~backend img ~f:(fun ~accu ~backend arch mem -> 
          let (superset_cfg, insn_map) = accu in
          let superset = 
            Superset.disasm ~backend ~accu:[] ~f:List.cons arch mem
            |> ok_exn  in
          Or_error.return ((Shingled.cfg_of_shingles 
                              ~superset_cfg superset mem arch),
                           Shingled.shingled_to_map superset insn_map)
        ) in
  superset, insn_map, arch

let sheaths_of_file ?(backend="llvm") bin = 
  let superset, insn_map, arch = disasm_file ~backend bin in
  let sheered = Shingled.sheer superset arch in
  Sheath_tree_set.decision_trees_of_shingles sheered insn_map



let sheered_sheaths_of_file ?(backend="llvm") bin =
  let superset, insn_map, arch = disasm_file ~backend bin in
  let sheered = Shingled.sheer superset arch in
  let insn_map = sheer sheered insn_map in
  Sheath_tree_set.decision_trees_of_shingles sheered insn_map

let fold_decision_set ?(backend="llvm") bin ~f = ()
let iter_decision_set ?(backend="llvm") bin ~f = ()
