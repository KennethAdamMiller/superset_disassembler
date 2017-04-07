open Core_kernel.Std
open Bap.Std
open Insn_cfg
open Common
open Graphlib.Std
open Graph

(* Extracts single node strongly connected components *)
let filter_components components = 
  List.fold_left components ~init:Addr.Set.empty ~f:(fun filtered component ->
      match component with
      | _ :: [] | [] -> filtered
      | _ :: _ -> List.fold component ~init:filtered ~f:
                    (fun filtered addr ->
                       Set.add filtered addr)
    )

(* TODO simplify the code *)
let sheer insn_map cfg arch = 
  let keep = filter_components @@ StrongComponents.scc_list cfg in
  let to_remove = Set.fold keep ~init:Addr.Set.empty
      ~f:(fun to_remove addr -> 
          conflicts_within_insn_at 
            ~conflicts:to_remove insn_map addr
        ) in
  let to_remove = Set.diff to_remove keep in
  let bad = bad_of_arch arch in
  Set.iter to_remove ~f:(G.add_edge cfg bad);
  Shingled.sheer insn_map cfg arch

let sheaths_of_file ?(backend="llvm") bin = 
  let arch, insn_map, superset = Shingled.sheered_cfg_of_file ~backend bin in
  insn_map, superset, Sheath_tree_set.decision_trees_of_shingles superset insn_map

let sheered_sheaths_of_file ?(backend="llvm") bin =
  let arch, insn_map, sheered_cfg =
    Shingled.sheered_cfg_of_file ~backend bin in
  let insn_map, insn_cfg = sheer insn_map sheered_cfg arch in
  insn_map, insn_cfg, Sheath_tree_set.decision_trees_of_shingles insn_cfg insn_map

let fold_decision_set ?(backend="llvm") bin ~f = ()
let iter_decision_set ?(backend="llvm") bin ~f = ()
