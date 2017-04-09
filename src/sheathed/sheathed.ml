open Core_kernel.Std
open Bap.Std
open Insn_cfg
open Common
open Graphlib.Std
open Graph

let parents_of_insns insn_cfg component = 
  Set.fold component ~init:Addr.Set.empty ~f:(fun potential_exits addr -> 
      Insn_cfg.G.fold_succ (fun ancestor potential_exits ->
          if not Set.(mem component ancestor) then
            Set.add potential_exits ancestor
          else potential_exits
        ) insn_cfg addr potential_exits
    )

let filter_components insn_cfg components = 
  List.fold_left components ~init:Addr.Set.empty
    ~f:(fun keep  component ->
        let component = Addr.Set.of_list component in
        if Set.length component > 20 then
          Addr.Set.(union keep component)
        else
          keep
      )

let insn_hex insn_map addr = 
  match Map.find insn_map addr with
  | Some(mem, insn) -> 
    let insn_to_string insn = Insn.(to_string (of_basic insn)) in
    sprintf "%s %s" 
      (Memory.to_string mem)
      Option.(map insn ~f:insn_to_string |> value ~default:"")
  | None -> ""

let string_of_insn insn_map addr =
  sprintf "insn at %s"
    (insn_hex insn_map addr)

let insn_mem_at insn_map addr = 
  match Map.find insn_map addr with
  | Some(mem, _) -> Some(mem)
  | None -> None

let trim insn_map cfg arch = 
  let keep = filter_components cfg @@ StrongComponents.scc_list cfg in
  (* Here we have to be careful; we only want to find instructions
     that occur within a loop that produce a self-contradiction *)
  let parents = parents_of_insns cfg keep in
  let to_remove = Insn_cfg.conflicts_within_insns insn_map keep in
  let to_remove = Set.inter to_remove parents in
  let to_remove = Set.diff to_remove keep in
  let bad = bad_of_arch arch in
  Set.iter to_remove ~f:(G.add_edge cfg bad);
  Shingled.trim insn_map cfg arch

let sheaths_of_file ?(backend="llvm") bin = 
  let arch, insn_map, superset = Shingled.trimmed_cfg_of_file ~backend bin in
  insn_map, superset, Sheath_tree_set.decision_trees_of_shingles superset insn_map

let trimmed_sheaths_of_file ?(backend="llvm") bin =
  let arch, insn_map, trimmed_cfg =
    Shingled.trimmed_cfg_of_file ~backend bin in
  let insn_map, insn_cfg = trim insn_map trimmed_cfg arch in
  insn_map, insn_cfg, Sheath_tree_set.decision_trees_of_shingles insn_cfg insn_map

(* TODO test the below functions *)
let iter_decision_set ?(backend="llvm") bin ~f = 
  let insn_map, insn_cfg, decision_trees =
    trimmed_sheaths_of_file ~backend bin in
  List.iter decision_trees ~f

let fold_decision_set ~init ?(backend="llvm") bin ~f =
  let insn_map, insn_cfg, decision_trees =
    trimmed_sheaths_of_file ~backend bin in
  List.fold decision_trees ~init ~f

