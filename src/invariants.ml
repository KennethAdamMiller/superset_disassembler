open Bap.Std
open Core_kernel

module type InvariantApplicator = sig
  val apply : Superset.t -> Superset.t
end


let enforce_exclusivity insn_delta data_delta =
  let insns_in_data = 
    Hash_set.fold ~init:[] data_delta ~f:(fun violators data -> 
        if Hash_set.mem insn_delta data then
          data :: violators
        else violators
      ) in
  let datas_in_insn = 
    Hash_set.fold ~init:[] insn_delta ~f:(fun violators insn -> 
        if Hash_set.mem data_delta insn then
          insn :: violators
        else violators
      ) in
  (insns_in_data, datas_in_insn)

let mark_nonexclusive superset insn_delta data_delta ~mark =
  let (data_violators, insn_violators) =
    enforce_exclusivity insn_delta data_delta in
  List.iter data_violators ~f:mark;
  List.iter insn_violators ~f:mark

let tag_layer_violations superset = 
  let add_data_of_insn dataset at = 
    Superset.Occlusion.with_data_of_insn superset at ~f:(Hash_set.add dataset)
  in
  let remove_data_of_insn dataset at =
    Superset.Occlusion.with_data_of_insn superset at ~f:(Hash_set.remove dataset)
  in
  let conflicts = Superset.Occlusion.find_all_conflicts superset in
  let entries = Superset.entries_of_isg superset in
  let tails = Decision_tree_set.tails_of_conflicts superset
      conflicts in
  let options = Map.fold tails ~init:Addr.Set.empty ~f:
      (fun ~key ~data options -> 
         List.fold ~init:options data ~f:Set.add) in
  let is_option addr = 
    Set.mem options addr in
  let insns = Addr.Hash_set.create () in
  let datas = Addr.Hash_set.create () in
  let pre deltas addr = 
    add_data_of_insn datas addr;
    Hash_set.add insns addr  in  
  let tag_violators deltas addr = 
    match Map.find deltas addr with
    | Some (insn_delta, data_delta) -> 
      Hash_set.iter insn_delta ~f:(fun insn -> 
          let inbound = Superset.ISG.descendants superset insn in
          (* TODO what if we encounter a predecessor we haven't *)
          (* visited before? *)
          List.iter inbound ~f:(fun src -> 
              if Hash_set.mem data_delta src then (
                Superset.Core.mark_bad superset insn;
              ) (*else if Hash_set.mem datas src then (
                  Superset.mark_bad superset insn;
                  )*)
            );
        );
      (*mark_nonexclusive superset insn_delta data_delta
        ~mark:(Superset.mark_bad superset)*)
    | None -> ();
  in
  let post deltas addr = 
    tag_violators deltas addr;
    Hash_set.remove insns addr;
    remove_data_of_insn datas addr in
  Traverse.visit_with_deltas 
    ~is_option ~pre ~post superset entries;
  superset

let tag_branch_violations superset = 
  let add_data_of_insn dataset at = 
    Superset.Occlusion.with_data_of_insn
      superset at ~f:(Hash_set.add dataset)
  in
  (* TODO removing should move to an alternate set to track discrete lineages *)
  let remove_data_of_insn dataset at =
    Superset.Occlusion.with_data_of_insn
      superset at ~f:(Hash_set.remove dataset)      
  in
  let insns = Addr.Hash_set.create () in
  let datas = Addr.Hash_set.create () in
  let pre addr = 
    add_data_of_insn datas addr;
    Hash_set.add insns addr;
    if Hash_set.mem datas addr then (
      Superset.Core.mark_bad superset addr;
    );
    let inbound = Superset.ISG.descendants superset addr in
    List.iter inbound ~f:(fun target -> 
        let ft = Superset.is_fall_through
            superset addr target in
        if not ft then (
          if Hash_set.mem datas target then
            Superset.Core.mark_bad superset addr;
        )
      )
  in
  let post addr =
    (* TODO removing should move to a different set, for tracking
       alternate lineages *)
    Hash_set.remove insns addr;
    remove_data_of_insn datas addr in
  let entries = Superset.entries_of_isg superset in
  Traverse.visit ~pre ~post superset entries;
  superset
