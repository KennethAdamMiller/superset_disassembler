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
  let tails = Decision_trees.tails_of_conflicts superset
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
  Decision_trees.visit_with_deltas 
    ~is_option ~pre ~post superset entries;
  superset

let tag_branch_violations superset = 
  let add_data_of_insn dataset at = 
    Superset.Occlusion.with_data_of_insn
      superset at ~f:(Hash_set.add dataset)
  in
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
    Hash_set.remove insns addr;
    remove_data_of_insn datas addr in
  let entries = Superset.entries_of_isg superset in
  Traverse.visit ~pre ~post superset entries;
  superset

(** This is a strong invariant. It builds a visitor with a superset
    that identifies static memory reads or writes to addresses the
    superset doesn't contain. *)
let find_non_mem_accesses superset = 
  let check_return_addr r addr = 
    match addr with
    | Bil.Int(addr) -> 
      if Superset.Inspection.contains_addr superset addr then
        r
      else r.return(Some(false))
    | _ -> r in
  (object(self) 
    inherit [bool] Stmt.finder
    method! enter_load ~mem ~addr _ _ r = 
      check_return_addr r addr
    method! enter_store ~mem ~addr ~exp _ _ r =
      check_return_addr r addr
  end)


(** This is a strong invariant. It looks for places where
    find_non_mem_accesses is tripped. *)
let accesses_non_mem superset mem insn _ =
  try
    let bil = Superset.Core.lift_at superset (Memory.min_addr mem) in
    let bil = Option.value ~default:([]) bil in
    let status = List.fold bil ~init:(Some(false)) ~f:(fun status _stmt ->
        Option.value_map status ~default:(Some(false)) ~f:(fun status ->
            if not status then
              Stmt.find (find_non_mem_accesses superset) _stmt
            else Some(status)
          )
      ) in
    Option.value status ~default:false
  with _ -> false 

(** Build the static successors of the current instruction, and tag
    the superset with the function f in a micro-context. *)
let tag_with ~f (mem, insn) superset = 
  let targets = Superset.Inspection.static_successors superset mem insn in
  f superset mem insn targets

(** Looks for control flow edges to addresses that are not statically
    known. *)
let tag_target_not_in_mem superset mem insn targets =
  List.iter targets
    ~f:(fun (target,_) ->
        match target with 
        | Some(target) -> 
          if not (Superset.Inspection.contains_addr superset target) then
            Superset.Core.mark_bad superset (Memory.min_addr mem)
        | None -> ()
      );
  superset

(** If a jmp to NULL occurs, then this is triggered. *)
let tag_target_is_bad superset mem insn targets =
  let width = Addr.bitwidth @@ Memory.min_addr mem in
  let z = Addr.zero width in
  List.iter targets
    ~f:(fun (target,_) ->
        match target with 
        | Some(target) -> 
          if Addr.(target = z) then
            Superset.Core.mark_bad superset target
        | None -> ()
      );
  superset

(** If a jmp specifies the body of the instruction itself, this is
    triggered. *)
let tag_target_in_body superset mem insn targets =
  let src = Memory.min_addr mem in
  List.iter targets
    ~f:(fun (target,_) ->
        match target with 
        | Some(target) -> 
          if (Memory.contains mem target) && 
             not Addr.(src = target) then
            Superset.Core.mark_bad superset src
        | None -> ()
      );
  superset

(** Applies the tag together with the visitor and smaller functions
    under the hood. *)
let tag_non_mem_access superset mem insn targets = 
  let src  = Memory.min_addr mem in
  if accesses_non_mem superset mem insn targets then (
    (* The instruction reads or writes to memory that is not mapped *)
    Superset.Core.mark_bad superset src
  );
  superset

(** If the instruction could not be disassembled or lifted, then tag
    it. *)
let tag_non_insn superset mem insn targets = 
  let src  = Memory.min_addr mem in
  if Option.is_none insn then (
    Superset.Core.mark_bad superset src
  );
  superset

(** Used for the maintenance and construction of the superset. *)
let tag_success superset mem insn targets =
  let src = Memory.min_addr mem in
  List.fold targets ~init:superset ~f:(fun superset (target,_) -> 
      match target with
      | Some (target) -> 
        Superset.ISG.link superset target src
      | None -> superset)

let default_tags = ["Tag non insn", tag_non_insn;
                    "Tag non mem access", tag_non_mem_access;
                    "Tag target not in mem", tag_target_not_in_mem;
                    "Tag target is bad", tag_target_is_bad;
                    "Tag target in body", tag_target_in_body;
                    (*tag_success;*)]

let default_funcs = List.map default_tags ~f:snd

(** Tag an individual instruction of a superset with a list of
    invariants. *)
let tag ?invariants =
  let invariants = Option.value invariants ~default:default_funcs in
  let f superset mem insn targets =
    List.fold_left invariants ~init:superset ~f:(fun superset f -> 
        (f superset mem insn targets)) in
  tag_with ~f


(** Tag with a list of invariants over an entire superset. *)
let tag_superset ?invariants superset = 
  let invariants = Option.value invariants ~default:default_funcs in
  let f superset mem insn targets =
    List.fold ~init:superset invariants
      ~f:(fun superset invariant -> 
          invariant superset mem insn targets) in
  Superset.Core.fold ~init:superset superset ~f:(fun ~key ~data superset -> 
      let mem, insn = data in
      tag_with ~f (mem, insn) superset
    )
