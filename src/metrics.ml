open Bap.Std
open Core_kernel
open Or_error

module Linear = Disasm_expert.Linear
exception Inconsistent_img of string

type format_as = | Latex
                 | Standard
[@@deriving sexp]

type metrics = {
  name                : string;
  detected_insn_count : int;
  false_negatives     : int;
  false_positives     : int;
  detected_entries    : int;
  actual_entries      : int;
  trimmed             : int list;
} [@@deriving sexp, bin_io]

module InvariantTrackingApplicator = struct
end

module type MetricsGathererInstance = sig
  type acc = (Addr.Hash_set.t)
  val accu : acc
end

module MetricsGatheringReducer(M : MetricsGathererInstance) : Trim.Reducer = struct
  type acc = M.acc
  let accu = M.accu
  let check_pre _ accu _ = accu
  let check_post _ accu _ = accu
  let check_elim _ _ _ = true
  let mark superset s addr =
    Hash_set.add s addr

end

module type PerMarkTracker = sig
  type acc = (Addr.Set.t Addr.Map.t) ref
  val accu : acc
end

module PerMarkTrackingReducer(M : PerMarkTracker) : Trim.Reducer =
struct
  type acc = M.acc
  let accu = M.accu
  let cur_root = ref None
  let check_pre superset accu addr =
    (match !cur_root with
     | Some x ->
       accu :=
         Map.update !accu x ~f:(fun s ->
             match s with
             | None -> Addr.Set.empty
             | Some s -> Set.add s addr
           );
     | None -> cur_root:=Some(addr));
    accu
  let check_post superset (accu : acc) addr =
    (match !cur_root with
     | None -> ()
     | Some x -> if Addr.(x = addr) then cur_root := None);
    accu
  let check_elim _ _ _ = true
  let mark superset accu addr =
    let cur_root = Option.value_exn !cur_root in
    accu :=
      Map.update !accu cur_root ~f:(fun s ->
          match s with
          | None -> Addr.Set.empty
          | Some s -> Set.add s addr
        );
end

let make_gatherer accu = 
  let module Instance = MetricsGatheringReducer(struct
      type acc = (Addr.Hash_set.t )
      let accu = accu
    end) in
  let module Instance = Trim.Reduction(Instance) in
  Instance.trim 

let make_mark_tracker accu =
  let module Instance =
    PerMarkTrackingReducer(
    struct
      type acc = (Addr.Set.t Addr.Map.t) ref
      let accu = accu
    end) in
  let module Instance = Trim.Reduction(Instance) in
  Instance.trim
  
let format_standard metrics =
  match metrics with 
  | Some metrics -> 
    sprintf "%s%d\n%s%d\n%s%d\n%s%d\n%s%d" 
      "Total instructions recovered: " metrics.detected_insn_count
      "False negatives: " metrics.false_negatives
      "False positives: " metrics.false_positives
      "Detected function entrances: " metrics.detected_entries
      "Actual function entrances: " metrics.actual_entries
  | None -> "No metrics gathered!"

let format_latex metrics = 
  match metrics with
  | Some metrics ->
    (match metrics.trimmed with
     | (phase1 :: phase2 :: _) ->
       sprintf "%s & %d & %d & %d & %d \\\\\n"
         metrics.name
         metrics.false_negatives
         phase1
         phase2
         metrics.detected_insn_count;
     | _ -> "Missing trim phases")
  | None -> "No metrics gathered!"

let true_positives_of_ground_truth superset ground_truth = 
  let true_positives = Addr.Hash_set.create () in
  Set.iter ground_truth ~f:(fun addr -> 
      if Superset.ISG.mem_vertex superset addr then
        Traverse.with_descendents_at
          ~visited:true_positives
          superset addr;
    );
  true_positives

let read arch ic : (string * addr * addr) list =
  let sym_of_sexp x = [%of_sexp:string * int64 * int64] x in
  let addr_of_int64 x =
    let width = Arch.addr_size arch |> Size.in_bits in
    Addr.of_int64 ~width x in
  List.(Sexp.input_sexps ic >>| sym_of_sexp >>| (fun (s, es, ef) ->
      s, addr_of_int64 es, addr_of_int64 ef))

let read_addrs ic : addr list =
  List.t_of_sexp Addr.t_of_sexp @@ Sexp.input_sexp ic

let ground_truth_of_unstripped_bin bin : addr seq Or_error.t =
  let name = Filename.basename bin in
  let tmp = Filename.temp_dir_name ^ "/bw_" ^ name ^ ".symbol" in
  let cmd = sprintf "bap-byteweight dump -i symbols %S > %S" 
      bin tmp in
  if Sys.command cmd = 0
  then return (Seq.of_list @@ In_channel.with_file tmp ~f:read_addrs)
  else errorf
      "failed to fetch symbols from unstripped binary, command `%s'
  failed" cmd
  
let true_positives superset f = 
  let function_starts = ground_truth_of_unstripped_bin f |> ok_exn
  in
  let ground_truth =
    Addr.Set.of_list @@ Seq.to_list function_starts in
  true_positives_of_ground_truth superset ground_truth

let reduced_occlusion superset tp =
  let fps = Addr.Hash_set.create () in
  Hash_set.iter tp ~f:(fun addr ->
      Superset.Occlusion.with_data_of_insn superset addr
        ~f:(fun x -> Hash_set.add fps x);
      Hash_set.remove fps addr;
    );
  fps

let false_positives superset ro = 
  let fps = Addr.Hash_set.create () in
  Hash_set.iter ro ~f:(fun v ->
      if Superset.ISG.mem_vertex superset v then
        Hash_set.add fps v
    );
  fps

let fn_insn_cnt superset tps =
  Hash_set.fold ~init:0 tps ~f:(fun count v -> 
      if Superset.ISG.mem_vertex superset v then count 
      else count+1)

let check_tp_set true_positives s =
  let n = Hash_set.length s in
  let tp_of_s = 
    Hash_set.fold ~init:0 true_positives
      ~f:(fun tp_of_s x -> 
          if Hash_set.mem s x
          then tp_of_s + 1 else tp_of_s) in
  let fp_of_s = n - tp_of_s in
  fp_of_s, tp_of_s

let check_fn_entries superset ground_truth =
  let detected_insns = 
    Superset.Core.fold superset ~init:Addr.Set.empty
      ~f:(fun ~key ~data detected_insns ->
          Set.add detected_insns key) in
  Set.diff ground_truth detected_insns

let gather_metrics ~bin superset =
  let function_starts = ground_truth_of_unstripped_bin bin |> ok_exn in
  let ground_truth =
    Addr.Set.of_list @@ Seq.to_list function_starts in
  let ground_truth = 
    Set.(filter ground_truth ~f:(fun e ->
        Superset.Inspection.contains_addr superset e
      )) in
  let true_positives = true_positives_of_ground_truth superset ground_truth in
  let datas = ref 0 in
  let ro = ref 0 in
  let detected_insns = Addr.Hash_set.create () in
  let dfs_find_conflicts addr =
    Traverse.with_descendents_at ~visited:detected_insns superset addr
      ~pre:(fun v ->
        Superset.Occlusion.with_data_of_insn superset v
          ~f:(fun x ->
            if Superset.Core.(mem superset x)
            then ro := !ro+1; 
            datas := !datas +1)) in
  let num_bytes = Superset.Inspection.total_bytes superset in
  let entries = Superset.entries_of_isg superset in
  let branches = Grammar.linear_branch_sweep superset entries in
  let fp_branches, tp_branches = check_tp_set true_positives branches in
  printf "Num f.p. branches: %d, num tp branches: %d\n" fp_branches tp_branches;
  printf "superset_isg_of_mem length %d\n" num_bytes;
  let total_clean,_ =
    Set.fold ground_truth ~init:(0,0) ~f:(fun (n,prev) x ->
        dfs_find_conflicts x;
        if !ro > prev then
          ((n),!ro)
        else ((n+1),prev)
      ) in
  printf "Number of functions precisely trimmed: %d of %d\n"
    total_clean Set.(length ground_truth);
  printf "Number of possible reduced false positives: %d\n" 
    !datas;
  printf "Reduced occlusion: %d\n" (!ro);
  printf "True positives: %d\n" Hash_set.(length true_positives);
  let fn_entries = check_fn_entries superset ground_truth in
  if not (Set.length fn_entries = 0) then
    printf "Missed function entrances %s\n" 
      (List.to_string ~f:Addr.to_string @@ Set.to_list fn_entries);
  printf "Occlusion: %d\n" 
    (Set.length @@ Superset.Occlusion.find_all_conflicts superset);
  printf "Instruction fns: %d\n"
    (fn_insn_cnt superset true_positives);
  let detected_insn_count = Superset.Inspection.count superset in
  printf "superset_map length %d graph size: %d\n" 
    Superset.Inspection.(count_unbalanced superset)
    detected_insn_count;
  let false_negatives = Set.(length fn_entries) in
  let detected_entries = Set.(length ground_truth) - false_negatives in
  let false_positives = Hash_set.fold detected_insns ~init:0
      ~f:(fun c v -> if not Set.(mem ground_truth v) then c+1 else c) in
  Some ({
      name                = bin;
      detected_insn_count = detected_insn_count;
      false_positives     = false_positives;
      false_negatives     = false_negatives;
      detected_entries    = detected_entries;
      actual_entries      = (Set.length ground_truth);
      trimmed             = [];
    })

module Opts = struct 
  open Cmdliner

  let list_content_doc = sprintf
      "Metrics may be collected against a symbol file"
  let content_type = 
    Arg.(value &
         opt (some string) (None)
         & info ["metrics_data"] ~doc:list_content_doc)

  let list_formats_types = [
    "standard", Standard;
    "latex", Latex;
  ]
  let list_formats_doc = sprintf
      "Available output metrics formats: %s" @@ 
    Arg.doc_alts_enum list_formats_types
  let metrics_fmt = 
    Arg.(value & opt (enum list_formats_types) Standard
         & info ["metrics_format"] ~doc:list_formats_doc)

end
