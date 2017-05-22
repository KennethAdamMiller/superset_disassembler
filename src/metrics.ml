open Bap.Std
open Core_kernel.Std
open Superset_rcfg

type format_as   = | Latex
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
}

let format_standard metrics =
  match metrics with 
  | Some metrics -> 
    sprintf "%s%d\n%s%d\n%s%d\n%s%d\n%s%d\n" 
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

let gather_metrics ~ground_truth insn_map insn_rcfg metrics =
  let metrics = Option.value metrics ~default:{
      name = ground_truth;
      detected_insn_count = 0;
      false_negatives     = 0;
      false_positives     = 0;
      detected_entries    = 0;
      actual_entries      = 0;
      trimmed             = [];
    } in
  let function_starts = Insn_disasm_benchmark.ground_truth_of_unstripped_bin
      ground_truth |> ok_exn in
  let ground_truth =
    Addr.Set.of_list @@ Seq.to_list function_starts in
  let reduced_occlusion = Addr.Hash_set.create () in
  let insn_cfg = Superset_rcfg.Oper.mirror insn_rcfg in
  let dfs_find_conflicts addr = 
    let add_conflicts addr = 
      Seq.iter (Superset_rcfg.conflict_seq_at insn_map addr)
        ~f:(fun x -> if Superset_rcfg.G.mem_vertex insn_cfg x then
               Hash_set.add reduced_occlusion x) in
    Superset_rcfg.Dfs.prefix_component add_conflicts insn_cfg addr;
  in
  Seq.iter function_starts ~f:dfs_find_conflicts;
  printf "Reduced occlusion: %d\n" Hash_set.(length reduced_occlusion);
  let detected_insns = 
    G.fold_vertex 
      (fun vert detected_insns -> Set.add detected_insns vert) 
      insn_rcfg Addr.Set.empty in
  let missed_set = Set.diff ground_truth detected_insns in
  if not (Set.length missed_set = 0) then
    printf "Missed function entrances %s\n" 
      (List.to_string ~f:Addr.to_string @@ Set.to_list missed_set);
  printf "Occlusion: %d\n" 
    (Set.length @@ Superset_rcfg.find_all_conflicts insn_map insn_rcfg);
  let detected_entries =
    Set.(length (inter detected_insns ground_truth)) in
  let missed_entrances = Set.diff ground_truth detected_insns in
  let false_negatives =
    Set.(length (missed_entrances)) in
  let false_positives =
    Set.(length (diff detected_insns ground_truth)) in
  let detected_insn_count = G.nb_vertex insn_rcfg in
  Some ({
      name                = metrics.name;
      detected_insn_count = detected_insn_count + metrics.detected_insn_count;
      false_positives     = false_positives + metrics.false_positives;
      false_negatives     = false_negatives + metrics.false_negatives;
      detected_entries    = metrics.detected_entries + detected_entries;
      actual_entries      = metrics.actual_entries
                            + (Set.length ground_truth);
      trimmed             = metrics.trimmed;
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
  let metrics_format = 
    Arg.(value & opt (enum list_formats_types) Standard
         & info ["metrics_format"] ~doc:list_formats_doc)

end
