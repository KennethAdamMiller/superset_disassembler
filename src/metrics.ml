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

let gather_metrics ~bin superset =
  let insn_map = Superset.get_data superset in
  let open Superset in
  let insn_rcfg = superset.insn_rcfg in
  let function_starts =
    Insn_disasm_benchmark.ground_truth_of_unstripped_bin bin |> ok_exn in
  let ground_truth =
    Addr.Set.of_list @@ Seq.to_list function_starts in
  let reduced_occlusion = Addr.Hash_set.create () in
  let true_positives = Addr.Hash_set.create () in
  let insn_cfg = Superset_rcfg.Oper.mirror insn_rcfg in
  let data_bytes = Addr.Hash_set.create () in
  let dfs_find_conflicts addr = 
    let add_conflicts addr =
      Hash_set.add true_positives addr;
      let len = match Addr.Map.find insn_map addr with 
        | Some (mem, _) -> 
          Memory.length mem
        | None -> 0 in
      Seq.iter (Superset_rcfg.seq_of_addr_range addr len) 
        ~f:(fun x -> Hash_set.add data_bytes x);
      Seq.iter (Superset_rcfg.conflict_seq_at insn_map addr)
        ~f:(fun x -> Hash_set.add reduced_occlusion x) in
    if Superset_rcfg.G.mem_vertex insn_cfg addr then
      Superset_rcfg.Dfs.prefix_component add_conflicts insn_cfg addr;
  in
  Set.iter ground_truth ~f:dfs_find_conflicts;
  printf "Number of possible reduced false positives: %d\n" 
    Hash_set.(length data_bytes);
  printf "Reduced occlusion: %d\n" Hash_set.(length reduced_occlusion);
  printf "True positives: %d\n" Hash_set.(length true_positives);
  let detected_insns = 
    G.fold_vertex 
      (fun vert detected_insns -> Set.add detected_insns vert) 
      insn_rcfg Addr.Set.empty in
  let missed_set = Set.diff ground_truth detected_insns in
  if not (Set.length missed_set = 0) then
    printf "Missed function entrances %s\n" 
      (List.to_string ~f:Addr.to_string @@ Set.to_list missed_set);
  printf "Occlusion: %d\n" 
    (Set.length @@ Superset_rcfg.find_all_conflicts insn_map);
  printf "superset_map length %d graph size: %d\n" 
    Addr.Map.(length insn_map) 
    (Superset_rcfg.G.nb_vertex superset.insn_rcfg);
  let detected_entries =
    Set.(length (inter detected_insns ground_truth)) in
  let missed_entrances = Set.diff ground_truth detected_insns in
  let false_negatives =
    Set.(length (missed_entrances)) in
  let false_positives =
    Set.(length (diff detected_insns ground_truth)) in
  let detected_insn_count = G.nb_vertex insn_rcfg in
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
  let metrics_format = 
    Arg.(value & opt (enum list_formats_types) Standard
         & info ["metrics_format"] ~doc:list_formats_doc)

end
