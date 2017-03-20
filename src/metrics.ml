open Bap.Std
open Core_kernel.Std
open Insn_cfg

type format_as   = | Latex
                   | Standard
[@@deriving sexp]

type metrics = {
  detected_insn_count : int;
  false_negatives     : int;
  false_positives     : int;
  extenuating_loss    : int option;
  total_actual_insns  : int option;
}

let format_standard metrics =
  match metrics with 
  | Some metrics -> 
    sprintf "Total instructions recovered: %d\n"
      metrics.detected_insn_count
  | None -> sprintf "No metrics gathered!"

let format_latex metrics = ""

let gather_metrics ~ground_truth cfg metrics =
  let metrics = Option.value metrics ~default:{
      detected_insn_count = 0;
      false_negatives     = 0;
      false_positives     = 0;
      extenuating_loss    = None;
      total_actual_insns  = None;      
    } in
  let function_starts = Insn_disasm_benchmark.ground_truth_of_unstripped_bin
      ground_truth |> ok_exn in
  let (ground_truth, total_actual_insns) =
    Seq.fold ~init:(Addr.Set.empty, 0)
      function_starts ~f:(fun (all, total) tp -> 
          Set.add all tp,total+1) in
  let detected_insns = 
    G.fold_vertex 
      (fun vert detected_insns -> Set.add detected_insns vert) 
      cfg Addr.Set.empty in
  let false_negatives =
    Set.length (Set.diff ground_truth detected_insns) in
  let false_positives =
    Set.length (Set.diff detected_insns ground_truth) in
  let detected_insn_count = G.nb_vertex cfg in
  let extenuating_loss = None in
  let total_actual_insns = Some (total_actual_insns) in
  Some ({
      detected_insn_count = 
        detected_insn_count + 
        metrics.detected_insn_count;
      false_positives=false_positives + 
                      metrics.false_positives;
      false_negatives=false_negatives + 
                      metrics.false_negatives;
      extenuating_loss;
      total_actual_insns;
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
