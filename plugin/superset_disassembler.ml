open Core_kernel
open Bap.Std
open Regular.Std
open Bap_knowledge
open Bap_core_theory
open Monads.Std
open Cmdoptions

include Self()
module Dis = Disasm_expert.Basic
           
let superdisasm options =
  let open Bap_future.Std in
  let open Format in
  let module Program =
    With_options(struct
        let options = options
      end) in
  let open Program in
  let req = Stream.zip Project.Info.arch Project.Info.code in
  Stream.observe req (fun (arch,cd) ->
      let codes = (Memmap.to_sequence cd) in
      let superset = Superset.Core.empty arch in
      let superset =
        Sequence.fold ~init:superset codes
          ~f:(fun superset (mem,v) ->
            Superset.Core.update_with_mem superset mem
          ) in
      let superset = with_options superset in
      KB.promise Theory.Label.is_valid
      @@ fun label ->
         let open KB.Syntax in
         Theory.Label.target label >>= fun tgt ->
         KB.collect Theory.Label.addr label >>= fun addr ->
         match addr with
         | Some addr ->
            let addr = (Word.code_addr tgt addr) in
            KB.return @@ Some (Superset.Core.mem superset addr)
         | None -> KB.return None
    )
  
let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P
        "Runs the superset disassembler along with any
                  optional trimming components to reduce false
                  positives to an ideal minimum. ";
    ] in
  let doc = "Number of analysis cycles" in
  let rounds = Config.(param (int) ~default:2 "rounds" ~doc) in
  let tp_threshold = Config.(param (some float) "threshold" ~doc) in
  let doc = "The file that houses the points-to ground truth" in
  let featureset =
    Config.(param (list string)
              ~default:Features.default_features "features" ~doc) in
  (*let doc = "Save target, one of truth, or some superset" in
  let savetgt = Config.(param (list string) "savetgt" ~default:[] ~doc) in*)
  (*let doc = "Load from a previous disassembly" in
  let load = Config.(param (some string) "load" ~default:None ~doc) in*)
  let doc = "Specify the desired invariants to apply to the superset" in
  let invariants =
    Config.(param ((list @@ enum Invariants.default_tags))
              "invariants" ~default:(Invariants.default_funcs) ~doc) in
  let doc =
    "The false positives removal proceedure may be changed." in
  let trim_method =
    Config.(param ((enum list_trimmers))
              "trimmer" ~default:(Trim.Default.trim) ~doc) in
  (*let doc = "The output format of choice" in
  let oformat =
    Config.(param (some string) "out_format" ~default:None ~doc) in*)
  let checkpoint =
    Config.(param (some (enum list_checkpoints)) "checkpoint"
              ~default:None
              ~doc:"Import or Export the disassembly graph and map.") in
  let disassembler =
    Config.(param string "backend" ~default:"llvm"
              ~doc:"Backend disassembler") in
  let ground_truth_bin =
    Config.(param (some string) ~default:None "ground_truth_bin"
              ~doc:("Compare results against a ground truth if desired," ^
                      " of either debug symbols or an unstripped binary")) in
  let ground_truth_file =
    Config.(param (some string) ~default:None "ground_truth_file"
              ~doc:("Compare results against a file that contains the " ^
                      "addresses of true positives")) in
  let target =
    Config.(param string "target" ~doc:"Specify target binary") in
  let metrics_format =
    let open Metrics in
    let list_formats_types = [
        "standard", Standard;
        "latex", Latex;
      ] in
    let list_formats_doc = sprintf
                             "Available output metrics formats: %s" @@ 
                             Cmdliner.Arg.doc_alts_enum list_formats_types in
    Config.(param ((enum list_formats_types)) ~default:(Standard)
              "metrics_format" ~doc:list_formats_doc) in
  let analyses = Config.(param (list (enum list_analyses))
                           ~default:[Sheathed.tag_loop_contradictions]
                           "analyses") in
  let save_addrs = Config.(flag "save_addrs") in
  let save_gt = Config.(flag "save_gt") in
  let save_dot = Config.(flag "save_dot") in
  let cut = Config.(param (some (t3 (enum list_cuts) string int))
                      ~default:None "cut") in
  let collect_report = Config.(flag "collect_reports") in
  let dforest = Config.(param (some (enum list_decision_trees))
                  ~default:None "decision_tree") in
  Config.when_ready (fun {Config.get=(!)} ->
      let checkpoint = !checkpoint in
      let disassembler = !disassembler in
      let ground_truth_bin = !ground_truth_bin in
      let ground_truth_file = !ground_truth_file in
      let target = !target in
      let metrics_format = !metrics_format in
      let phases = !invariants in
      let analyses = !analyses in
      let trim_method = !trim_method in
      let tp_threshold = !tp_threshold in
      let featureset = !featureset in
      let save_addrs = !save_addrs in
      let save_gt = !save_gt in
      let save_dot = !save_dot in
      let rounds = !rounds in
      let cut = !cut in
      let collect_report = !collect_report in
      let dforest = !dforest in
      let options =
        Fields.create ~checkpoint ~disassembler ~ground_truth_bin
          ~ground_truth_file ~target ~metrics_format ~phases ~trim_method
          ~setops:[] ~cut ~save_dot ~save_gt ~save_addrs ~tp_threshold
          ~rounds ~featureset ~analyses ~collect_report ~dforest in
      superdisasm options
    )
