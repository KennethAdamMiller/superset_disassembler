open Core_kernel
open Bap.Std
open Regular.Std
open Format
open Bap_knowledge
open Bap_future.Std
open Cmdoptions

include Self()

let superdisasm features threshold rounds save load calc_gt trimmer
      oformat =
  info "superset disasm";
  let req = Stream.zip Project.Info.arch Project.Info.code in
  Stream.observe req (fun (arch,cd) ->
      info "Stream.observe (arch,code)";
      let codes = (Memmap.to_sequence cd) in
      let superset = Superset.Core.empty arch in
      let superset =
        Sequence.fold ~init:superset codes
          ~f:(fun superset (mem,v) ->
            Superset.Core.update_with_mem superset mem
          ) in
      let superset = Invariants.tag_superset superset in
      let superset = Trim.Default.trim superset in
      (* apply features *)
      let superset =
        Features.apply_featureset features superset in
      let superset =
        Features.apply_featurepmap features superset in
      let superset = Trim.Default.trim superset in
      let selections = Knowledge.empty in
      ()
    )

(* TODO write the other command line arguments *)
  
let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P
        "Runs the superset disassembler along with any
                  optional trimming components to reduce false
                  positives to an ideal minimum. ";
    ] in
  let deps = [ ] in
  let doc = "Number of analysis cycles" in
  let rounds = Config.(param (some int) "rounds" ~doc) in
  let threshold = Config.(param (some float) "threshold" ~doc) in
  let doc = "The file that houses the points-to ground truth" in
  let features =
    Config.(param (list string)
              ~default:Features.default_features "features" ~doc) in
  let doc = "Save target, one of truth, or some superset" in
  let savetgt = Config.(param (list string) "savetgt" ~default:[] ~doc) in
  let doc = "Load from a previous disassembly" in
  let load = Config.(param (some string) "load" ~default:None ~doc) in
  let doc = "Specify the desired invariants to apply to the superset" in
  let invariants =
    Config.(param (some (list @@ enum list_phases))
              "invariants" ~default:(Some[Default]) ~doc) in
  let doc = "Calculate the ground truth from the superset" in
  let calc_gt = Config.(flag "calc_gt" ~doc) in
  let doc =
    "There are options of trimmer, which is the false positives removal method." in
  let trimmer =
    Config.(param (string) "trimmer" ~default:"Simple" ~doc) in
  let doc = "The output format of choice" in
  let oformat = Config.(param (some string) "out_format" ~default:None
                          ~doc) in
  Config.when_ready (fun {Config.get=(!)} ->
      superdisasm !features !threshold !rounds !savetgt !load
                     !invariants !calc_gt !trimmer !oformat
      $ground_truth_bin
      $metrics_format
       $setops $cut
      $retain_at 
    )
