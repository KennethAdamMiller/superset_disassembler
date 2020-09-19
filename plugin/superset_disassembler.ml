open Core_kernel
open Bap.Std
open Regular.Std
open Bap_knowledge
open Bap_core_theory
open Monads.Std
open Cmdoptions

include Self()
module Dis = Disasm_expert.Basic

let () =
  let open KB.Syntax in
  Theory.declare ~name:"is-valid" (
      Theory.instance () >>=
        Theory.require >>|
        fun (module Base) : Theory.core -> (module struct
                                             include Base
                                             let add x y =
                                               printf "add is called!\n%!";
                                               add x y
                                           end)
    )
           
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
      Superset.ISG.iter_vertex superset (fun v ->
          (* report the remaining as truths to the knowledge base *)
          ()
        )
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
  let rounds = Config.(param (int) ~default:1 "rounds" ~doc) in
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
    Config.(param (some (enum list_trimmers)) "trimmer" ~default:(Some Simple) ~doc) in
  let doc = "The output format of choice" in
  let oformat = Config.(param (some string) "out_format" ~default:None
                          ~doc) in
  Config.when_ready (fun {Config.get=(!)} ->
      let options =
        Fields.create ~checkpoint ~disassembler ~ground_truth_bin
          ~ground_truth_file ~target ~metrics_format ~phases ~trim_method
          ~setops ~cut ~save_dot ~save_gt ~save_addrs ~tp_threshold
          ~rounds ~featureset in
      superdisasm options
    )
