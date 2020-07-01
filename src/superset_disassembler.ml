open Core_kernel
open Bap.Std
open Regular.Std
open Format
open Bap_knowledge.Knowledge
open Bap_future.Std

include Self()

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P
        "Runs the superset disassembler along with any
                  optional trimming components to reduce false
                  positives to an ideal minimum. ";
    ] in
  let deps = [ ] in
  let doc = "The file that houses the types ground truth" in
  let threshold = Config.(param string "threshold" ~doc) in
  let doc = "The file that houses the points-to ground truth" in
  let features = Config.(param string "features" ~doc) in
  Config.when_ready (fun {Config.get=(!)} ->
      (* the superset disassembler can't be registered as a pass
         because by that time the original disassembler has already run and
         built the IR *)
      let superdisasm proj =
        Stream.observe Project.Info.code (fun code ->
            ()
          ) in
      Project.register_pass' ~deps (superdisasm)
    )
