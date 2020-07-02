open Core_kernel
open Bap.Std
open Regular.Std
open Format
open Bap_knowledge.Knowledge
open Bap_future.Std

include Self()

let superdisasm () =
  info "superset disasm";
  Stream.observe Project.Info.code (fun cd ->
      info "Stream.observe code ran";
      let codes = (Memmap.to_sequence cd) in
      let _ = Sequence.fold ~init:(false,None) codes
          ~f:(fun (status,prev) (mem,v) ->
              let cmin = (Memory.min_addr mem) in
              info "Memory starting at: %s, for %d"
                Addr.(to_string cmin) Memory.(length mem);
              if not status then (
                match prev with
                | None ->
                  (status, (Some cmin))
                | Some prev ->
                  if not Addr.(equal (succ prev) cmin) then (
                    info "There are skips %s" (Addr.to_string prev);
                    (true,Some prev)
                  ) else (status,Some cmin)
              ) else (status,prev)
            ) in
      ()
    )

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
  Config.when_ready (fun {Config.get=(!!)} ->
      (* the superset disassembler can't be registered as a pass
         because by that time the original disassembler has already run and
         built the IR *)
      superdisasm ()
    )
