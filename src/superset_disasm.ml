open Core_kernel
open Bap.Std
open Bap_plugins.Std
open Cmdoptions
open Format
open Or_error
open Metrics

let () = Stdlib.ignore(Plugins.load ())
let _ = Bap_main.init ()

module Cmdline = struct
  open Cmdliner

  let create 
      import export disassembler ground_truth_bin ground_truth_file target
      metrics_format phases analyses trim_method cut setops save_dot save_gt
      save_addrs collect_report dforest tp_threshold rounds featureset = 
    Fields.create ~import ~export ~disassembler ~ground_truth_bin
      ~ground_truth_file ~target ~metrics_format ~phases ~analyses ~trim_method
      ~setops ~cut ~save_dot ~save_gt ~save_addrs ~collect_report
      ~dforest ~tp_threshold ~rounds ~featureset
    

  let disassembler () : string Term.t =
    Disasm_expert.Basic.available_backends () |>
    List.map ~f:(fun x -> x,x) |> function
    | [] -> Term.const "<no-disassemblers-available>"
    | [x,_] -> Term.const x
    | backends ->
      let doc = sprintf
          "Disassembler backend, should be %s" @@
        Arg.doc_alts_enum backends in
      Arg.(value & opt (enum backends) "llvm" & info ["disassembler"] ~doc)

  let program () =
    let doc = "Extended superset disassembler for constructing decision trees" in
    let man = [
      `S "SYNOPSIS";
      `Pre "$(b,$mname) [FORMAT/METRICS/DISASM_METHOD OPTION]
           [--ground_truth=FILE] [--phases=TRIM_PHASES] --target=FILE ";
      `S "DESCRIPTION";
      `P
        "Given a binary, this utility will
    disassemble, trim, and format as requested and finally
    output the results.";
      `S "OPTIONS";
    ] in
    Term.(const create 
          $import_superset $export_superset $(disassembler ()) $ground_truth_bin
          $ground_truth_file $target $Opts.metrics_fmt $invariants
          $analyses_opt $trimmer $cut_opt $setops_opt $save_dot $save_gt
          $save_addrs $collect_reports $decision_trees_opt
          $tp_threshold $rounds $featureset_opt),
    Term.info "superset_disasm" ~doc ~man

  let parse argv =
    match Term.eval ~argv (program ()) ~catch:false with
    | `Ok opts -> Ok opts
    | `Error `Parse -> exit 64
    | `Error _ -> exit 2
    | _ -> exit 1
end

let exitf n =
  kfprintf (fun ppf -> pp_print_newline ppf (); exit n) err_formatter

let start options =
  let module Program = With_options(struct
      let options = options
    end) in
  return @@ Program.main ()

let _main = 
  try match Cmdline.parse Sys.argv >>= start with
    | Ok _ -> exit 0
    | Error err -> exitf (-1) "%s\n" Error.(to_string_hum err)
  with
  | Bad_user_input -> 
    exitf (-2) "Could not parse: malformed input"
  | No_input -> exitf (-2) "Could not read from stdin"
  | Unknown_arch -> 
    exitf (-2) "Unknown arch. Supported architectures:\n%s"
      (String.concat ~sep:"\n" @@ List.map Arch.all ~f:Arch.to_string)
