open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std
open Or_error
open Format
open Metrics
open Cmdoptions

let () = Pervasives.ignore(Plugins.load ())

type shingled_disasm = | Trimmed_disasm
                       | Superset_disasm
[@@deriving sexp]

module Program(Conf : Provider with type kind = shingled_disasm)  = struct
  open Conf
  open Metrics

  let main () =
    let backend = options.disassembler in
    let disasm_with = match options.disasm_method with
      | Superset_disasm -> Shingled.superset_disasm_of_file ~backend
      | Trimmed_disasm ->  Shingled.trimmed_disasm_of_file  ~backend
    in
    let format = match options.metrics_format with
      | Latex -> format_latex
      | Standard -> format_standard in
    let collect accu bin =
      let (arch, insn_map, cfg) = disasm_with bin in
      (match options.content with
       | Some content -> 
         List.iter content ~f:(function
             | Cfg -> Insn_cfg.Gml.print std_formatter cfg
             | _ -> ())
       | None -> ());
      match options.ground_truth with
      | Some ground_truth -> 
        gather_metrics ~ground_truth cfg accu
      | None -> accu in
    match options.input_kind with
    | Binary bin -> 
      print_endline @@ format @@ 
      collect None bin
    | Corpora_folder corpdir -> 
      print_endline @@ format @@ 
      Common.process_corpora ~corpdir collect


end

module Cmdline = struct
  open Cmdliner
  open Metrics.Opts
  open Insn_disasm_benchmark

  let list_disasm_methods = [
    "superset", Superset_disasm;
    "trimmed" , Trimmed_disasm;
  ]
  let list_disasm_methods_doc = sprintf
      "Select of the the following disassembly methods: %s" @@ 
    Arg.doc_alts_enum list_disasm_methods
  let disasm_method = 
    Arg.(required & opt (some (enum list_disasm_methods))
           (Some Trimmed_disasm) 
         & info ["method"] ~doc:list_disasm_methods_doc)

  let create disassembler ground_truth input_kind 
      disasm_method metrics_format content = 
    Fields.create ~disassembler ~ground_truth ~input_kind ~
      disasm_method ~metrics_format ~content

  (* TODO eliminate this through bap usage *)
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
    let doc = "Extended shingled sheering superset disassembler" in
    let man = [
      `S "SYNOPSIS";
      `Pre "
 $(b,$mname) [FORMAT/METRICS/BACKEND/DISASM_METHOD OPTION] --target=FILE/DIR ";
      `S "DESCRIPTION";
      `P
        "Given a binary, or a corpora folder location, will
    disassemble using the extended sheering techniques according to
    what is sepecified to construct the output that is desired and
    release that information to the specified location.";
      `S "OPTIONS";
    ](* TODO test and curate the following for version, help and other opations
        @ Bap_cmdline_terms.common_loader_options *) 
    in
    Term.(const create 
          $(disassembler ()) $ground_truth $input_kind $disasm_method
          $metrics_format $content),
    Term.info "shingled_disasm" ~doc ~man ~version:Config.version


  let print_data_formats t = 
    match t with
    | Standard -> printf "Standard: print metrics output to stdout"
    | Latex -> printf "Latex: metrics output to a latex parsable data file"

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
  let module Program = Program(struct 
      type kind = shingled_disasm
      let options = options
    end) in
  return @@ Program.main ()

let _main = 
  (* TODO correct let argv = Bap_plugin_loader.run Sys.argv in*)
  try match Cmdline.parse Sys.argv >>= start with
    | Ok _ -> exit 0
    | Error err -> exitf (-1) "%s\n" Error.(to_string_hum err)
  with
  | Bad_user_input -> 
    exitf (-2) "Could not parse: malformed input"
  | No_input -> exitf (-2) "Could not read rom stdin"
  | Unknown_arch -> 
    exitf (-2) "Unknown arch. Supported architectures:\n%s"
      (String.concat ~sep:"\n" @@ List.map Arch.all ~f:Arch.to_string)
