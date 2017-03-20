open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std
open Or_error
open Format
open Cmdoptions
open Metrics
open Metrics.Opts

let () = Pervasives.ignore(Plugins.load ())

type sheathed_disasm = 
  | Tree_set
  | Sheered_tree_set 

module Program(Conf : Provider with type kind = sheathed_disasm)  = struct
  open Conf

  let main () =
    let backend = options.disassembler in
    let sheathed = match options.disasm_method with
      | Tree_set -> Sheathed.sheaths_of_file ~backend
      | Sheered_tree_set -> Sheathed.sheered_sheaths_of_file ~backend
    in
    (* TODO result of sheathed disasm functions types are not
       consistent with `execute_disasm options sheathed` being able to
       use the results *)
    let format = match options.metrics_format with
      | Latex -> format_latex
      | Standard -> format_standard in
    let collect accu bin =
      let (insns, cfg, decision_trees) = sheathed bin in
      (match options.content with
       | Some content -> 
         List.iter content ~f:(function
             | Cfg -> Insn_cfg.Gml.print std_formatter cfg
             | Insn_map -> 
               let map_str = Sexp.to_string
                 @@ Addr.Map.sexp_of_t 
                   (Tuple2.sexp_of_t Memory.sexp_of_t
                      (Option.sexp_of_t @@ Disasm_expert.Basic.Insn.sexp_of_t))
                   insns in
               print_endline map_str
           )
       | None -> ());
      match options.ground_truth with
      | Some ground_truth -> 
        gather_metrics ~ground_truth cfg accu
      | None -> accu in
    (* TODO Should explore the possibility to abuse the cmd options
       by passing in the wrong filesystem kinds to the options *)
    match options.input_kind with
    | Binary bin -> 
      (* Output the results of disassembly *)
      let metrics = collect None bin in
      format metrics |> print_endline
    | Corpora_folder corpdir -> 
      print_endline @@ format @@ 
      Common.process_corpora ~corpdir collect

end

module Cmdline = struct
  open Cmdliner
  open Insn_disasm_benchmark

  let list_disasm_methods = [
    "tree_set", Tree_set;
    "sheered_tree_set" , Sheered_tree_set;
  ]
  let list_disasm_methods_doc = sprintf
      "Select of the the following disassembly methods: %s" @@ 
    Arg.doc_alts_enum list_disasm_methods
  let disasm_method = 
    Arg.(required & opt (some (enum list_disasm_methods))
           (Some Sheered_tree_set) 
         & info ["method"] ~doc:list_disasm_methods_doc)

  let create content disassembler ground_truth input_kind disasm_method metrics_format = 
    Fields.create ~content ~disassembler ~ground_truth ~input_kind ~disasm_method ~metrics_format

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

  (* TODO can probably move this into provider *)
  let program () =
    let doc = "Extended sheath sheering superset disassembler" in
    let man = [
      `S "SYNOPSIS";
      `Pre "
 $(b,$mname) [FORMAT/METRICS/DISASM_METHOD OPTION]
  [--ground_truth=FILE/DIR] --target=FILE/DIR ";
      `S "DESCRIPTION";
      `P
        "Given a binary, or a corpora folder location, will
    disassemble using the extended sheering techniques according to
    what is sepecified before formatting as requested and finally
    releasing that information to the specified location.";
      `S "OPTIONS";
    ](* TODO test and curate the following for version, help and other opations
        @ Bap_cmdline_terms.common_loader_options *) 
    in
    Term.(const create 
          $content $(disassembler ()) $ground_truth $input_kind $disasm_method
          $metrics_format),
    Term.info "sheathed_disasm" ~doc ~man ~version:Config.version

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
      type kind = sheathed_disasm
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
