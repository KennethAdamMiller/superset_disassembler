open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std
open Or_error
open Format
open Cmdoptions
open Metrics
open Metrics.Opts

let () = Pervasives.ignore(Plugins.load ())

type superset_disasm =
  | Superset_disasm
  | Trimmed_disasm
  | Tree_set
  | Trimmed_tree_set 
  | Grammar_convergent

let import bin = 
  let graph_str = In_channel.read_all (bin ^ ".graph") in
  let insn_rcfg = Superset_rcfg.Gml.parse graph_str in
  let map_str   = In_channel.read_all (bin ^ ".map") in
  let insn_map  = Superset.insn_map_of_string map_str in
  let meta_str  = In_channel.read_all (bin ^ ".meta") in
  let arch      = Superset.meta_of_string meta_str in
  let superset  = Superset.create ~insn_rcfg arch insn_map in
  superset, Decision_tree_set.(decision_trees_of_superset superset)

let export bin superset trees = 
  let graph_f   = Out_channel.create (bin ^ ".graph") in
  let formatter = Format.formatter_of_out_channel graph_f in
  let open Superset in
  let () = Superset_rcfg.Gml.print formatter superset.insn_rcfg in
  let () = Out_channel.close graph_f in
  let insn_map = Superset.get_data superset in
  let map_str  = Superset.insn_map_to_string insn_map in
  Out_channel.write_all (bin ^ ".map") ~data:map_str;
  let meta_str  = Superset.meta_to_string superset in
  Out_channel.write_all (bin ^ ".meta") ~data:meta_str;

module Program(Conf : Provider with type kind = superset_disasm)  = struct
  open Conf

  let main () =
    let backend = options.disassembler in
    let checkpoint = options.checkpoint in
    let dis_method =
      match options.disasm_method with
      | Superset_disasm -> 
        (fun x -> Superset.superset_disasm_of_file 
            ~data:Addr.Map.empty ~f:(fun (mem, insn) superset ->
                Trim.add_to_map superset mem insn []) ~backend x, None)
      | Trimmed_disasm -> (fun x -> 
          Sheathed.trimmed_disasm_of_file ~backend x, None)
      | Tree_set -> (fun x -> 
          let superset, trees = 
            Sheathed.sheaths_of_file ~backend x in 
          superset, Some(trees))
      | Trimmed_tree_set -> (fun x -> 
          let superset, trees = 
            Sheathed.trimmed_sheaths_of_file ~backend x in
          superset, Some(trees))
      | Grammar_convergent ->
        (fun x -> 
           let superset, trees = 
             Grammar.trimmed_disasm_of_file ~backend x in
           superset, Some(trees))
    in
    let dis_method bin = 
      match checkpoint with
      | Some Import -> 
        let superset, trees = import bin in
        superset, Some(trees)
      | Some Export -> 
        let superset, trees = dis_method bin in
        export bin superset trees;
        superset, trees
      | None -> 
        dis_method bin 
    in
    let format = match options.metrics_format with
      | Latex -> format_latex
      | Standard -> format_standard in
    let (superset, decision_trees) = dis_method options.target in
    (* Need to have options for specifying where the output goes *)
    let open Superset in
    let insn_map = Map.filteri ~f:(fun ~key ~data -> 
        let vert = key in
        let (mem, insn) = data in
        Option.is_some insn && 
        Superset_rcfg.G.(mem_vertex superset.insn_rcfg vert)) 
        (get_data superset) in
    let superset = Superset.rebuild ~data:insn_map superset in
    (match options.content with
     | Some content -> 
       List.iter content ~f:(function
           | Cfg -> 
             Superset.format_cfg ~format:Format.std_formatter superset
           | Insn_map -> 
             print_endline 
               Superset.(insn_map_to_string (get_data superset)))
     | None -> ());
    (match options.ground_truth with
     | Some bin -> 
       gather_metrics ~bin superset |> format |> print_endline
     | None -> ());
    printf "superset_map length %d graph size: %d\n\n" 
      Addr.Map.(length insn_map) (Superset_rcfg.G.nb_vertex superset.insn_rcfg)

end

module Cmdline = struct
  open Cmdliner
  open Insn_disasm_benchmark

  let list_disasm_methods = [
    "superset", Superset_disasm;
    "trimmed", Trimmed_disasm;
    "tree_set", Tree_set;
    "trimmed_tree_set" , Trimmed_tree_set;
    "grammar", Grammar_convergent;
  ]
  let list_disasm_methods_doc = sprintf
      "Select of the the following disassembly methods: %s" @@ 
    Arg.doc_alts_enum list_disasm_methods
  let disasm_method = 
    Arg.(required & opt (some (enum list_disasm_methods))
           (Some Trimmed_tree_set) 
         & info ["method"] ~doc:list_disasm_methods_doc)

  let create 
      checkpoint content disassembler ground_truth target
      disasm_method metrics_format phases = 
    Fields.create ~checkpoint ~content ~disassembler ~ground_truth ~target 
      ~disasm_method ~metrics_format ~phases

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
    disassemble using the extended sheering techniques according to
    what is sepecified before formatting as requested and finally
    releasing that information to the specified location.";
      `S "OPTIONS";
    ] in
    Term.(const create 
          $checkpoint $content $(disassembler ()) $ground_truth $target $disasm_method
          $metrics_format $phases),
    Term.info "superset_disasm" ~doc ~man ~version:Config.version

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
      type kind = superset_disasm
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
  | No_input -> exitf (-2) "Could not read rom stdin"
  | Unknown_arch -> 
    exitf (-2) "Unknown arch. Supported architectures:\n%s"
      (String.concat ~sep:"\n" @@ List.map Arch.all ~f:Arch.to_string)
