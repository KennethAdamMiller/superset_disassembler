open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std
open Or_error
open Format
open Cmdoptions
open Metrics
open Metrics.Opts

let () = Pervasives.ignore(Plugins.load ())

module Program(Conf : Provider)  = struct
  open Conf

  let main () =
    let phases = Option.value options.phases ~default:[] in
    let backend = options.disassembler in
    let checkpoint = options.checkpoint in
    let trim =
      match options.trim_method with
      | Some Simple | None -> Trim.Default.trim
      (*| Some Memoried -> Trim.*)
      | Some DeadBlockResistant ->
        Trim.DeadblockTolerant.trim in
    let format = match options.metrics_format with
      | Latex -> format_latex
      | Standard -> format_standard in
    let checkpoint dis_method bin = 
      match checkpoint with
      | Some Import -> 
        let superset = time ~name:"import" Superset.import bin in
        let phases =
          List.filter phases
            ~f:(function | Non_instruction -> true | _ -> false) in
        with_phases superset phases
      | Some Export ->
        let superset = dis_method bin in
        let superset = with_phases superset phases in
        Superset.export bin superset;
        superset
      | Some Update ->
        let superset = Superset.import bin in
        let phases =
          List.filter phases
            ~f:(function | Non_instruction -> true | _ -> false) in
        let superset = with_phases superset phases in
        Superset.export bin superset;
        superset
      | None ->
        let superset = dis_method bin in
        with_phases superset phases
    in
    let dis_method x =
      let f x = Superset.superset_disasm_of_file
          ~data:() ~backend x
          (*~f:(Trim.tag ~invariants:[Trim.tag_success]) *) in
      time ~name:"disasm binary" f x
    in
    let superset = 
      checkpoint dis_method options.target in
    let superset = trim superset in
    let insn_map = Superset.get_map superset in
    let addrs = Map.keys insn_map in
    let addrs = List.map addrs ~f:Addr.to_string in
    let addrs_file = open_out (options.target ^ "_addrs.txt") in
    Out_channel.output_lines addrs_file addrs;
    match options.ground_truth with
    | Some bin -> 
      gather_metrics ~bin superset |> format |> print_endline
    | None -> ()

end

module Cmdline = struct
  open Cmdliner
  open Insn_disasm_benchmark

  let create 
      checkpoint disassembler ground_truth target
      metrics_format phases trim_method = 
    Fields.create ~checkpoint ~disassembler ~ground_truth ~target 
      ~metrics_format ~phases ~trim_method

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
    disassemble using the extended trimming techniques according to
    what is sepecified before formatting as requested and finally
    releasing that information to the specified location.";
      `S "OPTIONS";
    ] in
    Term.(const create 
          $checkpoint $(disassembler ()) $ground_truth $target
          $metrics_format $phases $trim_method),
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
  let module Program = Program(struct
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
