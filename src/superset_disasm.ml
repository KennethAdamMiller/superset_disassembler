open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std
open Or_error
open Format
open Cmdoptions
open Metrics
open Metrics.Opts

let () = Pervasives.ignore(Plugins.load ())

let time ?(name="") f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "%s execution time: %fs\n" name (Sys.time() -. t);
  fx

module Program(Conf : Provider)  = struct
  open Conf

  let main () =
    let analyses = Int.Map.empty in
    let phases = Option.value options.phases ~default:[] in
    let backend = options.disassembler in
    let checkpoint = options.checkpoint in
    let format = match options.metrics_format with
      | Latex -> format_latex
      | Standard -> format_standard in
    let non_insn_idx = 4 in
    let tag_grammar ?min_size = 
      Grammar.tag_by_traversal ?threshold:None in
    let analyses = 
      List.fold ~init:analyses phases ~f:(fun analyses phase -> 
          match phase with
          | Default -> 
            let analyses = 
              List.foldi ~init:analyses Trim.default_tags
                ~f:(fun idx analyses tag_func ->
                    Map.add analyses idx (Some(tag_func), None, None))
            in
            let analyses = 
              Map.add analyses (Map.length analyses)
                (None, Some(Sheathed.tag_loop_contradictions), None) in
            Map.add analyses (Map.length analyses)
              (None, Some(tag_grammar), None)
          | Target_not_in_memory -> 
            Map.add analyses 1 
              (Some(Trim.tag_target_not_in_mem), None, None)
          | Target_within_body -> 
            Map.add analyses 2 (Some(Trim.tag_target_in_body), None, None)
          | Invalid_memory_access -> 
            Map.add analyses 3 (Some(Trim.tag_non_mem_access), None, None)
          | Non_instruction ->
            Map.add analyses non_insn_idx (Some(Trim.tag_non_insn), None, None)
          | Component_body -> 
            Map.add analyses 5 
              (None, Some(Sheathed.tag_loop_contradictions), None)
          | Cross_layer_invalidation ->
            let discard_arg ?min_size = 
              Invariants.tag_layer_violations in
            Map.add analyses 6
              (None, Some(discard_arg), None)
          | Grammar_convergent -> 
            Map.add analyses 7
              (None, Some(tag_grammar), None)
          | Tree_set -> 
            Map.add analyses 8
              (None, None, Some(Decision_tree_set.decision_trees_of_superset))
        ) in
    let collect_analyses analyses = 
      let x, y, z = 
        Map.fold ~init:([], [], []) analyses 
          ~f:(fun ~key ~data (tag_funcs, analysis_funcs, dset) -> 
              let (tag_func, analysis_func, make_tree) = data in
              let tag_func = Option.value tag_func 
                  ~default:(fun superset _ _ _ -> superset) in
              let make_tree = Option.value make_tree ~default:(fun _ -> []) in
              let analysis_func = 
                Option.value analysis_func 
                  ~default:(fun ?min_size -> ident) in
              (tag_func :: tag_funcs),
              (analysis_func :: analysis_funcs),
              make_tree :: dset
            ) in
      List.rev x, List.rev y, List.rev z in
    (* Instructions cannot be saved, so we skip the process of both
       lifting them and therefore of removing them, since it is
       assumed that there isn't a need to. *)
    let apply_analyses analyses superset =
      let (tag_funcs, analysis_funcs, make_tree) =
        collect_analyses analyses in
      let superset = 
        Trim.tag_superset superset ~invariants:tag_funcs in
      List.fold ~init:superset analysis_funcs 
        ~f:(fun superset analyze -> analyze superset) in
    let checkpoint dis_method bin = 
      match checkpoint with
      | Some Import -> 
        let superset = time ~name:"import" Superset.import bin in
        let analyses = Map.remove analyses non_insn_idx in
        apply_analyses analyses superset
      | Some Export ->
        let (tag_funcs, analysis_funcs, make_tree) =
          collect_analyses analyses in
        let superset = dis_method tag_funcs bin in
        let superset = List.foldi ~init:superset analysis_funcs 
            ~f:(fun idx superset analyze -> 
                let name = sprintf "analysis %d" idx in
                time ~name analyze superset) in
        Superset.export bin superset;
        superset
      | Some Update ->
        let superset = Superset.import bin in
        let analyses = Map.remove analyses non_insn_idx in
        let superset = apply_analyses analyses superset in
        Superset.export bin superset;
        superset
      | None ->
        let (tag_funcs, analysis_funcs, make_tree) =
          collect_analyses analyses in
        let superset = dis_method tag_funcs bin in
        List.fold ~init:superset analysis_funcs 
          ~f:(fun superset analyze -> 
              time analyze superset)
    in
    let dis_method tag_funcs x =
      let f = Trim.tagged_disasm_of_file 
          ~invariants:tag_funcs
          ~data:Addr.Map.empty ~f:[Trim.add_to_map] ~backend in
      time ~name:"disasm binary" f x
    in
    let superset = 
      checkpoint dis_method options.target in
    let superset = Trim.trim superset in
    (match options.ground_truth with
     | Some bin -> 
       gather_metrics ~bin superset |> format |> print_endline
     | None -> ());

end

module Cmdline = struct
  open Cmdliner
  open Insn_disasm_benchmark

  let create 
      checkpoint disassembler ground_truth target
      metrics_format phases = 
    Fields.create ~checkpoint ~disassembler ~ground_truth ~target 
      ~metrics_format ~phases

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
          $checkpoint $(disassembler ()) $ground_truth $target
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
