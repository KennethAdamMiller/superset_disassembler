open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std
open Or_error
open Format
open Cmdoptions
open Metrics
open Metrics.Opts

let () = Pervasives.ignore(Plugins.load ())

let make_gatherer accu = 
  let module Instance = MetricsGatheringReducer(struct
      type acc = (Addr.Hash_set.t * Superset_risg.t )
      let accu = accu
    end) in
  let module Instance = Trim.Reduction(Instance) in
  Instance.trim 

let build_metrics trim phases =
  List.fold phases ~init:(trim,String.Map.empty)
    ~f:(fun (trim,metrics) phase ->
        match List.find list_phases ~f:(fun (name,p) ->
            phase = p
          ) with
        | Some (name,p) ->
          let accu = (Addr.Hash_set.create () ,
                      Superset_risg.G.create ()) in
          let instance_trim = make_gatherer accu in         
          (fun x -> instance_trim @@ trim x), Map.add metrics name accu
        | None -> trim,metrics
      )

let build_setops trim setops =
  List.fold setops ~init:(trim,String.Map.empty)
    ~f:(fun (trim,metrics) (colr, (sop, (f1, f2))) ->
        let add (trim,metrics) f =
          if Map.mem metrics f then
            trim, metrics
          else
            let accu = (Addr.Hash_set.create () ,
                        Superset_risg.G.create ()) in
            let instance_trim = make_gatherer accu in
            let metrics = Map.add metrics f accu in
            ((fun x -> instance_trim @@ trim x), metrics) in
        add (add (trim,metrics) f1) f2
      )

let apply_setops metrics setops =
  let report_err colr f1 =
    Format.eprintf
      "Feature %s not found, error for color %s!" f1 colr in
  List.fold setops ~init:String.Map.empty
    ~f:(fun results (colr, (sop, (f1, f2))) -> 
        match Map.find metrics f1, Map.find metrics f2 with
        | Some (fmetric1,g1), Some (fmetric2,g2) -> (
            match sop with
            | Difference ->
              let s = Addr.Hash_set.create () in
              Hash_set.iter fmetric1 ~f:(fun fv ->
                  if not (Hash_set.mem fmetric2 fv) then
                    Hash_set.add s fv
                );
              Map.add results colr s
            | Union ->
              let s = Addr.Hash_set.create () in
              Hash_set.iter fmetric1 ~f:(fun fv ->
                  Hash_set.add s fv
                );
              Hash_set.iter fmetric2 ~f:(fun fv ->
                  Hash_set.add s fv
                );
              Map.add results colr s
            | Intersection ->
              let s = Addr.Hash_set.create () in
              Hash_set.iter fmetric1 ~f:(fun fv ->
                  if (Hash_set.mem fmetric2 fv) then
                    Hash_set.add s fv
                );
              Hash_set.iter fmetric2 ~f:(fun fv ->
                  if (Hash_set.mem fmetric1 fv) then
                    Hash_set.add s fv
                );
              Map.add results colr s
          )
        | None, None ->
          report_err colr f1;
          report_err colr f2;
          results
        | None, _ ->
          report_err colr f1;
          results
        | _, None ->
          report_err colr f2;
          results
      ) 

let take_random superset =
  let insn_map = Superset.get_map superset in
  let insn_risg = Superset.get_graph superset in
  let minimum_depth = 200 in
  let rec get_deep () =
    let r = Random.int (Map.length insn_map) in
    let x = List.nth Map.(keys insn_map) r |> Option.value_exn in
    let depth = Superset_risg.get_depth insn_risg x in
    if depth < minimum_depth then
      get_deep ()
    else
      x
  in get_deep ()

let process_cut superset options results =
  let insn_risg = Superset.get_graph superset in  
  match options.cut with 
  | None ->
    if options.save_dot then
      Metrics.print_dot superset results;
  | Some cut -> (
      let cut =
        let c, addr, len = cut in
        let addr =
          match addr with
          | "random" -> take_random superset
          | "lowest" ->
            let insn_map = Superset.get_map superset in
            let addr,_ = Map.min_elt insn_map |> Option.value_exn in
            addr
          | "highest" ->
            let insn_map = Superset.get_map superset in
            let addr,_ = Map.max_elt insn_map |> Option.value_exn in
            addr
          | _ -> Addr.of_string addr in
        c,addr,len in
      match cut with
      | DFS, addr, len ->
        let subgraph = Addr.Hash_set.create () in
        let depth = ref 0 in
        let post _ =
          depth := !depth - 1; in
        let pre addr =
          depth := !depth + 1;
          Hash_set.add subgraph addr in
        let terminator _ =
          !depth < len &&
          Hash_set.(length subgraph) < len in
        Superset_risg.iter_component ~terminator ~pre ~post insn_risg
          addr;
        let sg = Superset_risg.subgraph insn_risg subgraph in
        let superset = Superset.(rebuild ~insn_risg:sg superset) in
        Metrics.print_dot superset results
      | Interval, addr, len ->
        let subgraph = Addr.Hash_set.create () in
        let add x =
          if Addr.(addr <= x) && Addr.(x <= (addr ++ len)) then
            Hash_set.add subgraph x in
        Superset_risg.G.iter_vertex add insn_risg;
        let sg = Superset_risg.subgraph insn_risg subgraph in
        let superset = Superset.(rebuild ~insn_risg:sg superset) in
        Metrics.print_dot superset results
    )

let converge featureset superset =
  let superset = Features.apply_featureset featureset superset in
  Features.apply_featurepmap featureset superset

module Program(Conf : Provider)  = struct
  open Conf

  let trim_with f superset =
    let visited = Addr.Hash_set.create () in
    let datas = Addr.Hash_set.create () in
    let rec do_analysis round superset = 
      if round = options.rounds then superset else
        let superset = 
          let superset, pmap = f superset, Addr.Map.empty in
          Markup.mark_threshold_with_pmap
            superset ~visited ~datas pmap 
            options.tp_threshold;
          Markup.enforce_uncertain
            superset visited datas (ref pmap);
          Markup.check_convergence superset visited;
          Trim.Default.trim superset in
        do_analysis (round+1) superset in
    do_analysis 0 superset

  let main () =
    Random.self_init ();
    let phases = Option.value options.phases ~default:[] in
    let backend = options.disassembler in
    let format = match options.metrics_format with
      | Latex -> format_latex
      | Standard -> format_standard in
    let _ = 
      if options.save_gt then
        let gt = Insn_disasm_benchmark.ground_truth_of_unstripped_bin
            options.target |> ok_exn in
        let gt = Seq.map gt ~f:Addr.to_string in
        Seq.iter gt ~f:print_endline;
        exit 0
      else
        () in
    let checkpoint dis_method bin = 
      match options.checkpoint with
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
    let trim = select_trimmer options.trim_method in
    let (trim,metrics) = build_metrics trim phases in
    let (trim,setops) = build_setops trim options.setops in
    let superset = 
      checkpoint dis_method options.target in
    let superset = trim superset in
    let superset = trim_with (converge options.featureset) superset in
    let _ =
      if options.save_addrs then
        Superset.export_addrs options.target superset
      else () in
    let results = apply_setops setops options.setops in
    let results =
      match options.ground_truth_file with
      | Some (gt) ->
        let insn_map = Superset.get_map superset in
        let remaining = Addr.Hash_set.create () in
        Map.iter_keys insn_map ~f:(Hash_set.add remaining);
        let s = sprintf "metrics - %d" Map.(length metrics) in
        print_endline s;
        let s = sprintf "setops - %d" Map.(length setops) in
        print_endline s;
        let s = sprintf "results - %d" Map.(length results) in
        print_endline s;
        Map.iteri setops ~f:(fun ~key ~data ->
            let data,_ = data in
            let s = sprintf "%s - %d" key Hash_set.(length data) in
            print_endline s;
            Hash_set.iter data ~f:(Hash_set.remove remaining);
          );
        let min_addr,_ = Map.min_elt insn_map |> Option.value_exn in
        let tp = read_addrs Addr.(bitwidth min_addr) gt in
        let tps = Addr.Hash_set.create () in
        List.iter tp ~f:(Hash_set.add tps);
        let fps = Addr.Hash_set.create () in
        let _ = Hash_set.iter remaining ~f:(fun key -> 
            if not (Hash_set.mem tps key) then
              Hash_set.add fps key
          ) in
        let fns = Addr.Hash_set.create () in
        Hash_set.iter tps ~f:(fun v -> 
            if not (Hash_set.mem remaining v) then
              Hash_set.add fns v;
          );
        let s = sprintf "fps: %d" Hash_set.(length fps) in
        print_endline s;
        let s = sprintf "tps: %d" Hash_set.(length tps) in
        print_endline s;
        let results = String.Map.add results "True Positives" tps in
        let results = String.Map.add results "False Positives" fps in
        let results = String.Map.add results "False Negatives" fns in
        let s = sprintf "false negatives: %d" Hash_set.(length fns) in
        print_endline s;
        results
      | None -> results in
    process_cut superset options results;
    match options.ground_truth_bin with
    | Some bin -> 
      gather_metrics ~bin superset |> format |> print_endline
    | None -> ()

end

module Cmdline = struct
  open Cmdliner
  open Insn_disasm_benchmark

  let create 
      checkpoint disassembler ground_truth_bin ground_truth_file target
      metrics_format phases trim_method setops cut save_dot save_gt
      save_addrs tp_threshold retain_at rounds partition_method featureset = 
    Fields.create ~checkpoint ~disassembler ~ground_truth_bin
      ~ground_truth_file ~target ~metrics_format ~phases ~trim_method
      ~setops ~cut ~save_dot ~save_gt ~save_addrs ~tp_threshold ~
      retain_at ~rounds ~partition_method ~featureset

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
          $checkpoint $(disassembler ()) $ground_truth_bin
          $ground_truth_file $target $metrics_format $phases
          $trim_method $setops $cut $save_dot $save_gt $save_addrs
          $tp_threshold $retain_at $rounds $partition_method
          $featureset),
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
