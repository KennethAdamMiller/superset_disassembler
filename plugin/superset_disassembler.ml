open Core_kernel
open Bap.Std
open Regular.Std
open Bap_knowledge
open Bap_core_theory
open Monads.Std
open Cmdoptions
open Bap_main
open Bap_plugins.Std

include Self()
module Dis = Disasm_expert.Basic

let man = {|
  # DESCRIPTION

  Superset disassembly is a disassembly method in which every single
  byte offset within the executable region of a binary is initially
  treated as being potentially compiler intended output. However,
  after applying several rounds of heuristics the true positives, or
  the actually intended instructions, can be distinguished from the
  noise. It is an alternate disassembly method from linear sweep or
  recursive descent, the two (probably most) populate mainstream
  disassembly methods. This approach exchanges the possibility of
  some small portion of the final output including some occlusive
  unintended sequences being incorrectly kept (a superset) for the
  probabilistic guarantee of having no misses of those that are
  intended.

  Heuristics are broken into three main groups: invariants, analyses,
  and features. Invariants are ideally lawful characteristics of
  binary code, where disobedience is illegal for any well formed
  assembler, and run with a limited scope/visibility of just
  instructions. Analyses are typically processes that identify less
  visible violations of well-formed assembler rules or other lawful
  assembler characteristics that require global visibility. Features
  are data traits that may be dirty and require some iterative
  convergence to recognize the subset within the initial superset that
  can be guaranteeably cleansed. Once convergence occurs, the bodies
  of lineages with sufficient evidence are cleansed of occlusion, and
  any lineage that does not have enough features to support being kept
  is dropped.

  # ARGUMENTS

  Fundamental arguments specific to the superset disassembler include:
  rounds, loader, ground_truth_bin, ground_truth_file, target,
  invariants, analyses, trim_method, tp_threshold, save_addrs,
  save_gt, save_dot, collect_report

  # PASSES
   
  The passes are specified by the $(b,--pass) option and are run in the
  order in which they specified. In addition, all passes that are
  flagged with `autorun' are run before the explicitly specified
  passes. Finally, if a pass specifies other passes as its
  dependencies, then they are run before it, in the order in which
  they were specified (modulo their own dependencies).

  It's also possible to specify the passes using the old style syntax,
  e.g., `$(b,--<PASS>)`, which is discouraged and later could be disabled.
  Additionaly, it is not allowed to mix passes the old and the new
  style.

  # OUTPUT

  After all passes are run on the input, the resulting project data
  structure could be dumped using the $(b,--dump) (or $(b,-d) for short)
  option, whichaccepts the desired format and, optionally, the output
  file name.

  It is possible to specify the $(b,--dump) option multiple times, in
  which case the project will be dumped in several formats.

  ```
  bap superset_disasm /bin/echo -dbir:out.bir -dasm:out.asm
  
  ```
  |}
           
let superset_disasm options =
  print_endline "superset_disasm running";
  let open Bap_future.Std in
  let open Format in
  let module With_options =
    With_options(struct
        let options = options
      end) in
  (*let req = Stream.zip Project.Info.arch Project.Info.code in
  Stream.observe req (fun (arch,cd) ->
      let code = (Memmap.to_sequence cd) in
      print_endline @@ sprintf "Have %d mem segments"
                         Memmap.(length cd);*)
      let invariants =
        With_options.args_to_funcs options.phases
          Invariants.default_tags in
      let invariants = Invariants.tag_success :: invariants in
      let backend = options.disassembler in
      let superset = Superset.superset_disasm_of_file ~backend options.target
                       ~f:(Invariants.tag ~invariants) in
      let superset = Trim.Default.trim superset in
      (* TODO use the knowledge base to import the superset *)
      (*let superset =
        Sequence.fold ~init:superset code
          ~f:(fun superset (mem,v) ->
            print_endline @@ sprintf "disassembling with mem %s %d"
                               Addr.(to_string @@ Memory.min_addr mem)
                               Memory.(length mem);
            let f (mem, insn) superset =
              let superset = Superset.Core.add superset mem insn in
              Invariants.tag ~invariants:[Invariants.tag_success]
                 (mem,insn) superset in
            Superset.Core.update_with_mem superset mem ~f
          ) in*)
      (*let superset = Invariants.tag_superset ~invariants superset in*)
      let superset = With_options.with_options superset in
      (match options.ground_truth_bin with
       | Some bin ->
          let format = match options.metrics_format with
            | Latex -> Metrics.format_latex
            | Standard -> Metrics.format_standard in
         Metrics.gather_metrics ~bin superset |> format
         |> print_endline
      | None -> ());
      print_endline "disassembly finished! working with knowledge base";
      (* Provide the is_valid label as a check on whether a given
         address is in the superset after trimming *)
      KB.promise Theory.Label.is_valid
      @@ (fun label ->
         let open KB.Syntax in
         (* (target is just the machine target) *)
         Theory.Label.target label >>= fun tgt ->
         (* For each address the in the knowledge base *)
         (* Collect the is_valid's label address *)
         KB.collect Theory.Label.addr label >>= fun addr ->
         match addr with
         | Some addr ->
            let addr = (Word.code_addr tgt addr) in
            (* And return whether it should be kept or not *)
            KB.return @@ Some (Superset.Core.mem superset addr)
         | None -> KB.return None
      );
      print_endline "disassembly returning"
      (* ) *)

let features_used = [
  "disassembler";
  "lifter";
  "symbolizer";
  "brancher";
  "loader";
  "abi";
]

type failure =
  | Expects_a_regular_file of string
  | Old_and_new_style_passes
  | Unknown_pass of string
  | Incompatible_options of string * string
  | Project of Error.t
  | Pass of Project.Pass.error
  | Unknown_format of string
  | Unavailable_format_version of string
  | Unknown_collator of string
  | Unknown_analysis of string
  | No_knowledge

type Extension.Error.t += Fail of failure

module Err = Monad.Result.Make(Extension.Error)(Monad.Ident)
open Err.Syntax

let pass_error = Result.map_error ~f:(fun err -> Fail (Pass err))
let proj_error = Result.map_error ~f:(fun err -> Fail (Project err))

let run_passes base proj =
  Err.List.fold ~init:(0,proj) ~f:(fun (step,proj) pass ->
      report_progress
        ~stage:(step+base)
        ~note:(Project.Pass.name pass) ();
      Project.Pass.run pass proj |> pass_error >>= fun proj ->
      Ok (step+1,proj))

let knowledge_reader = Data.Read.create
    ~of_bigstring:Knowledge.of_bigstring ()

let knowledge_writer = Data.Write.create
    ~to_bigstring:Knowledge.to_bigstring ()

let knowledge_cache () =
  Data.Cache.Service.request
    knowledge_reader
    knowledge_writer

let project_state_cache () =
  let module State = struct
    type t = Project.state [@@deriving bin_io]
  end in
  let of_bigstring = Binable.of_bigstring (module State) in
  let to_bigstring = Binable.to_bigstring (module State) in
  let reader = Data.Read.create ~of_bigstring () in
  let writer = Data.Write.create ~to_bigstring () in
  Data.Cache.Service.request reader writer

let import_knowledge_from_cache digest =
  let digest = digest ~namespace:"knowledge" in
  info "looking for knowledge with digest %a"
    Data.Cache.Digest.pp digest;
  let cache = knowledge_cache () in
  match Data.Cache.load cache digest with
  | None -> false
  | Some state ->
    info "importing knowledge from cache";
    Toplevel.set state;
    true

let store_knowledge_in_cache digest =
  let digest = digest ~namespace:"knowledge" in
  info "caching knowledge with digest %a"
    Data.Cache.Digest.pp digest;
  let cache = knowledge_cache () in
  Toplevel.current () |>
  Data.Cache.save cache digest

let process passes outputs project =
  let autoruns = Project.passes () |>
                 List.filter ~f:Project.Pass.autorun in
  let autos = List.length autoruns in
  let total = List.length passes + autos in
  report_progress ~note:"analyzing" ~total ();
  run_passes 0 project autoruns >>= fun (step,proj) ->
  run_passes step proj passes >>| fun (_,proj) ->
  List.iter outputs ~f:(function
      | `file dst,fmt,ver ->
        Out_channel.with_file dst ~f:(fun ch ->
            Project.Io.save ~fmt ?ver ch proj)
      | `stdout,fmt,ver ->
        Project.Io.show ~fmt ?ver proj);
  proj

let old_style_passes =
  Extension.Command.switches
    ~doc:(sprintf "Enables the pass %s in the old style (DEPRECATED)")
    (Plugins.list () |> List.map ~f:Plugin.name)
    ident

let passes =
  Extension.Command.parameters
    ~doc:"Run the selected passes (in the specified order)"
    ~aliases:["p"]
    Extension.Type.("PASSES" %: list string) "passes"

let outputs =
  Extension.Command.parameters
    ~doc:"Dumps the program to <FILE> (defaults to stdout) \
          in the <FMT> format (defaults to bir)."
    ~as_flag:"bir"
    ~aliases:["d"]
    Extension.Type.("[<FMT>[:<FILE>]]" %: string)
    "dump"

let rw_file = Extension.Type.define
    ~name:"<FILE>" ~print:ident ~parse:ident
    ~digest:(fun path ->
        if Sys.file_exists path
        then Caml.Digest.file path
        else Caml.Digest.string "empty")
    ""

let update =
  Extension.Command.flag "update" ~aliases:["u"]
    ~doc: "Preserve the knowledge base, i.e., do not change it."

let knowledge =
  Extension.Command.parameter
    ~doc:"Import the knowledge to the provided knowledge base. \
          If the $(b,--update) flag is set the the knowledge base \
          will be also updated with the new information. If \
          $(b,--update) is set, the the knowledge base might not \
          exist and it will be created"
    ~aliases:["k"; "knowledge-base";]
    (Extension.Type.some rw_file) "project"

let input = Extension.Command.argument
    ~doc:"The input file" Extension.Type.("FILE" %: string =? "a.out" )

let loader =
  Extension.Command.parameter
    ~doc:"Use the specified loader.
          Use the loader `raw' to load unstructured files"
    Extension.Type.(string =? "llvm")
    "loader"

let target =
  let t = Extension.Type.define Theory.Target.unknown
      ~name:"NAME"
      ~digest:(fun t -> Caml.Digest.string@@Theory.Target.to_string t)
      ~parse:(fun s -> match Theory.Target.lookup ~package:"bap" s with
          | Some t -> t
          | None ->
            invalid_argf "unknown target %S, please see \
                          `bap list targets' for the full list \
                          of targets" s ())
      ~print:Theory.Target.to_string in
  Extension.Command.parameter t "target"
    ~doc:"Refines the target architecture of the binary. \
          See `bap list targets` for the full hierarchy of targets. \
          The specified target must be a refinement of the actual \
          target stored in the binary, otherwise an error is signaled."

let validate_input file =
  Result.ok_if_true (Sys.file_exists file)
    ~error:(Fail (Expects_a_regular_file file))

let validate_knowledge update kb = match kb with
  | None -> Result.ok_if_true (not update)
              ~error:(Fail No_knowledge)
  | Some path ->
    Result.ok_if_true (Sys.file_exists path || update)
      ~error:(Fail No_knowledge)

let validate_passes_style old_style_passes new_style_passes =
  match old_style_passes, new_style_passes with
  | xs,[] | [],xs -> Ok xs
  | _ -> Error (Fail Old_and_new_style_passes)

let validate_passes passes =
  let known = Project.passes () |>
              List.map ~f:(fun p -> Project.Pass.name p, p) |>
              Map.of_alist_exn (module String) in
  Result.all @@
  List.map passes ~f:(fun p -> match Map.find known p with
      | Some p -> Ok p
      | None -> Error (Fail (Unknown_pass p)))

let option_digest f = function
  | None -> "none"
  | Some s -> f s

let make_digest inputs =
  let inputs = String.concat inputs in
  fun ~namespace ->
    let d = Data.Cache.Digest.create ~namespace in
    Data.Cache.Digest.add d "%s" inputs

module Dump_formats = struct
  let parse_fmt fmt =
    match String.split ~on:'-' fmt with
    | [fmt;ver] -> fmt, Some ver
    | _ -> fmt,None

  let flatten (x,(y,z)) = x,y,z

  let split str = match String.split ~on:':' str with
    | [fmt;dst] -> flatten (`file dst,parse_fmt fmt)
    | _ -> flatten (`stdout,parse_fmt str)

  let parse_format str =
    let (_,fmt,ver) as r = split str in
    match Project.find_writer ?ver fmt with
    | Some _ -> Ok r
    | None -> match Project.find_writer fmt with
      | None -> Error (Fail (Unknown_format fmt))
      | Some _ -> Error (Fail (Unavailable_format_version fmt))

  let parse outputs =
    Result.all @@
    List.map outputs ~f:parse_format
end

let load_knowledge digest = function
  | None -> import_knowledge_from_cache digest
  | Some path when not (Sys.file_exists path) ->
    import_knowledge_from_cache digest
  | Some path ->
    info "importing knowledge from %S" path;
    Toplevel.set @@ Knowledge.load path;
    true

let save_knowledge ~had_knowledge ~update digest = function
  | None ->
    if not had_knowledge then store_knowledge_in_cache digest
  | Some path when update ->
    info "storing knowledge base to %S" path;
    Knowledge.save (Toplevel.current ()) path
  | Some _ -> ()


let create_and_process input outputs passes loader target update kb
      ctxt options =
  let digest = make_digest [
      Extension.Configuration.digest ctxt;
      Caml.Digest.file input;
      loader;
    ] in
  let had_knowledge = load_knowledge digest kb in
  let _ = Project.Input.load ~target ~loader input in
  superset_disasm options;
  save_knowledge ~had_knowledge ~update digest kb

let rounds =
  let doc = "Number of analysis cycles" in
  Extension.Command.parameter ~doc Extension.Type.(int =? 2) "rounds" 

let tp_threshold =
  let doc = "The threshold at which convergence occurs" in
  Extension.Command.parameter ~doc Extension.Type.float
    "threshold"

let featureset =
  let doc =
    "Specify the features used to converge upon the true positive set" in
  Extension.Command.parameter ~doc
    Extension.Type.(list string =? Features.default_features) "features"

let invariants =
  let doc = "Specify the desired invariants to apply to the superset" in
  let deflt = List.map Invariants.default_tags ~f:fst in
  Extension.Command.parameter ~doc
    Extension.Type.(list string =? deflt) "invariants"
  
let trim_method =
  let doc =
    "The false positives removal proceedure may be changed." in
  Extension.Command.parameter ~doc
    Extension.Type.(enum list_trimmers) "trimmer" 


(*let import_superset =
  Extension.Command.parameter
    ~doc:"Import the disassembly graph."
    Extension.Type.(some string =? None) "import"

let export_superset =
  Extension.Command.parameter Extension.Type.(some (string) =? None) 
    ~doc:"Export the disassembly graph." "export"*)

let ground_truth_bin =
  let doc = ("Compare results against a ground truth constructed" ^
               " from debug symbols of an unstripped binary") in
  Extension.Command.parameter
    ~doc Extension.Type.("<FILE>" %: (some string))
    "ground_truth_bin"

let ground_truth_file =
  let doc = ("Compare results against a file that contains the " ^
               "addresses of true positives") in
  Extension.Command.parameter ~doc
    Extension.Type.("<FILE>" %: some string) "ground_truth_file"

let metrics_format =
  let open Metrics in
  let list_formats_types = [
      "standard", Standard;
      "latex", Latex;
    ] in
  let list_formats_doc =
    sprintf "Available output metrics formats: %s" @@ 
      List.to_string ~f:ident (List.map ~f:fst list_formats_types) in
  Extension.Command.parameter ~doc:list_formats_doc
    Extension.Type.(enum list_formats_types =? Standard)  "metrics_format"

let analyses =
  let deflt = ["Strongly Connected Component Data"] in
  Extension.Command.parameter
    Extension.Type.(list string =? deflt) "analyses"

let save_addrs = Extension.Command.flag "save_addrs"
let save_gt = Extension.Command.flag "save_gt"
let save_dot = Extension.Command.flag "save_dot"
let collect_report =
  Extension.Command.flag "collect_reports"
  
let dforest =
  Extension.Command.parameter
    Extension.Type.(some (enum list_decision_trees) =? None)
    "decision_tree"
let cut =
  Extension.Command.parameter
    Extension.Type.(some (t3 (enum list_cuts) string int) =? None)
    "cut"

let _superset_disassemble_command : unit =
  let args =
    let open Extension.Command in
    args $input $outputs $old_style_passes $passes $loader $target
    $update $knowledge $ground_truth_bin $ground_truth_file
    $metrics_format $invariants $analyses $trim_method $tp_threshold
    $featureset $save_addrs $save_gt $save_dot $rounds $collect_report
    $dforest $cut
  in
  Extension.Command.declare ~doc:man "superset_disasm"
    ~requires:features_used args @@
    fun input outputs old_style_passes passes loader target update kb
        ground_truth_bin ground_truth_file metrics_format invariants
        analyses trim_method tp_threshold featureset save_addrs
        save_gt save_dot rounds collect_report dforest cut 
        ctxt  ->
    let options =
      Fields.create ~import:None ~export:None ~disassembler:loader
        ~ground_truth_bin ~ground_truth_file ~target:input
        ~metrics_format ~trim_method ~setops:[] ~cut ~save_dot
        ~save_gt ~save_addrs ~tp_threshold ~rounds ~featureset
        ~analyses ~collect_report ~phases:invariants ~dforest in
    validate_knowledge update kb >>= fun () ->
    validate_input input >>= fun () ->
    validate_passes_style old_style_passes (List.concat passes) >>=
      validate_passes >>= fun passes ->
    Dump_formats.parse outputs >>= fun outputs ->
    create_and_process input outputs passes loader target update kb
      ctxt options;
    Ok ()
  
(*let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P
        "Runs the superset disassembler along with any
                  optional trimming components to reduce false
                  positives to an ideal minimum. ";
    ] in
  let tp_threshold = Config.(param (float) ~default:0.99 "threshold" ~doc) in
  let doc = "The file that houses the points-to ground truth" in
  let featureset =
    Config.(param (list string)
              ~default:Features.default_features "features" ~doc) in
  (*let doc = "Save target, one of truth, or some superset" in
  let savetgt = Config.(param (list string) "savetgt" ~default:[] ~doc) in*)
  (*let doc = "Load from a previous disassembly" in
  let load = Config.(param (some string) "load" ~default:None ~doc) in*)

  let import_superset =
    Config.(param (some (string)) "import"
              ~default:None
              ~doc:"Import the disassembly graph.") in
  let export_superset =
    Config.(param (some (string)) "export"
              ~default:None
              ~doc:"Export the disassembly graph.") in
  let disassembler =
    Config.(param string "backend" ~default:"llvm"
              ~doc:"Backend disassembler") in
  let ground_truth_bin =
    Config.(param (some string) ~default:None "ground_truth_bin"
              ~doc:("Compare results against a ground truth if desired," ^
                      " of either debug symbols or an unstripped binary")) in
  let ground_truth_file =
    Config.(param (some string) ~default:None "ground_truth_file"
              ~doc:("Compare results against a file that contains the " ^
                      "addresses of true positives")) in
  let target =
    Config.(param string "target" ~doc:"Specify target binary") in
  let metrics_format =
    let open Metrics in
    let list_formats_types = [
        "standard", Standard;
        "latex", Latex;
      ] in
    let list_formats_doc = sprintf
                             "Available output metrics formats: %s" @@ 
                             Cmdliner.Arg.doc_alts_enum list_formats_types in
    Config.(param ((enum list_formats_types)) ~default:(Standard)
              "metrics_format" ~doc:list_formats_doc) in
  let analyses =
    let default = ["Strongly Connected Component Data"] in
    Config.(param (list string) ~default "analyses") in
  let save_addrs = Config.(flag "save_addrs") in
  let save_gt = Config.(flag "save_gt") in
  let save_dot = Config.(flag "save_dot") in
  let cut = Config.(param (some (t3 (enum list_cuts) string int))
                      ~default:None "cut") in
  let collect_report = Config.(flag "collect_reports") in
  let dforest = Config.(param (some (enum list_decision_trees))
                  ~default:None "decision_tree") in
  Config.when_ready (fun {Config.get=(!)} ->
      let import = !import_superset in
      let export = !export_superset in
      let disassembler = !disassembler in
      let ground_truth_bin = !ground_truth_bin in
      let ground_truth_file = !ground_truth_file in
      let target = !target in
      let metrics_format = !metrics_format in
      let phases = !invariants in
      let analyses = !analyses in
      let trim_method = !trim_method in
      let tp_threshold = !tp_threshold in
      let featureset = !featureset in
      let save_addrs = !save_addrs in
      let save_gt = !save_gt in
      let save_dot = !save_dot in
      let rounds = !rounds in
      let cut = !cut in
      let collect_report = !collect_report in
      let dforest = !dforest in
      let options =
        Fields.create ~import ~export ~disassembler ~ground_truth_bin
          ~ground_truth_file ~target ~metrics_format ~phases ~trim_method
          ~setops:[] ~cut ~save_dot ~save_gt ~save_addrs ~tp_threshold
          ~rounds ~featureset ~analyses ~collect_report ~dforest in
      superset_disasm options;
      print_endline "superset-disassembler plugin finished";
    )*)
