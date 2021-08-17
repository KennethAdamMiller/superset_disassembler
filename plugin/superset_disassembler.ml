open Core_kernel
open Bap.Std
open Regular.Std
open Bap_knowledge
open Bap_core_theory
open Monads.Std
open Cmdoptions
open Bap_main
open Bap_plugins.Std
open Bap_knowledge

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
   
  Passes are not run by the superset disassembler, but the output can
  be fed into the regular disassembly pipeline by making use of the
  cache. At that point, an analysis pass can be run, and it isn't
  meaningful to try to run an analysis pass on the raw superset alone
  because it does not reconstruct the full project type.

  # OUTPUT

  The resulting project data structure could be dumped using the
  $(b,--dump) (or $(b,-d) for short) option, which accepts the desired
  format and, optionally, the output file name.

  It is possible to specify the $(b,--dump) option multiple times, in
  which case the project will be dumped in several formats.

  ```
  bap superset_disasm /bin/echo -dasm:out.asm
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
  let invariants =
    With_options.args_to_funcs options.phases
      Invariants.default_tags in
  let invariants = Invariants.tag_success :: invariants in
  let backend = options.disassembler in
  let superset = Superset.superset_disasm_of_file ~backend options.target
                   ~f:(Invariants.tag ~invariants) in
  (* TODO use the knowledge base to import the superset *)
  let superset = With_options.with_options superset in
  (* TODO belongs in with_options *)
  (match options.ground_truth_bin with
   | Some bin ->
      Metrics.compute_metrics ~bin superset;
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
  | Incompatible_options of string * string
  | Project of Error.t
  | Unknown_format of string
  | Unavailable_format_version of string
  | Unknown_collator of string
  | Unknown_analysis of string
  | No_knowledge

type Extension.Error.t += Fail of failure

module Err = Monad.Result.Make(Extension.Error)(Monad.Ident)
open Err.Syntax

let proj_error = Result.map_error ~f:(fun err -> Fail (Project err))

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
          If the $(b,--update) flag is set the knowledge base \
          will be also updated with the new information. If \
          $(b,--update) is set, the knowledge base might not \
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


let create_and_process input outputs loader target update kb
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
  Extension.Command.parameter ~doc Extension.Type.(float =? 0.99)
    "threshold"

(* TODO rename featureset to heuristics *)
let featureset =
  let doc =
    "Specify the features used to converge upon the true positive set" in
  Extension.Command.parameter ~doc
    ~aliases:["enable_feature"] (* TODO remove alias enable_feature *)
    Extension.Type.(list string =? Features.default_features)
    "features"

let invariants =
  let doc = "Specify the desired invariants to apply to the superset" in
  let deflt = List.map Invariants.default_tags ~f:fst in
  Extension.Command.parameter ~doc
    ~aliases:["phases"] (* TODO remove phases alians *)
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
    args $input $outputs $loader $target
    $update $knowledge $ground_truth_bin $ground_truth_file
    $invariants $analyses $trim_method $tp_threshold
    $featureset $save_addrs $save_gt $save_dot $rounds $collect_report
    $dforest $cut
  in
  Extension.Command.declare ~doc:man "superset_disasm"
    ~requires:features_used args @@
    fun input outputs loader target update kb
        ground_truth_bin ground_truth_file invariants
        analyses trim_method tp_threshold featureset save_addrs
        save_gt save_dot rounds collect_report dforest cut 
        ctxt  ->
    let options =
      Fields.create ~import:None ~export:None ~disassembler:loader
        ~ground_truth_bin ~ground_truth_file ~target:input
        ~trim_method ~setops:[] ~cut ~save_dot
        ~save_gt ~save_addrs ~tp_threshold ~rounds ~featureset
        ~analyses ~collect_report ~phases:invariants ~dforest in
    validate_knowledge update kb >>= fun () ->
    validate_input input >>= fun () ->
    Dump_formats.parse outputs >>= fun outputs ->
    create_and_process input outputs loader target update kb
      ctxt options;
    Ok ()


let heuristics =
  Extension.Command.parameter Extension.Type.(list string) "heuristics"

let converge =
  Extension.Command.flag "converge"

let metrics =
  Extension.Command.flag "metrics"

(* TODO review features_used *)
let _distribution_command : unit =
  let args =
    let open Extension.Command in
    args $input $outputs $loader $target $update $knowledge
    $heuristics $converge $collect_report $metrics
  in
  Extension.Command.declare ~doc:man "superset_distribution"
    ~requires:features_used args @@
    fun input outputs loader target update kb
        feature converge collect_report metrics ctxt ->
    validate_knowledge update kb >>= fun () ->
    validate_input input >>= fun () ->
    Dump_formats.parse outputs >>= fun outputs ->
    let digest = make_digest [
                     Extension.Configuration.digest ctxt;
                     Caml.Digest.file input;
                     loader;
                   ] in
    let had_knowledge = load_knowledge digest kb in
    let _ = Project.Input.load ~target ~loader input in
    (*if metrics then (
      let bin = input in 
      Metrics.compute_metrics ~bin superset
    );*)
    save_knowledge ~had_knowledge ~update digest kb;
    Ok ()

let _cache_command : unit =
  let args =
    let open Extension.Command in
    args $input $outputs $loader $target $update $knowledge
  in
  Extension.Command.declare ~doc:man "superset_cache"
    ~requires:features_used args @@
    fun input outputs loader target update kb
        ctxt ->
    Ok ()
