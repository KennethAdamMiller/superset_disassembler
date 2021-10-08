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
  let module With_options =
    With_options(struct
        let options = options
      end) in
  let t = Sys.time() in
  let superset = With_options.with_options () in
  KB.promise Metrics.Cache.time (fun o ->
      KB.return (Some (int_of_float (Sys.time() -. t))));
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
  superset
      
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
  | No_knowledge of string

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

let load_cache_with_digest cache digest =
  match Data.Cache.load cache digest with
  | None -> false
  | Some state ->
     info "importing knowledge from cache";
     Toplevel.set state;
     true
  
let import_knowledge_from_cache digest =
  let digest = digest ~namespace:"knowledge" in
  info "looking for knowledge with digest %a"
    Data.Cache.Digest.pp digest;
  let cache = knowledge_cache () in
  load_cache_with_digest cache digest
    
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
  | None ->
     Ok ()
  | Some path ->
     let error =
       Fail (No_knowledge "No initial knowledge to update") in
    Result.ok_if_true (Sys.file_exists path || update) ~error

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
    store_knowledge_in_cache digest
  | Some path when update ->
    info "storing knowledge base to %S" path;
    Knowledge.save (Toplevel.current ()) path
  | Some _ -> ()


let compute_digest target disasm =
  make_digest [
      Caml.Digest.file target;
      disasm;
    ]

let superset_digest options =
  let open Cmdoptions in
  compute_digest options.target options.disassembler

let save_metadata  options =
  let digest = superset_digest options ~namespace:"knowledge"  in
  Metadata.with_digests (fun metadata ->
      let c = Option.value metadata
                ~default:Metadata.Cache_metadata.empty in
      KB.promise Metadata.digests (fun o ->
          KB.return @@ (Some
                          (Metadata.Cache_metadata.set c
                             options.target Data.Cache.Digest.(to_string digest)))
        );
    );
  Metadata.save ()
  
let create_and_process
      input outputs loader target update kb options =
  let () = save_metadata options in
  let digest = superset_digest options in
  let had_knowledge = load_knowledge digest kb in
  let () = if not had_knowledge then
             let _ = superset_disasm options in () else () in
  (match options.ground_truth_bin with
   | Some bin ->
      KB.promise Metrics.Cache.ground_truth_source
        (fun _ -> KB.return bin);
   | None -> ());
  let ro = Metrics.Cache.reduced_occlusion in
  let _ = Toplevel.eval ro Metrics.Cache.sym_label in
  let _ = Toplevel.eval Metrics.Cache.size Metrics.Cache.sym_label in
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
    args $input $outputs $loader $target $update $knowledge
    $ground_truth_bin $ground_truth_file $invariants $analyses
    $trim_method $tp_threshold $featureset $save_addrs
    $save_gt $save_dot $rounds $collect_report
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
    Ok (create_and_process input outputs loader
          target update kb options)
    
let converge =
  Extension.Command.flag "converge"

let metrics =
  let doc =
    sprintf "%s%s%s%s"
    "The format string specifying how to print the metrics"
    ", which include: clean_functions, true_positives, "
    "false_positives, false_negatives, reduced_occlusion, "
    " occlusive_space, and function_entrances " in
  Extension.Command.parameter ~doc
    Extension.Type.(some (list string)) "metrics"


(* TODO review features_used to possibly remove some *)
let _distribution_command : unit =
  let args =
    (* TODO can remove unnecessary arguments now that digest is by file *)
    let open Extension.Command in
    args $input $outputs $loader $target $update $knowledge
    $ground_truth_bin $invariants $analyses
    (* TODO use collect_report and featureset to evaluate them *)
    $tp_threshold $featureset $rounds $collect_report
    $converge $metrics in
  Extension.Command.declare ~doc:man "superset_distribution"
    ~requires:features_used args @@
    fun input outputs loader target update kb
        ground_truth_bin invariants
        analyses tp_threshold featureset rounds collect_report
        converge metrics
        ctxt ->
    validate_knowledge update kb >>= fun () ->
    validate_input input >>= fun () ->
    Dump_formats.parse outputs >>= fun outputs ->
    let options =
      Fields.create ~import:None ~export:None ~disassembler:loader
        ~ground_truth_bin ~ground_truth_file:None ~target:input
        ~trim_method:Cmdoptions.Simple ~setops:[] ~cut:None
        ~save_dot:false
        ~save_gt:false ~save_addrs:false ~tp_threshold ~rounds ~featureset
        ~analyses ~collect_report ~phases:invariants ~dforest:None in
    let digest = superset_digest options in
    let _ = load_knowledge digest kb in
    (*let _ = Project.Input.load ~target ~loader input in*)
    let map_opt =
      function | None -> "unknown" | Some v -> sprintf "%d" v in
    let open KB.Syntax in
    Toplevel.exec @@
      (match metrics with
       | Some metrics ->
          (* TODO This symbol belongs in Metrics *)
          KB.Symbol.intern "superset_analysis"
            Theory.Program.cls >>= (fun label ->
          let oc_space = Metrics.Cache.occlusive_space in
          let ro = Metrics.Cache.reduced_occlusion in
          let fns = Metrics.Cache.false_negatives in
          let fps = Metrics.Cache.false_positives in
          let tps = Metrics.Cache.true_positives in
          let slots = [ oc_space ; ro; fns; fps; tps ] in
          let slots =
            List.map slots ~f:(fun slt ->
                KB.collect slt label >>= fun d ->
                KB.return @@
                  ((KB.Name.show @@ KB.Slot.name slt),(map_opt d))
              ) in
          let fe = Metrics.Cache.function_entrances in
          let clean = Metrics.Cache.clean_functions in
          KB.all @@ List.append slots @@
            List.map [fe; clean] ~f:(fun slt ->
                KB.collect slt label >>= fun d ->
                let d = Option.map d ~f:Set.length in
                KB.return @@
                  ((KB.Name.show @@ KB.Slot.name slt),(map_opt d))
              ) >>= fun slots ->
          let metric_vals =
            List.fold ~init:String.Map.empty slots
              ~f:(fun m (name,v) -> String.Map.set m name v) in
          let fmt,rem = List.hd metrics, List.tl metrics in
          let s = 
            match fmt, rem with
            | Some fmt, Some rem ->
               let init = fmt,1 in
               let s,_=List.fold rem ~init ~f:(fun (fmt,v) s ->
                           let r = Str.regexp @@ sprintf "%%%d" v in
                           let opts =
                             List.to_string (Map.keys metric_vals)
                               ~f:ident in
                           let default =
                             sprintf
                               "\"%s\" is not a metric, opts: %s" s opts in
                           let s = Map.find metric_vals s in
                           let s = Option.value s ~default in
                           Str.global_replace r s fmt,v+1
                         ) in s
            | _ -> "inproper arguments given to metrics" in
          print_endline s;
          KB.return ()
         )
       | None -> KB.return ()
      );
    Ok ()

let show_cache_digest =
  Extension.Command.flag "show_cache_digest"

let reset_cache =
  Extension.Command.flag "reset_cache"
  
let _cache_command : unit =
  let args =
    let open Extension.Command in
    args $input $outputs $loader $target $update $knowledge
  in
  Extension.Command.declare ~doc:man "superset_cache"
    ~requires:features_used args @@
    fun input outputs loader target update kb
        ctxt ->
    let d = compute_digest input loader ~namespace:"knowledge" in
    info "%a" Data.Cache.Digest.pp d;
    Ok ()

