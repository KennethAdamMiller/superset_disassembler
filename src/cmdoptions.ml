open Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_core_theory

module Dis = Disasm_expert.Basic

let tag_loop_contradictions =
  Grammar.tag_loop_contradictions ?min_size:None
let tag_grammar  = 
  Grammar.tag_by_traversal ?threshold:None
let list_analyses = [
    "Strongly Connected Component Data", tag_loop_contradictions;
    "Grammar convergent", tag_grammar;
  ]
  
type opts = {
  disassembler       : string;
  ground_truth_bin   : string option;
  target             : string;
  invariants         : string list;
  analyses           : string list;
  converge           : bool;
  protect            : bool;
  save_dot           : bool;
  tp_threshold       : float;
  rounds             : int;
  heuristics         : string list;
} [@@deriving sexp, fields, bin_io]
type t = opts

module Cache = struct

  let disasm_opts =
    let package = "superset-disasm-cmdoptions" in
    let opts_persistent =
      Knowledge.Persistent.of_binable
        (module struct type t = opts option [@@deriving bin_io] end) in
    let attr ty persistent name desc =
      let open Theory.Program in
      Knowledge.Class.property ~package cls name ty
        ~persistent
        ~public:true
        ~desc in
    let open Knowledge.Domain in
    let disequal _ _ = false in
    let opts = optional ~inspect:sexp_of_opts ~equal:disequal "opts ty" in
    attr opts opts_persistent "disasm_opts"
      "All command options given"

end

module type Provider = sig
  val options : t
end

module With_options(Conf : Provider)  = struct
  open Conf
  open Or_error
  open Format

  let () = 
    let open KB.Syntax in
    KB.promise Cache.disasm_opts (fun o ->
        KB.return (Some options)
      )

  let with_analyses superset analyses =
    Trim.run @@
      List.fold analyses ~init:superset ~f:(fun superset analyze ->
          analyze superset
        )

  let checkpoint bin invariants =
    (* TODO use the knowledge base to import the superset *)
    let backend = options.disassembler in
    let invariants = Invariants.tag_success ::invariants in
    let f = Invariants.tag ~invariants in
    Superset.superset_disasm_of_file ~backend bin ~f

  let args_to_funcs args funcs =
    let l = List.filter_map args
      ~f:(fun arg ->
        List.find funcs ~f:(fun (name,f) ->
            String.equal arg name
          )
      ) in
    List.map l ~f:snd    
      
  let invariants =
    args_to_funcs options.invariants Invariants.default_tags

  let analyses = args_to_funcs options.analyses list_analyses

  let with_options () =
    let superset = checkpoint options.target invariants in
    let () = Metrics.set_ground_truth superset in
    let trim = Trim.run in
    let superset = trim superset in
    let superset = with_analyses superset analyses in
    let superset = trim superset in
    let pnts_of_percent prcnt =
      Int.of_float (1.0/.(1.0-.prcnt)) in
    let threshold = (pnts_of_percent options.tp_threshold) in
    Heuristics.with_featurepmap options.heuristics superset
      ~f:(fun pmap featureset superset ->
        let total_of_features l =
          List.fold ~init:0 ~f:(fun x (y,_,_) -> x + y) l in
        let feature_pmap = 
          Map.map pmap ~f:(total_of_features) in
        let feature_pmap = 
          Map.filter feature_pmap (fun total ->
              (total > threshold)) in
        Report.collect_distributions superset threshold pmap;
        if options.converge then
          let f superset =
            if options.protect then
              Fixpoint.protect superset (fun superset ->
                  Fixpoint.converge superset options.heuristics feature_pmap
                )
            else
              Fixpoint.converge superset options.heuristics feature_pmap in
          let superset = Fixpoint.iterate options.rounds f superset in
          Metrics.compute_metrics superset;
        else ()
      )
       
  let main = with_options 

end
