open Cmdliner
open Core_kernel.Std
open Metrics

exception Bad_user_input
exception Unknown_arch
exception No_input

type phase = 
  | Default
  | Target_not_in_memory
  | Target_within_body
  | Invalid_memory_access
  | Non_instruction
  | Component_body
  | Cross_layer_invalidation
  | Grammar_convergent
  | Tree_set
[@@deriving sexp]

(* TODO tree set should be a separate option *)

(* TODO execute in fixpoint or by number of rounds? *)

type checkpoint = 
  | Import
  | Export
  | Update
[@@deriving sexp]

type trimmer =
  | Simple
  (*| Memoried*)
  | DeadBlockResistant
[@@deriving sexp]

type t = {
  checkpoint     : checkpoint option;
  disassembler   : string;
  ground_truth   : string option;
  target         : string;
  metrics_format : format_as;
  phases         : phase list option;
  trim_method    : trimmer option;
} [@@deriving sexp, fields]

module type Provider = sig
  val options : t
end


let backend = 
  let doc = "The particular backend disassembler to use; llvm/IDA/ect" in
  Cmdliner.Arg.(value & opt string "llvm"
                & info ["backend"] ~docv:"Disassembler" ~doc)

let list_checkpoints = [
  "Import", Import;
  "Export", Export;
  "Update", Update;
]
let checkpoint = 
  let doc = "Import or Export the disassembly graph and map." in
  Cmdliner.Arg.(
    value & opt (some (enum list_checkpoints)) None & info ["checkpoint"] ~doc
  )

let parse_input f = 
  if Sys.file_exists f then (
    `Ok (f)
  ) else `Error "does not exist"
let input_type_printer p
  = Format.fprintf p "%s"
let target = 
  let doc = "Specify target binary or corpora folder." in
  Cmdliner.Arg.(
    required & opt (some (parse_input, input_type_printer)) None
    & info ["target"] ~docv:"Target" ~doc
  )


let list_phases = [
  "All Instruction invariants", Default;
  "Target_out_of_bounds", Target_not_in_memory;
  "Invalid memory accesses", Invalid_memory_access;
  "Target_within_body", Target_within_body;
  "Non instruction opcode", Non_instruction;
  "Strongly Connected Component Data", Component_body;
  "Cross Layer Invalidation", Cross_layer_invalidation;
  "Grammar convergent", Grammar_convergent;
]
let phases_doc = List.(to_string ~f:fst list_phases)
let phases =
  let doc = "Specify the desired trim phases to run." in
  Cmdliner.Arg.(
    value & opt (some (list (enum list_phases))) (Some([Default]))
    & info ["phases"] ~docv:phases_doc ~doc
  )

let list_trimmers = [
  "Simple", Simple;
  (*"Memoried", Memoried;*)
  "DeadBlockResistant", DeadBlockResistant;
]
let trimmer_doc =
  sprintf
    "Select from the following trim reduction implementation(s): %s"
    List.(to_string ~f:fst list_trimmers)
let trim_method =
  let doc = "Specify the desired trim reduction implementation to run." in
  Cmdliner.Arg.(
    value & opt (some ((enum list_trimmers))) (Some(Simple))
    & info ["trimmer"] ~docv:trimmer_doc ~doc
  )


let time ?(name="") f x =
  let t = Sys.time() in
  let fx = f x in
  let s = sprintf "%s execution time: %fs\n" name (Sys.time() -. t) in
  print_endline s;
  fx


let with_phases superset phases =
  let non_insn_idx = 4 in
  let tag_grammar ?min_size = 
    Grammar.tag_by_traversal ?threshold:None in
  let analyses = 
    List.fold ~init:Int.Map.empty phases ~f:(fun analyses phase -> 
        match phase with
        | Default -> 
          let analyses = 
            List.foldi ~init:analyses Trim.default_tags
              ~f:(fun idx analyses tag_func ->
                  Map.add analyses idx (Some(tag_func), None, None))
          in
          (*let discard_arg ?min_size = 
            Invariants.tag_branch_violations in
            let analyses = Map.add analyses (Map.length analyses)
              (None, Some(discard_arg), None) in*)
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
            Invariants.tag_branch_violations in
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
  let (tag_funcs, analysis_funcs, make_tree) =
    collect_analyses analyses in
  let superset = 
    time ~name:"tagging"
      (Trim.tag_superset  ~invariants:tag_funcs) superset in
  List.foldi ~init:superset analysis_funcs 
    ~f:(fun idx superset analyze ->
        let name = sprintf "analysis %d" idx in
        time ~name analyze superset)
