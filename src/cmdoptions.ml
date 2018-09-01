open Cmdliner
open Core_kernel.Std
open Metrics
open Bap.Std

exception Bad_user_input
exception Unknown_arch
exception No_input

type phase = 
  | Default
  | Target_not_in_memory
  | Target_is_bad
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

type setop =
  | Intersection
  | Difference
  | Union
[@@deriving sexp]
           
type checkpoint = 
  | Import
  | Export
  | Update
[@@deriving sexp]

type trimmer =
  | Simple
  (*| Memoried*)
  | DeadBlockResistant
  | Disabled
[@@deriving sexp]

type cut =
  | DFS
  | Interval
[@@deriving sexp]

type t = {
  checkpoint         : checkpoint option;
  disassembler       : string;
  ground_truth_bin   : string option;
  ground_truth_file  : string option;
  target             : string;
  metrics_format     : format_as;
  phases             : phase list option;
  trim_method        : trimmer option;
  cut                : (cut * string * int) option;
  setops             : (string * (setop * (string * string))) list;
  save_dot           : bool;
} [@@deriving sexp, fields]

let read_addrs width ic : addr list = 
  List.filter_map In_channel.(read_lines ic) ~f:(fun x -> 
      let addr = sprintf "0x%s:%d" String.(strip x) width in
      try 
        Some(Addr.of_string (addr))
      with _ -> None
    )  

let fn_of_tp tp od = 
  let tp = Addr.Set.of_list tp in
  let od = Addr.Set.of_list od in
  Set.(diff od tp)

let fp_of_tp tp od = 
  let tp = Addr.Set.of_list tp in
  let od = Addr.Set.of_list od in
  Set.(diff tp od)
       
let save_dot =
  let doc =
    "Without any cut operations, you can specify to still save the
     entire graph. Not recommended - disassembly tends to be large" in
  Cmdliner.Arg.(value & flag & info ["save_dot"] ~doc)
       
let list_cuts = [
    "DFS", DFS;
    "Interval", Interval; 
  ]
let cut =
  let doc = "Specify a sub-graph, among feature based shallow BFS and address intervals " in
  Cmdliner.Arg.(value & opt (some (t3 (enum list_cuts) string int)) None
                             & info ["cut"] ~doc)


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
  "Target_is_bad", Target_is_bad;
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
  "Disabled", Disabled;
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

let ground_truth_bin = 
  Cmdliner.Arg.(
    value & opt (some string) (None) 
    & info ["ground_truth_bin"] ~doc:
        ("Compare results against a ground truth if desired," ^
           " of either debug symbols or an unstripped binary"))

let ground_truth_file = 
  Cmdliner.Arg.(
    value & opt (some string) (None) 
    & info ["ground_truth_file"] ~doc:
        ("Compare results against a file that contains the addresses
          of true positives"))
  
let list_setops = [
    "Intersection", Intersection;
    "Difference", Difference;
    "Union", Union;
]
let setops_doc = List.(to_string ~f:fst (list_setops))
let setops =
  let doc =
    "Specify set operations to perform for coloring of features. Once
    specified, a color may subsequently be the target of another set
    operation. In addition, some non color tag names can be used
    provided they start with ':', and will not appear colored in the
    dot output." in
  Cmdliner.Arg.(
    value & opt_all ((pair ~sep:'=' string & pair (enum (list_setops)) & pair string string)) []
    & info ["setop"] ~docv:setops_doc ~doc
  ) 

let time ?(name="") f x =
  let t = Sys.time() in
  let fx = f x in
  let s = sprintf "%s execution time: %fs\n" name (Sys.time() -. t) in
  print_endline s;
  fx

let select_trimmer trim_method =
  match trim_method with
  | Some Simple | None -> Trim.Default.trim
  (*| Some Memoried -> Trim.*)
  | Some DeadBlockResistant ->
     Trim.DeadblockTolerant.trim
  | Some Disabled ->
     Trim.Disabled.trim
    
let with_phases superset phases =
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
           (*Map.add analyses (Map.length analyses)
             (None, Some(tag_grammar), None)*)
           analyses
        | Target_not_in_memory -> 
           Map.add analyses Map.(length analyses)
             (Some(Trim.tag_target_not_in_mem), None, None)
        | Target_is_bad ->
           Map.add analyses Map.(length analyses)
             (Some(Trim.tag_target_is_bad), None, None)
        | Target_within_body -> 
           Map.add analyses Map.(length analyses) (Some(Trim.tag_target_in_body), None, None)
        | Invalid_memory_access -> 
           Map.add analyses Map.(length analyses) (Some(Trim.tag_non_mem_access), None, None)
        | Non_instruction ->
           Map.add analyses Map.(length analyses) (Some(Trim.tag_non_insn), None, None)
        | Component_body -> 
           Map.add analyses Map.(length analyses) 
             (None, Some(Sheathed.tag_loop_contradictions), None)
        | Cross_layer_invalidation ->
           let discard_arg ?min_size = 
             Invariants.tag_branch_violations in
           Map.add analyses Map.(length analyses)
             (None, Some(discard_arg), None)
        | Grammar_convergent -> 
           Map.add analyses Map.(length analyses)
             (None, Some(tag_grammar), None)
        | Tree_set -> 
           Map.add analyses Map.(length analyses)
             (None, None, Some(Decision_tree_set.decision_trees_of_superset))
      ) in
  let analyses = Map.add analyses (1+Map.(length analyses))
                   (Some(Trim.tag_success),None,None) in
  (*let analyses = Map.add analyses (1+Map.(length analyses))
                       (Some(Trim.tag_target_is_bad),None,None) in*)
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
