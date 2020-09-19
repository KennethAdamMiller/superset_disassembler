open Cmdliner
open Core_kernel
open Metrics
open Core_kernel
open Bap.Std

module Dis = Disasm_expert.Basic

exception Bad_user_input
exception Unknown_arch
exception No_input

type invariant =
  Superset.t -> mem -> Dis.full_insn option -> Brancher.dests
  -> Superset.t [@@deriving sexp]
let invariants_doc = List.(to_string ~f:fst Invariants.default_tags)
let invariants =
  let doc = "Specify the desired trim phases to run." in
  Cmdliner.Arg.(
    value & opt ((list (enum Invariants.default_tags)))
              ((Invariants.default_funcs))
    & info ["phases"] ~docv:invariants_doc ~doc
  )

type analysis =
  Superset.t -> Superset.t [@@deriving sexp]
let tag_loop_contradictions =
  Sheathed.tag_loop_contradictions ?min_size:None
let tag_grammar  = 
  Grammar.tag_by_traversal ?threshold:None
let cross_layer_invalidation = 
  Invariants.tag_branch_violations
let list_analyses = [
    "Strongly Connected Component Data", tag_loop_contradictions;
    "Cross Layer Invalidation", cross_layer_invalidation;
    "Grammar convergent", tag_grammar;
  ]
let analyses_doc = List.(to_string ~f:fst (list_analyses))
let analyses_opt =
  let doc = "Select macro level analyses to run" in
  Cmdliner.Arg.(
    value & opt ((list (enum list_analyses)))
              (([Sheathed.tag_loop_contradictions]))
    & info ["analyses"] ~docv:analyses_doc ~doc
  )

type forester = | Tails [@@deriving sexp]
let list_decision_trees = [
    "decision_tree:tails",
    Tails;
  ]
let dtrees_doc = List.(to_string ~f:fst (list_decision_trees))                        
let decision_trees_opt =
  let doc = "Select macro level analyses to run" in
  Cmdliner.Arg.(
    value & opt ((some (enum list_decision_trees)))
              (None)
    & info ["decision_tree"] ~docv:dtrees_doc ~doc
  )

type setop =
  | Intersection
  | Difference
  | Union
[@@deriving sexp]
let list_setops = [
  "Intersection", Intersection;
  "Difference", Difference;
  "Union", Union;
]
let setops_doc = List.(to_string ~f:fst (list_setops))
let setops_opt =
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
  
type checkpoint = 
  | Import
  | Export
  | Update
[@@deriving sexp]
let list_checkpoints = [
  "Import", Import;
  "Export", Export;
  "Update", Update;
]
let ckpt = 
  let doc = "Import or Export the disassembly graph and map." in
  Cmdliner.Arg.(
    value & opt (some (enum list_checkpoints)) None & info ["checkpoint"] ~doc
  )

type trimmer = Superset.t -> Superset.t [@@deriving sexp]
let list_trimmers = [
  "Simple", Trim.Default.trim;
  (*"Memoried", Memoried;*)
  "DeadBlockResistant", Trim.DeadblockTolerant.trim;
  "Disabled", Trim.Disabled.trim;
]
let trimmer_doc =
  sprintf
    "Select from the following trim reduction implementation(s): %s"
    List.(to_string ~f:fst list_trimmers)
let trimmer =
  let doc = "Specify the desired trim reduction implementation to run." in
  Cmdliner.Arg.(
    value & opt (enum list_trimmers) (Trim.Disabled.trim)
    & info ["trimmer"] ~docv:trimmer_doc ~doc
  )
  
type cut =
  | DFS
  | Interval
[@@deriving sexp]
let list_cuts = [
  "DFS", DFS;
  "Interval", Interval; 
]
let cut_opt =
  let doc = "Specify a sub-graph, among feature based shallow BFS and address intervals " in
  Cmdliner.Arg.(value & opt (some (t3 (enum list_cuts) string int)) None
                & info ["cut"] ~doc)

  
type t = {
  checkpoint         : checkpoint option;
  disassembler       : string;
  ground_truth_bin   : string option;
  ground_truth_file  : string option;
  target             : string;
  metrics_format     : format_as;
  phases             : invariant list;
  analyses           : analysis list;
  trim_method        : trimmer;
  cut                : (cut * string * int) option;
  setops             : (string * (setop * (string * string))) list;
  save_dot           : bool;
  save_gt            : bool;
  save_addrs         : bool;
  collect_report     : bool;
  dforest            : forester option;
  tp_threshold       : float  option;
  rounds             : int;
  featureset         : string list;
} [@@deriving sexp, fields]


let save_gt =
  let doc =
    "Save nothing but the symbols reported by " in
  Cmdliner.Arg.(value & flag & info ["save_gt"] ~doc)

let save_addrs =
  let doc = "Save the addrs recovered" in
  Cmdliner.Arg.(value & flag & info ["save_addrs"] ~doc)

let collect_reports =
  let doc = "Collect reports between each feature and round" in
  Cmdliner.Arg.(value & flag & info ["collect_reports"] ~doc)

let rounds = 
  let doc = "Number of analysis cycles" in
  Cmdliner.Arg.(value & opt int 1 & info ["rounds"] ~doc)

let featureset_opt = 
  let doc =
    "Specify the features to extract for leverage in the probabilistic
  analysis, used to converge upon the true positive set" in
  Cmdliner.Arg.(value & opt ((list string)) (Features.default_features)
                & info ["enable_feature"] ~doc)

let tp_threshold =
  let doc = 
    "Used for specifying a tp trimming threshold" in
  Cmdliner.Arg.(value & opt (some float) (Some 0.99)
                & info ["tp_threshold"] ~doc)

let read_addrs width ic : addr list = 
  List.filter_map In_channel.(read_lines ic) ~f:(fun x ->
      try 
        Some(Addr.of_string x)
      with _ ->
      try
        let addr = sprintf "0x%s:%d" String.(strip x) width in
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

module type Provider = sig
  val options : t
end

let backend = 
  let doc = "The particular backend disassembler to use; llvm/IDA/ect" in
  Cmdliner.Arg.(value & opt string "llvm"
                & info ["backend"] ~docv:"Disassembler" ~doc)

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

let time ?(name="") f x =
  let t = Sys.time() in
  let fx = f x in
  let s = sprintf "%s execution time: %fs\n" name (Sys.time() -. t) in
  print_endline s;
  fx

let add_tag_loop analyses = 
  Map.set analyses (Map.length analyses)
    (None, Some("Tag loop contradictions",
                Sheathed.tag_loop_contradictions), None)

let build_tracker trim phases = 
  List.fold phases ~init:(trim,String.Map.empty)
    ~f:(fun (trim,trackers) phase ->
      match List.find Invariants.default_tags ~f:(fun (name,p) ->
                phys_equal phase p
              ) with
      | Some (name,p) ->
         let accu = ref Addr.Map.empty in
         let instance_trim = Metrics.make_mark_tracker accu in
         (fun x -> instance_trim @@ trim x), Map.set trackers name accu
      | None -> trim,trackers
    )

let build_metrics trim phases =
  List.fold phases ~init:(trim,String.Map.empty)
    ~f:(fun (trim,metrics) phase ->
      match List.find Invariants.default_tags ~f:(fun (name,p) ->
                phys_equal phase p
              ) with
      | Some (name,p) ->
         let accu = (Addr.Hash_set.create ()) in
         let instance_trim = Metrics.make_gatherer accu in
         (fun x -> instance_trim @@ trim x), Map.set metrics name accu
      | None -> trim,metrics
    )

let build_setops trim setops =
  List.fold setops ~init:(trim,String.Map.empty)
    ~f:(fun (trim,metrics) (colr, (sop, (f1, f2))) ->
      let add (trim,metrics) f =
        if Map.mem metrics f then
          trim, metrics
        else
          let accu = (Addr.Hash_set.create ()) in
          let instance_trim = Metrics.make_gatherer accu in
          let metrics = Map.set metrics f accu in
          ((fun x -> instance_trim @@ trim x), metrics) in
      add (add (trim,metrics) f1) f2
    )

let collect_results superset ground_truth_file results metrics setops tracker =
  match ground_truth_file with
  | Some (gt) ->
     let remaining = Addr.Hash_set.create () in
     let remaining = 
       Superset.Core.fold superset ~init:remaining
         ~f:(fun ~key ~data remaining ->
           Hash_set.add remaining key; remaining
         ) in
     (* apply setops within ground truth? *)
     Map.iteri setops ~f:(fun ~key ~data ->
         let s = sprintf "%s - %d" key Hash_set.(length data) in
         print_endline s;
         Hash_set.iter data ~f:(Hash_set.remove remaining);
       );
     let min_addr = Superset.Inspection.get_base superset in
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
     let s = sprintf "tracker size: %d" Map.(length tracker) in
     print_endline s;
     Map.iteri tracker ~f:(fun ~key ~data ->
         let d = !data in
         let s = sprintf "tracker %s size: %d"
                   key Map.(length d) in
         print_endline s;
         Map.iteri !data ~f:(fun ~key ~data ->
             if (not Superset.Core.(mem superset key))
                && Hash_set.(mem tps key) then (
               let memstr =
                 match Superset.Core.lookup superset key with
                 | Some (mem, _ ) -> Memory.str () mem
                 | None -> "" in
               let s =
                 sprintf
                   "marked fn bad with mem %s of removal size %d"
                   memstr Set.(length data) in
               print_endline s;
             );
             Set.iter data ~f:(fun x ->
                 let s = sprintf "marked fn bad at %s"
                           Addr.(to_string key) in
                 let s = sprintf "%s, ancestor fn at %s"
                           s Addr.(to_string x) in
                 print_endline s;
               ) ; 
           )
       );
     let results = String.Map.set results "True Positives" tps in
     let results = String.Map.set results "False Positives" fps in
     let results = String.Map.set results "False Negatives" fns in
     results
  | None -> results 

(* might be able to adjust this with new cmd features. *)
let apply_setops metrics setops =
  let report_err colr f1 =
    Format.eprintf
      "Feature %s not found, error for color %s!" f1 colr in
  List.fold setops ~init:String.Map.empty
    ~f:(fun results (colr, (sop, (f1, f2))) -> 
      match Map.find metrics f1, Map.find metrics f2 with
      | Some (fmetric1), Some (fmetric2) -> (
        match sop with
        | Difference ->
           let s = Addr.Hash_set.create () in
           Hash_set.iter fmetric1 ~f:(fun fv ->
               if not (Hash_set.mem fmetric2 fv) then
                 Hash_set.add s fv
             );
           Map.set results colr s
        | Union ->
           let s = Addr.Hash_set.create () in
           Hash_set.iter fmetric1 ~f:(fun fv ->
               Hash_set.add s fv
             );
           Hash_set.iter fmetric2 ~f:(fun fv ->
               Hash_set.add s fv
             );
           Map.set results colr s
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
           Map.set results colr s
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

  
module With_options(Conf : Provider)  = struct
  open Conf
  open Metrics.Opts
  open Or_error
  open Format

  let with_invariants superset invariants =
    let invariants = Invariants.tag_success :: invariants in 
    time ~name:"tagging"
      (Invariants.tag_superset ~invariants) superset

  let with_analyses superset analyses =
    let tps = Addr.Hash_set.create () in
    let fps = Addr.Hash_set.create () in
    let ro = Addr.Hash_set.create () in
    List.fold analyses ~init:superset ~f:(fun superset analyze ->
        if options.collect_report then (
          let analysis = "" in
          match Map.find Features.exfiltmap analysis with
          | Some (extractf, filterf) ->
             let pmap = Features.make_featurepmap options.featureset superset in
             let report = 
               Report.collect_map_report superset extractf filterf tps
                 ro fps  in ()
          | None -> ()
        );
        analyze superset
      )
 
  let process_cut superset options results =
    match options.cut with 
    | None ->
       if options.save_dot then
         Superset.ISG.print_dot ~colorings:results superset;
    | Some cut -> (
      let cut =
        let c, addr, len = cut in
        let addr =
          match addr with
          | "lowest" ->
             Superset.Inspection.get_base superset
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
         Traverse.iter_component ~terminator ~pre ~post superset addr;
         let superset = Superset.ISG.filter superset subgraph in 
         Superset.ISG.print_dot ~colorings:results superset
      | Interval, addr, len ->
         let subgraph = Addr.Hash_set.create () in
         let add x =
           if Addr.(addr <= x) && Addr.(x <= (addr ++ len)) then
             Hash_set.add subgraph x in
         Superset.ISG.iter_vertex superset add;
         let superset = Superset.ISG.filter superset subgraph in 
         Superset.ISG.print_dot ~colorings:results superset
    )

  let converge featureset superset =
    let superset = Features.apply_featureset featureset superset in
    Features.apply_featurepmap featureset superset

  let trim_with f superset =
    let rec do_analysis round superset = 
      if round = options.rounds then superset else
        let superset = Trim.Default.trim @@ f superset in
        do_analysis (round+1) superset in
    do_analysis 0 superset

  let checkpoint bin invariants =
    let backend = options.disassembler in
    let dis_method x =
      let f x =
        Superset.superset_disasm_of_file ~backend x
          ~f:(Invariants.tag ~invariants:[Invariants.tag_success])
      in time ~name:"disasm binary" f x in
    match options.checkpoint with
    | Some Import -> 
       let superset = time ~name:"import" Superset.import bin in
       with_invariants superset invariants
    | Some Export ->
       let superset = dis_method bin in
       let superset = with_invariants superset invariants in
       Superset.export bin superset;
       superset
    | Some Update ->
       let superset = Superset.import bin in
       let superset = with_invariants superset invariants in
       Superset.export bin superset;
       superset
    | None ->
       let superset = dis_method bin in
       with_invariants superset invariants

  let with_options superset =
    let phases = options.phases in
    let analyses = options.analyses in
    let trim = options.trim_method in
    let superset = with_analyses superset analyses in
    let (trim,metrics) = build_metrics trim phases in
    let (trim,tracker) = build_tracker trim phases in
    let (trim,setops) = build_setops trim options.setops in
    let superset = trim superset in
    let superset = trim_with (converge options.featureset) superset in
    let _ =
      if options.save_addrs then
        Superset.export_addrs options.target superset
      else () in
    let results = apply_setops setops options.setops in
    let results =
      collect_results superset
        options.ground_truth_file results metrics setops tracker in
    process_cut superset options results;
    superset
       
  let main () =
    let format = match options.metrics_format with
      | Latex -> Metrics.format_latex
      | Standard -> Metrics.format_standard in
    let _ = 
      if options.save_gt then
        let gt = Metrics.ground_truth_of_unstripped_bin
            options.target |> ok_exn in
        let gt = Seq.map gt ~f:Addr.to_string in
        Seq.iter gt ~f:print_endline;
        exit 0
      else () in
    let superset = checkpoint options.target options.phases in
    let superset = with_options superset in
    match options.ground_truth_bin with
    | Some bin -> 
      Metrics.gather_metrics ~bin superset |> format |> print_endline
    | None -> ()

end
