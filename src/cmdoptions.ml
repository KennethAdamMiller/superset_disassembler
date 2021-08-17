open Cmdliner
open Core_kernel
open Metrics
open Core_kernel
open Bap.Std

module Dis = Disasm_expert.Basic

exception Bad_user_input
exception Unknown_arch
exception No_input

let invariants_doc = List.(to_string ~f:fst Invariants.default_tags)
let invariants =
  let doc = "Specify the desired trim phases to run." in
  Cmdliner.Arg.(
    value & opt (list string)
              (List.map Invariants.default_tags ~f:fst)
    & info ["phases"] ~docv:invariants_doc ~doc
  )

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
    value & opt ((list string))
              (["Strongly Connected Component Data"])
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


let import_superset = 
  let doc = "Import the disassembly graph and map." in
  Cmdliner.Arg.(
    value & opt (some string) None & info ["import"] ~doc
  )
let export_superset = 
  let doc = "Export the disassembly graph and map." in
  Cmdliner.Arg.(
    value & opt (some string) None & info ["export"] ~doc
  )

type trimmer = | Simple
               | DeadBlockResistant
               | Disabled [@@deriving sexp]
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
let trimmer =
  let doc = "Specify the desired trim reduction implementation to run." in
  Cmdliner.Arg.(
    value & opt (enum list_trimmers) (Simple)
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
  import             : string option;
  export             : string option;
  disassembler       : string;
  ground_truth_bin   : string option;
  ground_truth_file  : string option;
  target             : string;
  phases             : string list;
  analyses           : string list;
  trim_method        : trimmer;
  cut                : (cut * string * int) option;
  setops             : (string * (setop * (string * string))) list;
  save_dot           : bool;
  save_gt            : bool;
  save_addrs         : bool;
  collect_report     : bool;
  dforest            : forester option;
  tp_threshold       : float;
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
  Cmdliner.Arg.(value & opt (float) (0.99)
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

let build_setops trim setops =
  List.fold setops ~init:(trim,String.Map.empty)
    ~f:(fun (trim,metrics) (colr, (sop, (f1, f2))) ->
      let add (trim,metrics) f =
        if Map.mem metrics f then
          trim, metrics
        else
          let accu = (Addr.Hash_set.create ()) in
          let metrics = Map.set metrics f accu in
          ((fun x -> trim x), metrics) in
      add (add (trim,metrics) f1) f2
    )

(* TODO belongs in fixpoint *)
let percent_of_pnts pnts =
  1.0 -. 1.0/.(Float.of_int pnts)
let pnts_of_percent prcnt =
  Int.of_float (1.0/.(1.0-.prcnt))

(* TODO move this to within the with_featuremap *)
let trim_with rounds f superset =
  let (superset,accu) = f superset in
  let rec do_analysis round superset = 
    if round = rounds then superset, accu else
      let (superset,_) = f superset in
      let before = Superset.Inspection.count superset in
      let superset = Trim.Default.trim superset in
      let after = Superset.Inspection.count superset in
      print_endline @@ sprintf "removed %d" (before - after);
      do_analysis (round+1) superset in
  do_analysis 1 superset

let converge options superset =
  let threshold = options.tp_threshold in
  let featureset = options.featureset in
  let visited = Addr.Hash_set.create () in
  (* TODO what callsite protection value would serve the best? *)
  let callsites = Features.get_callsites ~threshold:0 superset in
  let superset = Features.tag_callsites visited ~callsites superset in
  Features.with_featurepmap featureset superset
    ~f:(fun pmap featureset superset ->
      print_endline @@ sprintf "raw pmap size: %d, threshold: %d"
                         (Map.length pmap) (pnts_of_percent threshold);
      let total_of_features l =
        List.fold ~init:0 ~f:(fun x (y,_,_) -> x + y) l in
      let mxlen =Map.fold pmap ~init:0 ~f:(fun ~key ~data mx ->
                     max mx (List.length data)) in
      let feature_pmap = 
        Map.map pmap ~f:(total_of_features) in
      let mxp = Map.fold feature_pmap ~init:0 ~f:(fun ~key ~data mx ->
                    max mx data) in
      let feature_pmap = 
        Map.filter feature_pmap (fun total ->
            Float.((percent_of_pnts total) > threshold)) in
      print_endline
      @@ sprintf "filtered pmap size: %d, mxp: %d, mxlen: %d"
           (Map.length feature_pmap) mxp mxlen;
      let f superset = 
        let before = Superset.Inspection.count superset in
        let superset = 
          Features.with_featureset featureset superset
            ~init:(superset)
            ~f:(fun fname feature (superset) ->
              let before = Superset.Inspection.count superset in
              let superset =
                Trim.Default.trim @@ time ~name:fname feature superset in
              let after = Superset.Inspection.count superset in
              print_endline @@
                sprintf "with_featureset ~f %s %d, before %d, after %d, removed %d"
                  fname List.(length featureset) before after (before - after);

              superset
            ) in
        let after = Superset.Inspection.count superset in
        print_endline @@
          sprintf "converge: feat set %d, before %d, after %d, removed %d"
            List.(length featureset) before after (before - after);
        let before = Superset.Inspection.count superset in
        let superset = Trim.Default.trim superset in
        let after = Superset.Inspection.count superset in
        print_endline @@
          sprintf "featureset %d, before %d, after %d, removed %d"
            List.(length featureset) before after (before - after);

        let cache = Addr.Hash_set.create () in
        List.iter Map.(keys feature_pmap) ~f:(fun addr -> 
            Traverse.mark_descendent_bodies_at superset ~visited:cache addr
          );
        (* A visited address is expected to be a true positive, but under
         * some unexpectedly complicated possibilities at edge corner
         * cases, they could get marked bad by belonging on the edge. *)
        Features.clear_each superset visited;
        print_endline
        @@ sprintf "superset marked bad after callsite descendant removal: %d"
             (Hash_set.length @@ Superset.Core.copy_bad superset);
        print_endline
        @@ sprintf "feature_pmap: %d" @@ Map.length feature_pmap;
        let before = Superset.Inspection.count superset in
        let superset = Trim.Default.trim superset in
        let after = Superset.Inspection.count superset in
        print_endline @@
          sprintf "featureset %d, before %d, after %d, removed %d"
            List.(length featureset) before after (before - after);
        print_endline
        @@ sprintf "marked bad after trim: %d"
             (Hash_set.length @@ Superset.Core.copy_bad superset);
        superset,pmap
      in
      let name = sprintf "trim_with %d" options.rounds in
      time ~name (trim_with options.rounds f) superset
    )
  
(* TODO belongs in metrics or report *)
(* TODO needs comments for the role of results metrics setops
 * tracker *)
(* TODO split into smaller functions *)
let collect_results superset options pmap results setops =
  let ground_truth_file = options.ground_truth_file in
  let funcs,tps = 
    match ground_truth_file with
    | Some (gt) -> (
      let min_addr = Superset.Inspection.get_base superset in
      let tp = read_addrs Addr.(bitwidth min_addr) gt in
      let tps = Addr.Hash_set.create () in
      List.iter tp ~f:(Hash_set.add tps);
      tps,Hash_set.copy tps
    )
    | None -> (
      match options.ground_truth_bin with
      | Some (gt) ->
         print_endline "Using gt bin";
         let funcs = Addr.Hash_set.create () in
         Seq.iter (ground_truth_of_unstripped_bin gt |> ok_exn)
           ~f:(Hash_set.add funcs);
         funcs, Metrics.true_positives superset gt
      | None -> Addr.Hash_set.create (),Addr.Hash_set.create ()
    ) in
  let conflicts = Superset.Occlusion.find_all_conflicts superset in
  let conflicted_starts = Hash_set.fold funcs ~init:0 ~f:(fun count start ->
                  if Set.mem conflicts start then count + 1 else count
                ) in
  print_endline @@ sprintf "There are %d starts that are in conflict"
                     conflicted_starts;
  let occluded_starts =
    Hash_set.fold funcs ~init:0 ~f:(fun occluded start ->
        let behind = Addr.(start -- 20) in
        let range = Superset.Core.seq_of_addr_range behind 21 in
        let range = Seq.filter range ~f:(fun v -> not Addr.(v = start)) in
        if Seq.exists range 
          ~f:(fun current ->
            Seq.exists
              (Superset.Occlusion.conflict_seq_at superset current)
              ~f:(fun conflict -> Addr.((not (conflict = current))
                                        && conflict = start)
              )) then occluded + 1 else occluded
      ) in
  print_endline @@ sprintf "There are %d starts that are occlusive"
                     occluded_starts;
  let _ =
    if options.collect_report then (
      print_endline @@ sprintf "Report with %d tps:" @@ Hash_set.length tps;
      let threshold = (pnts_of_percent options.tp_threshold) in
      let init = String.Map.empty in
      let pmap =
        Map.mapi pmap ~f:(fun ~key ~data ->
            List.fold data ~init:String.Map.empty
              ~f:(fun dist (p,addr,ftname) ->
                String.Map.update dist ftname ~f:(fun total ->
                    (Option.value total ~default:0) + p
                  )
              )
          ) in
      let reports =
        List.fold (Addr.Map.to_alist pmap) ~init ~f:(fun reports (p_at,fttot) ->
            String.Map.fold fttot ~init:reports
              ~f:(fun ~key ~data reports ->
                let name = key in
                let p = data in
                String.Map.update reports name ~f:(fun dist ->
                    let dist =
                      Option.value dist
                        ~default:(Report.Distribution.empty threshold)
                    in Report.Distribution.add dist tps (p,p_at,name)
                  )
              )
          ) in
      Map.iteri reports ~f:(fun ~key ~data ->
          let dist = data in
          print_endline @@ key ^ " " ^ (Sexp.to_string
                                     @@ Report.Distribution.sexp_of_t
                                          dist);
        );
    ) else () in
  if Hash_set.length tps > 0 then
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
    let results = String.Map.set results "True Positives" tps in
    let results = String.Map.set results "False Positives" fps in
    let results = String.Map.set results "False Negatives" fns in
    results
  else results

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


module type Provider = sig
  val options : t
end

module With_options(Conf : Provider)  = struct
  open Conf
  open Metrics.Opts
  open Or_error
  open Format

  let with_analyses superset analyses =
    Trim.Default.trim @@
      List.fold analyses ~init:superset ~f:(fun superset analyze ->
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

  (* TODO would be better having been passed the pmap *)
  let collect_map_distribution superset feature f =
    if options.collect_report then (
      let tps = Addr.Hash_set.create () in
      let fps = Addr.Hash_set.create () in
      let ro = Addr.Hash_set.create () in
      match Map.find Features.exfiltmap feature with
      | Some (extractf, filterf) ->
         (* TODO need to correct the pmap type *)
         let pmap =
           Features.make_featurepmap options.featureset superset in
         let pmap = Features.fixpoint_map superset pmap in
         (* TODO with_featurepmap *)
         let pmap = Map.filter_mapi pmap ~f:(fun ~key ~data ->
                        List.find data ~f:(fun (p,x,feat) ->
                            String.equal feature feat
                          )
                      ) in
         let report = 
           Report.collect_map_report superset extractf filterf tps
             ro fps pmap in
         print_endline @@ Report.format_report report
      | None -> ()
    )

  let collect_set_distribution superset feature f =
    if options.collect_report then (
      let tps = Addr.Hash_set.create () in
      let fps = Addr.Hash_set.create () in
      let ro = Addr.Hash_set.create () in
      match Map.find Features.exfiltset feature with
      | Some (extractf,_) ->
         let pmap =
           Features.make_featurepmap options.featureset superset in
         let pmap = Features.fixpoint_map superset pmap in
         (* TODO with_featurepmap *)
         let pmap = Map.filter_mapi pmap ~f:(fun ~key ~data ->
                        List.find data ~f:(fun (p,x,feat) ->
                            String.equal feature feat
                          )
                      ) in
         let report = 
           Report.collect_set_report superset extractf (fun _ x -> x)
             tps ro fps pmap in
         print_endline @@ Report.format_report report
      | None -> ()
    )

  (* TODO rename checkpoint *)
  let checkpoint bin invariants =
    let backend = options.disassembler in
    match options.import with
    | Some suffix ->
      let graph = bin ^ "_" ^ suffix ^ ".graph" in
      time ~name:"import" Superset.import options.disassembler bin
        graph
    | None -> 
      let invariants = (Invariants.tag_success::invariants) in
      Superset.superset_disasm_of_file ~backend bin
        ~f:(Invariants.tag ~invariants)

  let args_to_funcs args funcs =
    let l = List.filter_map args
      ~f:(fun arg ->
        List.find funcs ~f:(fun (name,f) ->
            String.equal arg name
          )
      ) in
    List.map l ~f:snd    
      
  let phases =
    args_to_funcs options.phases Invariants.default_tags

  let analyses = args_to_funcs options.analyses list_analyses

  let with_options superset =
    let trim =
      match options.trim_method with
      | Simple -> Trim.Default.trim
      | DeadBlockResistant -> Trim.DeadblockTolerant.trim
      | Disabled -> Trim.Disabled.trim in
    let before = Superset.Inspection.count superset in
    let (trim,setops) = build_setops trim options.setops in
    let superset = trim superset in
    let after = Superset.Inspection.count superset in
    print_endline @@
      sprintf "invariants: before %d, after %d, removed %d"
        before after (before - after);
    let before = after in
    let superset = with_analyses superset analyses in
    let superset = trim superset in
    let after = Superset.Inspection.count superset in
    print_endline @@
      sprintf "%d analyses: before %d, after %d, removed %d"
        List.(length analyses) before after (before - after);
    let superset,pmap = converge options superset in
    let results = apply_setops setops options.setops in
    let results =
      collect_results superset
        options pmap results setops in
    process_cut superset options results;
    superset
       
  let main () =
    let superset = checkpoint options.target phases in
    let superset = with_options superset in
    print_endline "with_options finished";
    let () =
      match options.export with
      | None -> ()
      | Some suffix -> (
        (*with_invariants superset invariants*)
        let f =
          if String.equal suffix "" then options.target else
            (options.target ^ "_" ^ suffix) in
        Superset.export f superset;
      ) in
    (* TODO duplicate of collect_results *)
    match options.ground_truth_bin with
    | Some bin ->
       if options.save_gt then (
         let gt = Metrics.ground_truth_of_unstripped_bin
                    bin |> ok_exn in
         let gt = Seq.map gt ~f:Addr.to_string in
         let base = Filename.basename bin in
         let addrs_file = Out_channel.create ("./" ^ base ^ "_gt.txt")
         in
         Out_channel.output_lines addrs_file @@ Seq.to_list gt;
         Out_channel.flush addrs_file;
         Out_channel.close addrs_file;
       ) else ();
       (* TODO replace gather_metrics with report *)
       Metrics.compute_metrics ~bin superset
    | None -> ()

end
