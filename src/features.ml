open Core_kernel
open Bap.Std

module Dis = Disasm_expert

module type Heurism = sig
  type t
  val name : string
  val impl : Superset.t -> t
end

module HeurismSet(H : Heurism) = struct
  (* TODO make the cache *)
  module Cache = struct
  end
end

module HeurismMap(H : Heurism) = struct
  (* TODO make the cache *)
  module Cache = struct
  end
end

let default_features = [
  "ImgEntry";
  (*"NoExit";*)
  (*"LoopsWithBreak";*)
  "BranchViolations";
  (*"LayerViolations";*)
  "TrimLimitedClamped";
  "Callsites3";
  (*"TrimFixpointGrammar";
    "TrimFixpointTails";*)
  (*"Clamped";
    "SCC";
    "LoopGrammar";
    "CallsiteLineage";
    "SSA";*)
  (*"FreeVarSSA";*)
  (*"Grammar";*)
  (*"Constant";*)
]
let default_features = List.rev default_features

let transform = Hash_set.fold ~init:Addr.Set.empty ~f:Set.add

(* TODO belongs in superset *)
let clear_each superset visited =
  Hash_set.iter visited ~f:(fun tp -> 
      Superset.Core.clear_bad superset tp
    )

(* TODO belongs in superset *)
let get_non_fall_through_edges superset = 
  Superset.ISG.fold_edges superset
    (fun child parent jmps -> 
       if Superset.is_fall_through superset parent child then
         Map.set jmps child parent
       else jmps
    ) Addr.Map.empty


(** A callsite is a location which is shared as the target of a call
    by several other locations in the binary. *)
let get_callsites ?(threshold=6) superset =
  let callsites = Addr.Hash_set.create () in
  Superset.ISG.iter_vertex superset
    (fun v -> 
       let callers = Superset.ISG.ancestors superset v in
       let num_callers = 
         List.fold callers ~init:0 ~f:(fun total caller -> 
             if not (Superset.is_fall_through superset caller v) then
               total + 1
             else total) in
       if num_callers > threshold then (
         Hash_set.add callsites v;
       )
    ) ;
  callsites

(** Adds to the set visited the set of reachable descendents of a
    callsite of a given sufficient threshold number of external
    callers *)
(* TODO this should just be collect_descendants *)
let tag_callsites visited ?callsites superset =
  let callsites = Option.value callsites 
      ~default:(get_callsites ~threshold:6 superset) in
  Hash_set.iter callsites ~f:(fun callsite ->
      Traverse.with_descendents_at ~visited
        ?post:None ?pre:None superset callsite;
    );
  superset

(* TODO belongs in superset *)
(* could be factored to work with conflict_seq_at *)
(* could be factored to work with find_all_conflicts *)
let find_free_insns superset = 
  let mem = Superset.Core.mem superset in
  let all_conflicts = Addr.Hash_set.create () in
  let to_clamp =
    Superset.Core.fold superset ~init:([])
      ~f:(fun ~key ~data to_clamp ->
          let (addr,(memory,_)) = key, data in
          let len = Memory.length memory in
          let conflicts = Superset.Occlusion.range_seq_of_conflicts
              ~mem addr len in
          let no_conflicts = Seq.is_empty conflicts in
          Seq.iter conflicts ~f:(fun c -> Hash_set.add all_conflicts c);
          if no_conflicts && not Hash_set.(mem all_conflicts addr) then
            addr :: to_clamp
          else (
            to_clamp
          )
        ) in
  to_clamp
(*Hash_set.fold all_conflicts ~init:to_clamp ~f:Set.remove*)

let restricted_clamp superset = 
  let entries = Superset.entries_of_isg superset in
  let conflicts = Superset.Occlusion.find_all_conflicts superset in
  let to_clamp = ref Addr.Set.empty in
  Hash_set.iter entries ~f:(fun entry -> 
      let b = ref false in
      let pre v = 
        if Addr.(v = entry) then
          b := false
        else if not (!b) then
          if Set.mem conflicts v then
            b := true
          else to_clamp := Set.add (!to_clamp) v
      in Traverse.with_ancestors_at ~post:(fun _ -> ()) ~pre superset entry;
    );
  !to_clamp

let extended_clamp superset = 
  let to_clamp = find_free_insns superset in
  List.fold to_clamp ~init:Addr.Set.empty ~f:(fun to_clamp clamp -> 
      let _, to_clamp =
        Superset.ISG.dfs_fold superset
          ~pre:(fun (struck,to_clamp) addr ->
            if struck then (struck,to_clamp) else
              let conflicts =
                Superset.Occlusion.conflicts_within_insn_at
                  superset addr in
              let no_conflicts = Set.length conflicts = 0 in
              (*let conflicts = Superset.Occlusion.parent_conflict_at
                     insn_risg insn_map addr in
                   let no_conflicts = Set.length conflicts = 0
                                    && no_conflicts in*)
              if no_conflicts then (struck, Set.(add to_clamp addr))
              else (true, to_clamp)
          ) ~post:(fun x _ -> x) (false, to_clamp) clamp 
      in to_clamp
    )

(* TODO this is duplicated *)
let extract_loop_addrs superset = 
  let loop_addrs = Superset.ISG.raw_loops superset in
  List.fold_left ~init:Addr.Map.empty loop_addrs
    ~f:(fun addrs loop ->
        if List.length loop >= 2 then
          Option.value ~default:addrs 
            Option.(map List.(hd loop) ~f:(fun addr -> 
                Map.set addrs addr loop))
        else addrs
      )

let extract_filtered_loop_addrs superset =
  let loop_addrs = extract_loop_addrs superset in
  Map.filteri loop_addrs ~f:(fun ~key ~data ->
      List.length data > 20)

(* TODO what is this for? *)
let extract_constants superset =
  let width =
    Addr.bitwidth Superset.Inspection.(get_base superset) in
  let s = Size.of_int_exn width in
  let imgmem =
    Memmap.to_sequence @@ Superset.Inspection.get_memmap superset in
  let addrs =
    Seq.bind imgmem
      ~f:(fun (segment,_) ->
          let words_of_mem s mem =
            let rec yield_next addr =
              let open Seq.Generator in
              match Memory.view ~word_size:s ~from:addr mem with
              | Ok next ->
                yield next >>= fun () -> yield_next (Addr.succ addr)
              | _ -> return () in
            Sequence.Generator.run (yield_next Memory.(min_addr mem))
          in words_of_mem s segment
        ) in
  Seq.fold ~init:Addr.Map.empty  addrs
    ~f:(fun constants m ->
        let constant = Memory.(m ^ (min_addr m)) in
        match constant with
        | Ok constant -> 
          if Superset.Inspection.contains_addr superset constant
          && Superset.Core.(mem superset constant) then
            Map.set constants Memory.(min_addr m) constant
          else constants
        | _ -> constants
      )

(* pre is called from descendant to ancestor order, so we want to
   check for usage and put that into a map, and then for define on
   post visitation, when coming back down from ancestors back to
   descendants (as execution would move). *)
let pre_ssa superset lift factors var_use addr =
  ()
          
let post_ssa_with superset lift defs addr f = 
  match Superset.Core.lift_at superset addr with
  | Some (bil) -> (
    let use_vars = Abstract_ssa.use_ssa bil in
    let use_mems = Abstract_ssa.use_mem_ssa bil in
    Set.iter use_vars ~f:(fun use_var ->
        match Map.find !defs use_var with
        | Some waddr -> f waddr addr
        | None -> ()
      );
    Set.iter use_mems ~f:(fun use_mem ->
        match Map.find !defs use_mem with
        | Some waddr -> f waddr addr
        | None -> ()
      );
    let var_defs = Abstract_ssa.def_ssa bil in
    let mem_defs = Abstract_ssa.def_mem_ssa bil in
    defs := Set.fold ~init:!defs var_defs ~f:(fun defs e ->
                Map.set defs ~key:e ~data:addr
              );
    defs := Set.fold ~init:!defs mem_defs ~f:(fun defs e ->
                Map.set defs ~key:e ~data:addr
              );
  )
  | None -> ()
          
let extract_ssa_to_map superset =
  let var_use = ref Exp.Map.empty in
  let defuse_map = ref Addr.Map.empty in
  let add_to_map def use = 
    defuse_map := Map.set !defuse_map def use in
  let lift (mem, insn) =
    Superset.Core.lift_insn superset ( (mem, insn)) in
  let pre = pre_ssa superset lift () var_use in
  let post addr = post_ssa_with superset lift var_use
      addr add_to_map in
  let entries = Superset.entries_of_isg superset in
  let visited = Addr.Hash_set.create () in
  Hash_set.iter entries ~f:(fun addr ->
      Traverse.with_ancestors_at superset ~visited addr ~post ~pre;
      var_use := Exp.Map.empty
    );
  !defuse_map

let pre_freevarssa superset lift factors var_use addr =
  match Superset.Core.lift_at superset addr with
  | Some (bil) -> (
    try 
      let use_vars = Abstract_ssa.use_freevars bil in
      Set.iter use_vars ~f:(fun use_var -> 
          var_use := Map.set !var_use use_var addr
        )
    with _ -> ()
  )
  | None -> ()
  
let post_freevarssa_with superset lift var_use addr f = 
  match Superset.Core.lift_at superset addr with
  | Some (bil) -> (
    try 
      let use_vars = Abstract_ssa.use_freevars bil in
      let var_defs = Abstract_ssa.def_freevars bil in
      Set.iter var_defs ~f:(fun var_def -> 
          match Map.find !var_use var_def with
          | Some(waddr) ->
             if not Set.(mem use_vars var_def) then (
               f waddr addr
             )
          | None -> ()
        );
      Set.iter use_vars ~f:(fun use_var -> 
          var_use := Map.remove !var_use use_var;
        );
      Set.iter var_defs ~f:(fun write_reg -> 
          var_use := Map.remove !var_use write_reg
        )
     with _ -> ()
  )
  | None -> ()
  
let extract_freevarssa_to_map superset =
  let var_use = ref Var.Map.empty in
  let defuse_map = Addr.Table.create () in
  let add_to_map def use = 
    Addr.Table.set defuse_map ~key:def ~data:use in
  let lift (mem, insn) =
    Superset.Core.lift_insn superset ((mem, insn)) in
  let pre = pre_freevarssa superset lift () var_use in
  let post addr = post_freevarssa_with superset lift var_use
      addr add_to_map in
  let entries = Superset.entries_of_isg superset in
  (* TODO because visited is being used, there is a possibility of
  some missed results. *)
  let visited = Addr.Hash_set.create () in
  Hash_set.iter entries ~f:(fun addr -> 
      Traverse.with_ancestors_at superset ~visited addr ~post ~pre;
      (* TODO: I don't think that this is a very good implementation *)
      var_use := Var.Map.empty
    );
  defuse_map

let pre_mem_ssa superset lift factors var_use addr =
  match Superset.Core.lift_at superset addr with
  | Some (bil) -> (
    try 
      let use_vars = Abstract_ssa.use_mem_ssa bil in
      Set.iter use_vars ~f:(fun use_var -> 
          var_use := Map.set !var_use use_var addr
        )
    with _ -> ()
  )
  | None -> ()

let post_mem_ssa_with superset lift var_use addr f = 
  match Superset.Core.lift_at superset addr with
  | Some (bil) -> (
    try 
      let use_vars = Abstract_ssa.use_mem_ssa bil in
      let var_defs = Abstract_ssa.def_mem_ssa bil in
      Set.iter var_defs ~f:(fun var_def -> 
          match Map.find !var_use var_def with
          | Some(waddr) ->
             (*if not Set.(mem use_vars var_def) then ( *)
               f waddr addr
             (* ) *)
          | None -> ()
        );
      Set.iter use_vars ~f:(fun use_var -> 
          var_use := Map.remove !var_use use_var;
        );
      Set.iter var_defs ~f:(fun write_reg -> 
          var_use := Map.remove !var_use write_reg
        )
     with _ -> ()
  )
  | None -> ()

let extract_mem_ssa_to_map superset =
  let var_use = ref Exp.Map.empty in
  let defuse_map = Addr.Table.create () in
  let add_to_map def use = 
    Addr.Table.set defuse_map ~key:def ~data:use in
  let lift (mem, insn) =
    Superset.Core.lift_insn superset ((mem, insn)) in
  let pre = pre_mem_ssa superset lift () var_use in
  let post addr = post_mem_ssa_with superset lift var_use
      addr add_to_map in
  let entries = Superset.entries_of_isg superset in
  (* TODO because visited is being used, there is a possibility of
  some missed results. *)
  let visited = Addr.Hash_set.create () in
  Hash_set.iter entries ~f:(fun addr -> 
      Traverse.with_ancestors_at superset ~visited addr ~post ~pre;
      (* TODO: I don't think that this is a very good implementation *)
      var_use := Exp.Map.empty
    );
  defuse_map
  
let extract_cross_section_jmps superset = 
  let segments = Superset.Inspection.get_memmap superset in
  let cross_section_edges = Superset.ISG.fold_edges superset
      (fun src dst csedges ->
         let collect_minaddrs addr =
           let seg = Memmap.lookup segments addr in
           Seq.fold seg ~init:Addr.Set.empty ~f:(fun s1 (mem,_) ->
               Set.add s1 @@ Memory.min_addr mem
             ) in
         let s1 = collect_minaddrs src in
         let s2 = collect_minaddrs dst in
         if not (Set.(length @@ inter s1 s2) >= 1) then
           let ft1 = Superset.is_fall_through superset src dst in
           let ft2 = Superset.is_fall_through superset dst src in
           if (ft1 || ft2) then (
             (*Superset_risg.G.remove_edge insn_risg src dst;*)
             Map.set csedges src dst
           ) else csedges
         else csedges
      ) Addr.Map.empty in
  cross_section_edges

(* TODO clamped name is out of date. *)
let extract_trim_clamped superset = 
  let to_clamp = find_free_insns superset in
  let visited = Addr.Hash_set.create () in
  let datas   = Addr.Hash_set.create () in
  List.iter to_clamp ~f:(fun c -> 
      if not Hash_set.(mem visited c) then
        if Superset.Core.mem superset c then (
          Traverse.mark_descendent_bodies_at
            ~visited ~datas superset c
        )
    );
  (*Hash_set.iter datas ~f:(fun d -> 
      if Hash_set.(mem visited d) || Set.(mem to_clamp d) then
        Superset.Core.clear_bad superset d
    );*)
  clear_each superset visited;
  List.iter to_clamp ~f:(Superset.Core.clear_bad superset);
  superset

(* TODO duplicated elsewhere. replace with landmark *)
let time ?(name="") f x =
  let t = Sys.time() in
  let fx = f x in
  let s = sprintf "%s execution time: %fs\n" name (Sys.time() -. t) in
  print_endline s;
  fx

(* TODO call to Safely.protected *)
(* TODO can optimize this just for a few seconds off, but it results
 * in lower accuracy *)
let extract_trim_limited_clamped superset =
  let protection = Addr.Hash_set.create () in
  if Hash_set.length protection = 0 then (
    let callsites = get_callsites ~threshold:0 superset in
    let f s = tag_callsites protection ~callsites s in
    let superset = time ~name:"tagging callsites: " f superset in
    Superset.Core.clear_all_bad superset
  );
  Superset.Core.clear_all_bad superset;
  let superset = time ~name:"extract_trim_clamped "
                   extract_trim_clamped superset in
  clear_each superset protection; superset

(* TODO move to fixpoint *)
let fixpoint_descendants superset extractf depth = 
  let rec fix_descendants cur_features d =
    if d >= depth then
      cur_features
    else
      let visited = Addr.Hash_set.create () in
      let subset_features = Addr.Hash_set.create () in
      Hash_set.iter cur_features ~f:(fun cur ->
          if not Hash_set.(mem visited cur) then
            Traverse.with_descendents_at superset
              ~pre:(fun v ->
                  if Hash_set.(mem cur_features v)
                  && not Addr.(cur = v) then
                    Hash_set.add subset_features v
                ) ~visited cur
          else Hash_set.add subset_features cur
        );
      fix_descendants subset_features (d+1)
  in
  let cur_features = extractf superset in
  fix_descendants cur_features 0

(* TODO change type of pmap from Addr.Map to ...? *)
(* Only contain convergence ancestors points *)
(* TODO move to fixpoint *)
(* TODO possibly operates incorrectly! *)
let fixpoint_map superset feature_pmap = 
  let visited = Addr.Hash_set.create () in
  let entries = Superset.frond_of_isg superset in
  Hash_set.fold ~init:feature_pmap entries ~f:(fun feature_pmap cur -> 
      if not Hash_set.(mem visited cur) then
        let prev = ref [] in
        let feature_pmap = ref feature_pmap in
        Traverse.with_descendents_at ~pre:(fun v ->
            match Map.find !feature_pmap v with
            | None -> ()
            | Some(p) ->
              prev :=  List.append p  !prev;
              feature_pmap := Map.set !feature_pmap v !prev;
          ) ~visited superset cur;
        !feature_pmap
      else feature_pmap
    )

(* TODO move to fixpoint *)
let fixpoint_grammar superset depth =
  let branches = Superset.get_branches superset in
  print_endline @@
    sprintf "\tfixpoint_grammar: %d" (Hash_set.length branches);
  let extractf superset = 
    Superset.get_branches superset in
  fixpoint_descendants superset extractf depth

(* TODO move this to fixpoint *)
let fixpoint_ssa superset depth = 
  let extractf superset = 
    let ssa_map = extract_ssa_to_map superset in
    let ssa = Addr.Hash_set.create () in
    List.iter Map.(data ssa_map) ~f:Hash_set.(add ssa);
    ssa in
  fixpoint_descendants superset extractf depth

let fixpoint_memssa superset = 
  let extractf superset = 
    let ssa_map = extract_mem_ssa_to_map superset in
    let ssa = Addr.Hash_set.create () in
    let memssa = Addr.Hash_set.create () in
    List.iter Addr.Table.(data ssa_map) ~f:Hash_set.(add memssa);
    ssa in
  fixpoint_descendants superset extractf 2

let fixpoint_freevarssa superset depth = 
  let extractf superset = 
    let freevars_map = extract_freevarssa_to_map superset in
    let freevars = Addr.Hash_set.create () in
    List.iter Addr.Table.(data freevars_map) ~f:Hash_set.(add freevars);
    freevars in
  fixpoint_descendants superset extractf depth

(* TODO measure this effectively *)
let fixpoint_tails superset = 
  let extractf superset =
    let conflicts = Superset.Occlusion.find_all_conflicts superset in
    let tails_map = 
      Decision_trees.tails_of_conflicts superset conflicts in 
    let tails = Addr.Hash_set.create () in
    List.iter Map.(keys tails_map) ~f:Hash_set.(add tails);
    tails
  in
  fixpoint_descendants superset extractf 4

(* TODO all features is not all features *)
let allfeatures = 
  "RestrictedClamped"      ::
  "ExtendedClamped"        ::
  "ClassicGrammar"         ::
  "LinearGrammar"          ::
  "UnfilteredGrammar"      ::
  "FalseBranchMap"         ::
  "FilteredFalseBranchMap" ::
  "UnfilteredSCC"          ::
  "FreeVarSSA"             ::
  "FixpointGrammar"        :: 
  "FixpointSSA"            ::
  "FixpointFreevarSSA"     :: 
  "FixpointTails"          :: 
  default_features

let get_branches superset = 
  let branches = Superset.get_branches superset in
  transform branches

  (*
(* TODO poorly named, can't be complete *)
(* TODO get branches, filter by true positives *)
let fp_branches superset branches =
  let name = Superset.Inspection.filename superset in
  let name = Option.value_exn name in
  let open Bap_knowledge in
  let open Bap_core_theory in
  let open KB.Syntax in
  (*KB.promise fp_branches @@ fun label ->
  KB.collect Metrics.Cache.true_positives  @@ fun true_positives ->*)
  let true_positives = Metrics.true_positives superset name in
  let branches = 
    Hash_set.fold true_positives ~init:branches ~f:Set.remove in
  Set.fold branches ~init:Addr.Map.empty ~f:(fun fpbranchmap fpbranch ->
      let target = 
        List.find_exn Superset.ISG.(descendants superset fpbranch) 
          ~f:Superset.(is_fall_through superset fpbranch) in
      Map.set fpbranchmap fpbranch target
    )

let extract_fp_branches superset = 
  let branches = get_branches superset in
  fp_branches superset branches

let extract_filter_fp_branches superset =
  let superset = Invariants.tag_layer_violations superset in
  let violations = Superset.Core.copy_bad superset in
  let _ = Superset.Core.clear_all_bad superset in
  let branches = get_branches superset in
  let branches = Set.diff branches (transform violations) in
  let superset = Invariants.tag_branch_violations superset in
  let violations = Superset.Core.copy_bad superset in
  let _ = Superset.Core.clear_all_bad superset in
  let branches = Set.diff branches (transform violations) in
  branch_map_of_branches superset branches*)

let linear_grammar superset =
  let entries = Superset.entries_of_isg superset in
  transform Grammar.(linear_branch_sweep superset entries)

let classic_grammar superset =
  transform Grammar.(identify_branches superset)

let branch_violations superset = 
  let superset = Invariants.tag_branch_violations superset in
  let violations = Superset.Core.copy_bad superset in
  let _ = Superset.Core.clear_all_bad superset in
  transform violations

let layer_violations superset = 
  let superset = Invariants.tag_layer_violations superset in
  let violations = Superset.Core.copy_bad superset in
  let _ = Superset.Core.clear_all_bad superset in
  transform violations

let filtered_grammar superset = 
  let violations = (layer_violations superset) in
  let branches = Set.diff (get_branches superset) violations in
  Set.diff branches (branch_violations superset)

(* TODO: duplicated in other parts of the code *)
let extract_loops_to_set superset =
  let loops = Superset.ISG.raw_loops superset in
  let loops = List.filter loops ~f:(fun l -> List.length l >= 2) in
  Sheathed.addrs_of_loops loops

(* TODO: duplicated in other parts of the code *)
let extract_filter_loops superset =
  Sheathed.addrs_of_filtered_loops superset

(* TODO: duplicated in other parts of the code *)
let loop_grammar superset =
  let superset = Invariants.tag_layer_violations superset in
  let violations = Superset.Core.copy_bad superset in
  let _ = Superset.Core.clear_all_bad superset in
  let branches = get_branches superset in
  let branches = Set.diff branches (transform violations) in
  let superset = Invariants.tag_branch_violations superset in
  let violations = Superset.Core.copy_bad superset in
  let _ = Superset.Core.clear_all_bad superset in
  let branches = Set.diff branches (transform violations) in
  let loop_addrs = extract_loops_to_set superset in
  Set.filter branches ~f:(fun x -> Set.(mem loop_addrs x))

(* TODO is this necessary? *)
let extract_loops_with_break superset =
  let loop_addrs = extract_loop_addrs superset in
  Map.fold ~init:Addr.Set.empty loop_addrs ~f:(fun ~key ~data loops -> 
      let loop = List.fold ~init:Addr.Set.empty data ~f:Set.add in
      let has_break = List.exists  data
                        ~f:(fun addr ->
            let targets = Superset.ISG.descendants superset addr in
            List.exists targets
                ~f:(fun x -> not Set.(mem loop x))
            ) in
      if has_break then Set.union loops loop else loops
    )

let extract_exitless superset = 
  let returned = Addr.Hash_set.create () in
  let entries = Superset.entries_of_isg superset in
  Hash_set.iter entries ~f:(fun entry -> 
      Traverse.with_ancestors_at superset
        ?post:None ~pre:(Hash_set.add returned) entry
    );
  Superset.Core.fold superset ~f:(fun ~key ~data exitless ->
      let v = key in
      if not (Hash_set.mem returned v) 
      then Set.add exitless v else exitless
    ) ~init:Addr.Set.empty

let extract_constants_to_set superset = 
  let constants = extract_constants superset in
  Map.fold constants ~init:Addr.Set.empty ~f:(fun ~key ~data consts -> 
      Set.add consts data
    )

(* TODO move to fixpoint *)
let collect_descendants superset ?insn_isg ?visited ?datas targets = 
  let visited = Option.value visited ~default:(Addr.Hash_set.create ()) in
  let datas = Option.value datas ~default:(Addr.Hash_set.create ()) in
  Hash_set.iter targets ~f:(fun v -> 
      if not Hash_set.(mem visited v) then
        Traverse.mark_descendent_bodies_at ~visited ~datas superset v      
    )

let extract_img_entry superset =
  let e = Addr.Set.empty in
  match Superset.Inspection.get_main_entry superset with
  | Some mentry -> 
    let s = sprintf "entry: %s" 
        Addr.(to_string  mentry) in
    print_endline s;
    Set.add e mentry 
  | None -> e

let extract_trim_callsites superset =
  let visited = Addr.Hash_set.create () in
  let callsites = get_callsites ~threshold:2 superset in
  let protection = get_callsites ~threshold:0 superset in
  collect_descendants superset ~visited protection;
  Superset.Core.clear_all_bad superset;
  let superset = tag_callsites visited ~callsites superset in
  clear_each superset visited;
  superset
let extract_trim_loops_with_break superset = 
  (*let loops = extract_loops_with_break superset in*)
  superset
let extract_trim_entry superset =
  let imgentry = extract_img_entry superset in
  Set.iter imgentry ~f:Traverse.(mark_descendent_bodies_at superset);
  superset

(* TODO remove all Invariants from features *)
let extract_trim_branch_violations superset = 
  Invariants.tag_branch_violations superset
let extract_trim_layer_violations superset =
  Invariants.tag_layer_violations superset

let extract_trim_noexit superset =
  let exitless = extract_exitless superset in
  Set.iter exitless ~f:Superset.Core.(mark_bad superset);
  superset

let extract_trim_fixpoint_grammar superset =
  (* TODO The fixpoint threshold should be a parameter *)
  let gdesc = fixpoint_grammar superset 10 in
  let visited = Addr.Hash_set.create () in
  let datas   = Addr.Hash_set.create () in
  let callsites = get_callsites ~threshold:0 superset in
  let superset = tag_callsites visited ~callsites superset in
  Superset.Core.clear_all_bad superset;
  collect_descendants ~visited superset gdesc;
  Hash_set.iter datas ~f:(fun d -> 
      if Hash_set.(mem visited d) || Hash_set.(mem gdesc d) then
        Superset.Core.clear_bad superset d
    );
  clear_each superset visited;
  clear_each superset gdesc;
  superset  

let extract_trim_fixpoint_ssa superset =
  (* TODO The fixpoint threshold should be a parameter *)
  let gdesc = fixpoint_ssa superset 6 in
  let visited = Addr.Hash_set.create () in
  let datas   = Addr.Hash_set.create () in
  let callsites = get_callsites ~threshold:0 superset in
  (*collect_descendants ~visited ~insn_isg superset callsites;*)
  let superset = tag_callsites visited ~callsites superset in
  Superset.Core.clear_all_bad superset;
  collect_descendants ~visited superset gdesc;
  Hash_set.iter datas ~f:(fun d -> 
      if Hash_set.(mem visited d) || Hash_set.(mem gdesc d) then
        Superset.Core.clear_bad superset d
    );
  clear_each superset visited;
  superset  

let extract_trim_fixpoint_freevarssa superset =
  (* TODO The fixpoint threshold should be a parameter *)
  let gdesc = fixpoint_freevarssa superset 6 in
  let visited = Addr.Hash_set.create () in
  let datas   = Addr.Hash_set.create () in
  let callsites = get_callsites ~threshold:0 superset in
  (*collect_descendants ~visited ~insn_isg superset callsites;*)
  let superset = tag_callsites visited ~callsites superset in
  Superset.Core.clear_all_bad superset;
  collect_descendants ~visited superset gdesc;
  Hash_set.iter datas ~f:(fun d -> 
      if Hash_set.(mem visited d) || Hash_set.(mem gdesc d) then
        Superset.Core.clear_bad superset d
    );
  clear_each superset visited;
  superset

let extract_trim_fixpoint_tails superset = 
  let tdesc = fixpoint_tails superset in
  let visited = Addr.Hash_set.create () in
  let datas   = Addr.Hash_set.create () in
  let callsites = get_callsites ~threshold:0 superset in
  let superset = tag_callsites visited ~callsites superset in
  Superset.Core.clear_all_bad superset;
  Hash_set.iter tdesc ~f:(fun v -> 
      if not Hash_set.(mem visited v) then
        Traverse.mark_descendent_bodies_at ~visited ~datas superset v
    );
  Hash_set.iter datas ~f:(fun d -> 
      if Hash_set.(mem visited d) || Hash_set.(mem tdesc d) then
        Superset.Core.clear_bad superset d
    );
  clear_each superset visited;
  superset

let discard_edges superset =
  Superset.ISG.fold_edges superset
    (fun child parent superset -> 
       if not Superset.(is_fall_through superset parent child) then (
         match Superset.Core.lookup superset parent with
         | None -> superset
         | Some (mem, insn) -> 
           match insn with 
           | Some(insn) -> 
             let insn = Insn.of_basic insn in
             if Insn.(is Insn.call insn) then
               Superset.ISG.unlink superset child parent
             else superset
           | None -> superset
       ) else superset
    ) superset
  (*let edges = Superset.get_non_fall_through_edges superset in*)

type extractor = (Superset.t -> Addr.Set.t)
type ('b) mapextractor = (Superset.t -> 'b Addr.Map.t)
type setfilter = (Superset.t -> Addr.Set.t -> Addr.Set.t)
type ('b) mapfilter = (Superset.t -> 'b Addr.Map.t -> 'b Addr.Map.t)
type setexfilt = extractor * setfilter
type ('a, 'b) mapexfilt = ('b) mapextractor * ('b) mapfilter
let unfiltered _ = ident

let get_gfather descendents_at = 
  Addr.Table.fold descendents_at ~init:None
    ~f:(fun ~key ~data cur ->
      match cur with
      | None -> Some (key,data)
      | Some (cur_addr,cur_mx) ->
         Some Int64.(if cur_mx > data then cur_addr,cur_mx else (key,data))
    )
                 
let top_ancestors descendents_at superset remaining = 
  let visited = Addr.Hash_set.create ()  in
  let all_insns = Addr.Hash_set.create ()  in
  let rec process remaining =
    if remaining > 0 then
      match get_gfather descendents_at with
      | None -> transform visited
      | Some (gf,m) ->
         let pre v =
           Addr.Table.remove descendents_at v;
           Hash_set.add all_insns v;
           Superset.Occlusion.with_data_of_insn
             superset v ~f:(Addr.Table.remove descendents_at) in
         Traverse.with_descendents_at ~visited ~pre superset gf;
         print_endline
         @@ sprintf "Grandfather at %s with %s, counted as %d"
              (Addr.to_string gf)
              Int64.(to_string m)
              (Hash_set.(length visited));
         Hash_set.clear visited;
         process (remaining - 1)
    else transform all_insns in process remaining

(* For any given address, should be able to know it's ID, which
 * represents it's membership in an exclusive group of reachable addresses.
 * allows efficient lookup of  *)
type lineage =
  | Id of Uuidm.t     (* used to merge multiple roots. *)
  | Link of addr      (* Between the source and final group, there is an
                       * adjustable link. *)
  | Root of addr      (* Whether it be a function entry or exit, this
                       * represents  *)
  | Descendents of (Addr.Hash_set.t * Addr.Hash_set.t)
type partial_utree =
  lineage Addr.Table.t

(* In this algorithm, a *mostly* memoized union find tree will be
  used. With this, a level of indirection and history can be used to
  provide an efficient addition, removal, and query for the problem of
  determining descendnt containment between addresses. The problem we
  are trying to solve is to determine the set(s) that contains
  other(s). This is hard to do efficiently, because the false positive
  possibilities mean that there are not only meany sets, but also that
  possibly along the lineage that possibly some unexpected violations
  of well formed assembler could occur. Efficiency can be achieved by
  recognizing that the data structure can change and resolve local
  inquiries, and not a single final and comprehensive round.To do
  this, a union-find for which entire groups can be efficiently
  re-pointed to new targets as the space is explored is maintained as
  a tree with all leaves pointed toward the root. There are some
  challenges, though in that whether exploring from exit to entry or
  entry to exit there may be more than one root (exit). Also, some
  false positive branches may force the tree to become difficult to
  work with, so some protective measures need to be taken.   *)
let lineage_set_containment superset = Addr.Set.empty
                                     
(*  *)
let ascendantly_reflective_descendents superset = 
  let zero = Int64.of_int 0 in
  let descendents_at = Addr.Table.create () in
  let entries = Superset.entries_of_isg superset in
  Hash_set.iter entries ~f:(fun e ->
      (* initialize the starting points so that we have a sound
       * invariant *)
      Addr.Table.set descendents_at e zero
    );
  let visited = Addr.Hash_set.create () in
  let addrs = Addr.Hash_set.create () in
  let set_at v =
    Addr.Table.set descendents_at v
      (Int64.of_int @@ Hash_set.length addrs) in
  (*let stack = Stack.create () in*)
  let accumulate v =
    let add v =
      Hash_set.add addrs v;
    in
    List.iter (Superset.ISG.descendants superset v) ~f:(fun d ->
        Traverse.with_descendents_at superset ~visited
          ~post:add d
      )
  in
  let push v =
    accumulate v;
    set_at v;
    (*Stack.push stack Hash_set.(copy addrs);
    Hash_set.clear addrs;*)
  in
  let pop v = ()
    (*let remove = Stack.pop v in
    Hash_set.iter remove ~f:(fun r ->
        
      )*)
  in
  Hash_set.iter entries ~f:(fun e -> 
      Traverse.with_ancestors_at superset ~visited
        ~pre:push ~post:pop e;
      Hash_set.clear addrs;
    );
  top_ancestors descendents_at superset 20

exception UnexpectedlyNotFound
(* envelopment refers to the set containment method used to capture
 * the lineage efficiently. The problem is that there are possibly
 * many connections to certain points, but which of these is deepest
 * is not necessarily known at the time of starting from a
 * termination point. For this, a regular dfs may not work, since the
 * question the needs to be resolved has to do with set inclusion of
 * descendents for many mutually exclusive sets as they merge with one
 * another. *)
let bfs_descendent_depth superset =
  let zero = Int64.of_int 0 in
  let descendents_at = Addr.Table.create () in
  let entries = Superset.entries_of_isg superset in
  Hash_set.iter entries ~f:(fun e ->
      (* initialize the starting points so that we have a sound
       * invariant *)
      Addr.Table.set descendents_at e zero
    );
  let counted = Addr.Hash_set.create () in
  let exists_missing mem check =
    (List.exists check ~f:(fun d -> not (mem d))) in
  let mem addr = Addr.Table.mem descendents_at addr in
  let rec expand cursors deferred =
    if Hash_set.length cursors <> 0 (*|| Hash_set.length deferred <> 0*) then (
      let new_cursors = Addr.Hash_set.create () in
      Hash_set.iter cursors ~f:(fun cursor ->
          let descds = (Superset.ISG.descendants superset cursor) in
          let missing = exists_missing mem descds in
          if missing then (
            (* if a cursor cannot total all descendents it is
             * deffered *)
            Hash_set.add deferred cursor;
          (*Hash_set.add new_cursors cursor;*)
          ) else if not (Hash_set.mem counted cursor) then (
            let descds =
              List.filter descds ~f:(fun v ->
                  not @@ Hash_set.mem counted v) in
            let total =
              List.fold ~init:zero descds ~f:(fun total d ->
                  Hash_set.add counted d;
                  match Addr.Table.find descendents_at d with
                  | Some (dscds_at) -> Int64.(total + dscds_at)
                  | None -> total
                ) in
            Addr.Table.update descendents_at cursor ~f:(fun preexisting ->
                match preexisting with
                | Some pre -> Int64.(pre + total + (of_int (List.length descds)))
                | None -> Int64.(total + (of_int (List.length descds)))
              );
            let ancests = (Superset.ISG.ancestors superset cursor) in
            List.iter ancests ~f:(fun above ->
                (* Context, there is a current node, it's descendents
                 * and ancestors. Reasoning: even though missing
                 * descendents is checked for, there is the
                 * possibility that a given point is the *ancestor* of
                 * numerous calls with discrete exit points. So, check
                 * to be certain that this previous point isn't
                 * already in deferred. The check for above's
                 * descendents could be repeated here, but it occurs
                 * in the next frame anyway, but it simply gets added
                 * back to deferred for each descendent until it finishes. *)
                if (not @@ Hash_set.mem counted above) then
                  Hash_set.add new_cursors above;
              );
          );
        );
      (* possibly a node has had all of it's children processed, and
       * could be resumed *)
      let deferred =
        Hash_set.filter deferred ~f:(fun defrd ->
            let descds = (Superset.ISG.descendants superset defrd) in
            let r = exists_missing mem descds in
            if not r && (not @@ mem defrd) then
              Hash_set.add new_cursors defrd;
            r
          ) in
      let new_cursors =
        Hash_set.filter new_cursors ~f:(fun v ->
            not @@ Hash_set.mem counted v) in
      (* filter all cursors that are also in deferred *)
      expand new_cursors deferred
    ) else (
      top_ancestors descendents_at superset 20
      (*let m =
        List.fold ~init:Int64.Map.empty
          ~f:(fun m (addr,c) ->
            Map.update m c
              ~f:(Option.value_map
                    ~default:[addr] ~f:(List.cons addr))
          ) @@ Addr.Table.to_alist descendents_at in
      let l = (Int64.Map.to_alist ~key_order:`Decreasing m)  in
      let len = 40 in
      let l = if List.length l > len then
                List.sub ~pos:0 ~len l
              else l in
      let l = List.filter l ~f:(fun (c,addrs) -> Int64.(c <> zero)) in
      print_endline @@
        List.to_string l ~f:(fun (c,addrs) ->
            (Int64.to_string c) ^ "-"
            ^ (List.to_string ~f:Addr.to_string addrs));
      let l = List.map l ~f:(fun (_,addr) -> addr) in
      let l = List.concat l in
      Addr.Set.of_list @@ l*)
    )
  in
  let deferred = Addr.Hash_set.create () in
  expand entries deferred

let deepest_descendent superset =
  let descendents_at = Addr.Table.create () in
  let entries = Superset.entries_of_isg superset in
  let cur_count = ref @@ Int64.of_int 0 in
  let counted = Addr.Hash_set.create () in
  let post v =(*() in*)
    cur_count := Int64.(!cur_count - of_int 1); in
  let pre v =
    cur_count := Int64.( !cur_count + of_int 1 );
    (*if not (Hash_set.mem counted v) then*)
      Addr.Table.update descendents_at ~f:(fun cur ->
          let cur = Option.value cur ~default:Int64.(of_int 0) in
          let size = Int64.of_int (Hash_set.length counted) in
        (*Int64.(cur + size)*)
        Int64.(cur + !cur_count);
        ) v;
    Hash_set.add counted v;
  in    
  Hash_set.iter entries ~f:(fun v ->
      cur_count := Int64.of_int 0;
      Hash_set.clear counted;
      Traverse.with_ancestors_at superset ~post ~pre v;
    );
  top_ancestors descendents_at superset 80

(* This presents difficulty according to the following disagreeing
 * possibilities:
 * 1) a bifuricating join point, at which the descendant must be
 * counted at each join
 * 2) a *)
let highest_ancestor superset = 
  let descendents_at = Addr.Table.create () in
  let frond = Superset.frond_of_isg superset in
  let visited = Addr.Hash_set.create () in
  let counted = Addr.Hash_set.create () in
  let zero = (Int64.of_int 0) in   
  let post v =
    let descds = (Superset.ISG.descendants superset v) in
    let total =
      List.fold ~init:zero descds
        ~f:(fun total d ->
          let total = 
            if not (Hash_set.mem counted d) then (
              match Addr.Table.find descendents_at d with
              | Some d_at ->
                 Int64.(total + d_at)
              | None ->
                 (* it is possible to have uncounted descendents
                  * because more than one path to a common point
                  * would have the dfs miss the others at first. *)
                 (*print_endline
                 @@ sprintf
                      "unexpectedly do not have entry for %s, %d
                       descds, %b visited"
                      Addr.(to_string d)
                      List.(length descds)
                      Hash_set.(mem visited d);*)
                 total
            ) else total in
          Hash_set.add counted d;
          total
        ) in
    Addr.Table.update descendents_at v ~f:(fun pre ->
        let pre = Option.value pre ~default:zero in
        Int64.(pre + total + (Int64.of_int List.(length descds)))) in
  Hash_set.iter frond ~f:(fun v ->
      Traverse.with_descendents_at ~visited superset ~post v;
    (*Hash_set.clear counted;*)
    );
  let descendents_at =
    Addr.Table.filteri descendents_at ~f:(fun ~key ~data ->
        Hash_set.mem frond key) in
  let descendents_at =
    Addr.Table.filter descendents_at ~f:(fun c -> Int64.(c > (of_int 100
      ))) in
  let remaining = 20 in
  top_ancestors descendents_at superset remaining

let false_branch_elimination superset = Addr.Set.empty
  
let _exfiltset = [
  ("FixpointGrammar",
   ((fun x -> transform (fixpoint_grammar x 0)), unfiltered));
  ("FixpointTails",
   ((fun x -> transform (fixpoint_tails x)), unfiltered));
  ("FixpointFreevarSSA",
   ((fun x -> transform (fixpoint_freevarssa x 0)), unfiltered));
  ("FixpointMemSSA", ((fun x -> transform (fixpoint_memssa x)),
                      unfiltered));
  ("DeepestDescendent", (deepest_descendent,unfiltered));
  ("HighestAncestor", (highest_ancestor,unfiltered));
  ("BFSDepth", (bfs_descendent_depth,unfiltered));
  ("AscendantlyReflectiveDescendents" ,
   (ascendantly_reflective_descendents, unfiltered));
  ("LineageSetContainment",(lineage_set_containment, unfiltered));
  ("FalseBranchElimination",(false_branch_elimination, unfiltered));
  ("LinearGrammar", (linear_grammar, unfiltered));
  ("UnfilteredGrammar", (get_branches, unfiltered));
  ("BranchViolations", (branch_violations, unfiltered));
  ("LayerViolations", (layer_violations, unfiltered));
  ("FilteredGrammar", (filtered_grammar, unfiltered));
  ("LoopGrammar", (loop_grammar, unfiltered));
  ("ClassicGrammar", (classic_grammar, unfiltered));
  ("Callsites3",
   ((fun x -> transform (get_callsites
                           ~threshold:6 x)), unfiltered));
  ("Clamped",
   ((fun s -> Addr.Set.of_list @@ find_free_insns s), unfiltered));
  ("RestrictedClamped", (restricted_clamp, unfiltered));
  ("ExtendedClamped", (extended_clamp, unfiltered));
  ("UnfilteredSCC", (extract_loops_to_set,unfiltered));
  ("LoopsWithBreak", (extract_loops_with_break,unfiltered));
  ("SCC", (extract_filter_loops,unfiltered));
  ("NoExit", (extract_exitless, unfiltered));
  ("Constant", (extract_constants_to_set,unfiltered));
  ("ImgEntry", (extract_img_entry, unfiltered));
]
let exfiltset :(setexfilt) String.Map.t
  = List.fold ~init:String.Map.empty _exfiltset
    ~f:(fun exfiltset (name, f) ->
        String.Map.set exfiltset name f
      )

let _exfiltmap = [
  ("SSA", (extract_ssa_to_map, unfiltered));
  (*("FalseBranchMap", (extract_fp_branches, unfiltered));*)
  (*("FilteredFalseBranchMap", (extract_filter_fp_branches, unfiltered));*)
    (*("FreeVarSSA", (extract_freevarssa_to_map, unfiltered));*)
  ("SSA", (extract_ssa_to_map, unfiltered));
]
let exfiltmap : ((unit, Addr.t) mapexfilt) String.Map.t
  = List.fold ~init:String.Map.empty _exfiltmap
    ~f:(fun exfiltmap (name, f) ->
        String.Map.set exfiltmap name f
      )

let featureflist =
  [("Callsites3", extract_trim_callsites);
   ("DiscardEdges", discard_edges);
   ("LoopsWithBreak", extract_trim_loops_with_break);
   ("ImgEntry",extract_trim_entry);
   ("BranchViolations", extract_trim_branch_violations);
   ("LayerViolations", extract_trim_layer_violations);
   (*("SCC", extract_tag_loops)*)
   ("NoExit", extract_trim_noexit);
   ("TrimLimitedClamped" ,extract_trim_limited_clamped);
   ("TrimFixpointGrammar", extract_trim_fixpoint_grammar);
   ("TrimFixpointSSA", extract_trim_fixpoint_ssa);
   ("TrimFixpointFreevarSSA", extract_trim_fixpoint_freevarssa);
   ("TrimFixpointTails", extract_trim_fixpoint_tails);
  ]
let featuremap = List.fold featureflist ~init:String.Map.empty
    ~f:(fun featuremap (name, f) ->
        Map.set featuremap name f
      )

let with_featureset ~f ~init featureset superset =
  let superset = List.fold ~init featureset ~f:(fun accu fname ->
      match Map.(find featuremap fname) with
      | None -> accu
      | Some (feature) ->
         (* TODO remove debug/console printing code *)
        print_endline @@ sprintf "with_featureset %s" fname;
        f fname feature accu
    ) in
  superset

(* TODO hide the type of fdists behind an api *)
let fdists = String.Map.empty
let fdists = String.Map.set fdists "FixpointGrammar" 1
let fdists = String.Map.set fdists "FixpointFreevarSSA" 1
let fdists = String.Map.set fdists "FixpointMemSSA" 1
let fdists = String.Map.set fdists "HighestAncestor" 1
let fdists = String.Map.set fdists "DeepestDescendent" 1
let fdists = String.Map.set fdists "BFSDepth" 1
let fdists = String.Map.set fdists "AscendantlyReflectiveDescendents" 1
let fdists = String.Map.set fdists "LineageSetContainment" 1
(* TODO belongs in report *)
(* TODO stop using Addr.Map *)
(* TODO calculate the optimal fdists point value for each feature *)
let make_featurepmap featureset superset = 
  List.fold ~f:(fun (feature_pmap) feature -> 
      let p = Map.find fdists feature in
      let p = Option.value p ~default:2 in
      print_endline @@ sprintf "searching for: %s" feature;
      match Map.(find exfiltset feature) with
      | None -> feature_pmap
      | Some (extract,filter) -> 
        print_endline @@ sprintf "make_featurepmap %s" feature;
        let fset = extract superset in
        Set.fold fset ~init:feature_pmap 
          ~f:(fun feature_pmap x -> 
              Map.update feature_pmap x ~f:(function 
                  | Some l ->  (p, x, feature) :: l
                  | None -> [(p, x, feature)]
                )
            )
    ) ~init:Addr.Map.empty featureset

(* TODO belongs in report *)
let with_featurepmap featureset superset ~f =
  let feature_pmap = make_featurepmap featureset superset in
  print_endline @@
    sprintf "make fpmap built size: %d" @@ Map.length feature_pmap;
  let feature_pmap = fixpoint_map superset feature_pmap in
  f feature_pmap featureset superset

(* TODO Often using callsites or other features to protect the
 * code. Could just be a lambda protect superset f *)
(* TODO tag_callsites doesn't need to exist because it is just
 * descendant traversal *)
(* TODO move all fixpoint functions to fixpoint *)
