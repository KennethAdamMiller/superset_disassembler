open Bap.Std
open Core_kernel
open Or_error

module Linear = Disasm_expert.Linear
exception Inconsistent_img of string

let read arch ic : (string * addr * addr) list =
  let sym_of_sexp x = [%of_sexp:string * int64 * int64] x in
  let addr_of_int64 x =
    let width = Arch.addr_size arch |> Size.in_bits in
    Addr.of_int64 ~width x in
  List.(Sexp.input_sexps ic >>| sym_of_sexp >>| (fun (s, es, ef) ->
      s, addr_of_int64 es, addr_of_int64 ef))

let read_addrs ic : addr list =
  List.t_of_sexp Addr.t_of_sexp @@ Sexp.input_sexp ic

let ground_truth_of_unstripped_bin bin : addr seq Or_error.t =
  let name = Filename.basename bin in
  let tmp = Filename.temp_dir_name ^ "/bw_" ^ name ^ ".symbol" in
  let cmd = sprintf "bap-byteweight dump -i symbols %S > %S" 
      bin tmp in
  if Sys.command cmd = 0
  then return (Seq.of_list @@ In_channel.with_file tmp ~f:read_addrs)
  else errorf
      "failed to fetch symbols from unstripped binary, command `%s'
  failed" cmd

let true_positives_of_ground_truth superset ground_truth = 
  let true_positives = Addr.Hash_set.create () in
  Set.iter ground_truth ~f:(fun addr -> 
      if Superset.ISG.mem_vertex superset addr then
        Traverse.with_descendents_at
          ~visited:true_positives
          superset addr;
    );
  true_positives
  
let true_positives superset f = 
  let function_starts = ground_truth_of_unstripped_bin f |> ok_exn
  in
  let ground_truth =
    Addr.Set.of_list @@ Seq.to_list function_starts in
  true_positives_of_ground_truth superset ground_truth
  
let reduced_occlusion superset tp =
  let fps = Addr.Hash_set.create () in
  Hash_set.iter tp ~f:(fun addr ->
      Superset.Occlusion.with_data_of_insn superset addr
        ~f:(fun x -> Hash_set.add fps x);
      Hash_set.remove fps addr;
    );
  fps  

let calc_false_positive_set superset ro = 
  let fps = Addr.Hash_set.create () in
  Hash_set.iter ro ~f:(fun v ->
      if Superset.ISG.mem_vertex superset v then
        Hash_set.add fps v
    );
  fps

let fn_insn_cnt superset tps =
  Hash_set.fold ~init:0 tps ~f:(fun count v -> 
      if Superset.ISG.mem_vertex superset v then count 
      else count+1)

let check_tp_set true_positives s =
  let n = Hash_set.length s in
  let tp_of_s = 
    Hash_set.fold ~init:0 true_positives
      ~f:(fun tp_of_s x -> 
          if Hash_set.mem s x
          then tp_of_s + 1 else tp_of_s) in
  let fp_of_s = n - tp_of_s in
  fp_of_s, tp_of_s

let check_fn_entries superset ground_truth =
  let detected_insns = 
    Superset.Core.fold superset ~init:Addr.Set.empty
      ~f:(fun ~key ~data detected_insns ->
          Set.add detected_insns key) in
  Set.diff ground_truth detected_insns

module Cache = struct
  open Bap_knowledge
  open Bap_core_theory

  let package = "superset-disasm-metrics"
  let bool_t = Knowledge.Domain.optional
                 ~inspect:sexp_of_bool ~equal:Bool.equal "bool"
  let bool_persistent =
    Knowledge.Persistent.of_binable
      (module struct type t = bool option [@@deriving bin_io] end)

  let int_t = Knowledge.Domain.optional
                ~inspect:sexp_of_int ~equal:Int.equal "int"

  let int_persistent =
    Knowledge.Persistent.of_binable
      (module struct type t = int option [@@deriving bin_io] end)

  let addrs_t =
    Knowledge.Domain.optional
      ~inspect:Addr.Set.sexp_of_t ~equal:Addr.Set.equal "addr.set"

  let addrs_persistent =
    Knowledge.Persistent.of_binable
      (module struct type t = Addr.Set.t option [@@deriving bin_io] end)
    
  let attr ty persistent name desc =
    let open Theory.Program in
    Knowledge.Class.property ~package cls name ty
      ~persistent ~public:true ~desc
    
  let string_persistent =
    Knowledge.Persistent.of_binable
      (module struct type t = string [@@deriving bin_io] end)
    
  let ground_truth_source =
    let open Knowledge.Domain in
    attr string string_persistent "ground_truth_source"
      "Binary containing debug information in the form of function
    entrances"

  let function_entrances =
    attr addrs_t addrs_persistent "function_entrances"
      "List of compiler intended function entrances"

  let ground_truth =
    attr addrs_t addrs_persistent "ground_truth"
      "Set of addresses statically reachable from function entrances"
    
  let occlusive_space =
    attr int_t int_persistent "occlusive_space"
      "Number of addresses are in the bodies (addrs) of compiler intended
       instructions"

  let reduced_occlusion =
    attr int_t int_persistent "reduced_occlusion"
      "Of the bodies (addrs) of compiler intended instructions, how many
       are occupied"

  let false_negatives =
    attr int_t int_persistent "false_negatives"
      "Number of compiler intended instructions missing"

  let false_positives =
    attr int_t int_persistent "false_positives"
      "Number of compiler intended instructions missing"

  let true_positives =
    attr int_t int_persistent "true_positives"
      "Number of retained compiler intended instructions"
    
  let clean_functions =
    attr addrs_t addrs_persistent "clean_functions"
      "Functions that were perfectly disassembled, with no false positives"

end

let compute_metrics superset =
  let open Bap_knowledge in
  let open Bap_core_theory in
  let open KB.Syntax in
  (* TODO collect the debug information for the particular superset
     name, and use it to calculate the cache digest in order to have
     separate digest for debug information and each feature *)
  (*KB.Object.create Theory.Program.cls >>= fun obj ->*)
  KB.promise Superset.Cache.superset_t (fun o ->
      KB.return @@ Some Addr.Set.empty
    );
  KB.promise Cache.function_entrances (fun label ->
      KB.collect Cache.ground_truth_source label >>= fun bin ->
  (*KB.provide Cache.function_entrances obj*)
      (* list of compiler intended entrances *)
      let function_addrs = ground_truth_of_unstripped_bin bin
                            |> ok_exn in
      let function_addrs =
        Addr.Set.of_list @@ Seq.to_list function_addrs in
      print_endline "providing function_addrs";
      KB.return (Some function_addrs)
    );
  
  KB.promise Cache.ground_truth (fun label ->
      (* List of compiler intended addresses *)
      let sg = Superset.Cache.superset_graph in
      KB.collect sg label >>= fun superset_graph ->
      KB.collect Cache.function_entrances label >>=
        fun function_addrs ->
        match function_addrs with
        | None -> KB.return None
        | Some function_addrs ->
           let visited = Addr.Hash_set.create () in
           Set.iter function_addrs ~f:(fun x ->
               Traverse.with_descendents_at ~visited superset x
             );
           let ground_truth = Hash_set.fold visited ~init:
                                Addr.Set.empty ~f:Set.add in
           KB.return (Some ground_truth)
    );

  KB.promise Cache.occlusive_space (fun label ->
      KB.collect Cache.ground_truth label >>= fun ground_truth ->
      match ground_truth with
      | None -> KB.return None
      | Some ground_truth ->
         KB.return @@
           Some (Set.fold ground_truth ~init:0
                   ~f:(fun occ addr ->
                     occ + (Superset.Inspection.len_at superset addr)
             ))
    );

  (* per feature metrics *)
  KB.promise Cache.reduced_occlusion (fun label ->
      KB.collect Cache.ground_truth label >>= fun ground_truth ->
      match ground_truth with
      | None -> KB.return None
      | Some ground_truth ->
         KB.return @@
           Some (Set.fold ground_truth ~init:0
                   ~f:(fun ro addr ->
                     let conflicts = 
                       Superset.Occlusion.conflict_seq_at
                         superset addr in
                     ro + (Seq.length conflicts)
             ))

    );

  KB.promise Cache.clean_functions (fun label ->
      KB.collect Cache.function_entrances label >>=
        fun function_entrances ->
        match function_entrances with
        | None -> KB.return None
        | Some (function_entrances) ->
           let ro_at x =
             let ro = ref false in
             let pre x =
               let c = Superset.Occlusion.conflict_seq_at superset x in
               ro := Seq.exists c ~f:(fun _ -> true); in
             Traverse.with_descendents_at superset ~pre x; !ro in
           let init = Addr.Set.empty in
           KB.return @@
             (Some
                (Set.fold function_entrances ~init ~f:(fun clean x ->
                  if ro_at x then Set.add clean x else clean
                ))
             )
    );

  KB.promise Cache.true_positives (fun label ->
      KB.collect Cache.ground_truth label >>= fun ground_truth ->
      match ground_truth with
      | None -> KB.return None
      | Some ground_truth ->
         KB.return
           (Some (Set.fold ground_truth ~init:0 ~f:(fun tp_cnt x ->
                      if Superset.Core.mem superset x then
                        tp_cnt + 1
                      else tp_cnt
              ))
           )
    );
  
  KB.promise Cache.false_negatives (fun label ->
      KB.collect Cache.ground_truth label >>= fun ground_truth ->
      match ground_truth with
      | Some ground_truth ->
         let fn_cnt =
           Set.fold ground_truth ~init:0 ~f:(fun cnt x ->
               if not (Superset.Core.mem superset x) then
                 cnt + 1
               else cnt
             ) in
         KB.return @@ Some fn_cnt
      | None -> KB.return None          
    );
  
  KB.promise Cache.false_positives (fun label ->
      KB.collect Cache.ground_truth label >>= fun ground_truth ->
      match ground_truth with
      | Some ground_truth ->
         let false_positives =
           Superset.Core.fold superset ~init:0
             ~f:(fun ~key ~data c ->
               if not Set.(mem ground_truth key) then c+1
               else c) in
         KB.return (Some false_positives)
      | None -> KB.return None
    );

(*  printf "Number of functions precisely trimmed: %d of %d\n"
    total_clean Set.(length ground_truth);
  printf "Number of possible reduced false positives: %d\n" 
    !datas;
  printf "Reduced occlusion: %d\n" (!ro);
  printf "True positives: %d\n" Hash_set.(length true_positives);
  let fn_entries = check_fn_entries superset ground_truth in
  if not (Set.length fn_entries = 0) then
    printf "Missed function entrances %s\n" 
      (List.to_string ~f:Addr.to_string @@ Set.to_list fn_entries);
  printf "Occlusion: %d\n" 
    (Set.length @@ Superset.Occlusion.find_all_conflicts superset);
  printf "Instruction fns: %d\n"
    (fn_insn_cnt superset true_positives);
  let false_negatives = Set.(length fn_entries) in
  ()*)

module Opts = struct 
  open Cmdliner

  let list_content_doc = sprintf
      "Metrics may be collected against a symbol file"
  let content_type = 
    Arg.(value &
         opt (some string) (None)
         & info ["metrics_data"] ~doc:list_content_doc)

end
