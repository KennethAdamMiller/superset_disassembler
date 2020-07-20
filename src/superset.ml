open Bap.Std
open Regular.Std
open Core_kernel
open Or_error
open Graphlib.Std   

module Dis = Disasm_expert.Basic
open Superset_impl
type elem = Superset_impl.elem
type t = Superset_impl.t

(* private accessors *)
let get_map superset = superset.insn_map
let get_graph superset = superset.insn_risg

(* private modifiers *)
let add_to_map superset mem insn = 
  let insn_map = get_map superset in
  let addr = (Memory.min_addr mem) in
  let insn_map = Addr.Map.set insn_map addr (mem, insn) in
  { superset with insn_map }

module OG = Graphlib.To_ocamlgraph(G)
  
let add_to_graph superset mem insn =
  let addr = Memory.min_addr mem in
  let insn_risg = OG.add_vertex superset.insn_risg addr in
  { superset with insn_risg }

module ISG = struct
  let ancestors superset =
    OG.succ superset.insn_risg

  let descendants superset =
    OG.pred superset.insn_risg

  let iter_vertex superset f =
    OG.iter_vertex f superset.insn_risg

  let fold_edges superset f =
    let insn_risg = superset.insn_risg in 
    OG.fold_edges f insn_risg

  let link superset v1 v2 = 
    let insn_risg = OG.add_edge superset.insn_risg v1 v2 in
    { superset with insn_risg }

  let unlink superset v1 v2 = 
    let insn_risg = OG.remove_edge superset.insn_risg v1 v2 in
    { superset with insn_risg }

  let remove superset addr =
    let insn_risg = OG.remove_vertex superset.insn_risg addr in
    { superset with insn_risg } 

  let mem_vertex superset = OG.mem_vertex superset.insn_risg

  let raw_loops superset = 
    StrongComponents.scc_list superset.insn_risg

  let dfs_fold ?visited superset =
    fold_component ?visited superset.insn_risg

  let print_dot ?colorings superset =
    (*if not (colorings = String.Map.empty) then*)
    let colorings = Option.value colorings ~default:String.Map.empty in
    let fout =
      Out_channel.create @@ Option.value_exn superset.filename
                            ^ ".dot" in
    let superset_isg = Oper.mirror superset.insn_risg in
    let insn_map = superset.insn_map in
    let module Layout =
      Make(struct
          let instance = (superset_isg, colorings, insn_map)
        end) in
    Layout.Dot.output_graph fout (superset_isg, colorings, insn_map)

  let format_isg ?format superset =
    let format = Option.value format ~default:Format.std_formatter in
    Gml.print format superset.insn_risg

  let isg_to_string superset = 
    let format = Format.str_formatter in
    format_isg ~format superset;
    Format.flush_str_formatter ()

  let filter superset subgraph =
    let insn_risg = superset.insn_risg in
    let g = Graphlib.create (module G) () in
    let g =
      Hash_set.fold subgraph ~init:g ~f:(fun g addr ->
          let g = OG.add_vertex g addr in
          let g = OG.fold_succ
                    (fun s g ->
                      if Hash_set.mem subgraph s then
                        OG.add_edge g addr s
                      else g
                    ) insn_risg addr g in
          let g = OG.fold_pred
                    (fun s g ->
                      if Hash_set.mem subgraph s then
                        OG.add_edge g s addr
                      else g
                    ) insn_risg addr g in g
        ) in 
    { superset with insn_risg =g; }

end

module Core = struct
  let add superset mem insn =
    let superset = add_to_graph superset mem insn in
    let superset = add_to_map superset mem insn in
    superset

  let empty arch =
    let brancher = Brancher.of_bil arch in
    let module Target = (val target_of_arch arch) in
    let lifter = Target.lift in
    {
      arch;
      filename = None;
      main_entry = None;
      sections = Memmap.empty;
      brancher;
      endianness= None;
      lifter;
      balanced = true;
      insn_map = Addr.Map.empty;
      insn_risg = Graphlib.create (module G) ();
      bad         = Addr.Hash_set.create ();
      keep        = Addr.Hash_set.create ();
    }

  let lookup superset addr =
    Map.find superset.insn_map addr

  let mem superset addr =
    OG.mem_vertex superset.insn_risg addr

  let fold superset =
    Addr.Map.fold superset.insn_map

  let clear_bad superset addr =
    Hash_set.remove superset.bad addr

  let clear_all_bad superset =
    Hash_set.clear superset.bad

  let mark_bad superset addr =
    Hash_set.add superset.bad addr

  let copy_bad superset =
    Hash_set.copy superset.bad
    
  let next_chunk mem ~addr =
    let next_addr = Addr.succ addr in
    Memory.view ~from:next_addr mem

  let run_seq dis mem = 
    let open Seq.Generator in 
    let rec disasm cur_mem = 
      let elem = match Dis.insn_of_mem dis cur_mem with
        | Ok (m, insn, _) -> (m, insn)
        | Error _ -> (cur_mem, None) in
      yield elem >>= fun () ->
      match next_chunk mem ~addr:(Memory.min_addr cur_mem) with
      | Ok next -> disasm next
      | Error _ -> return () in
    run (disasm mem)

  let run dis ~accu ~f mem =
    Seq.fold ~init:accu ~f:(fun x y -> f y x) (run_seq dis mem)

  let disasm ?(backend="llvm") ~accu ~f arch mem =
    Dis.with_disasm ~backend (Arch.to_string arch)
      ~f:(fun d -> Ok(run d ~accu ~f mem))

  let lift_insn superset (mem,insn) =
    let insn = Option.map insn ~f:(superset.lifter mem) in
    Option.map insn ~f:(fun bil -> (mem, bil |> ok_exn))

  let lift superset insns =
    let lifted_superset = Addr.Map.empty in
    List.fold insns ~init:lifted_superset
      ~f:(fun lifted_superset (mem, insn) -> 
          match lift_insn superset (mem, insn) with
          | Some (mem, bil) -> 
            let addr = Memory.min_addr mem in 
            Map.set lifted_superset ~key:addr
              ~data:(bil, Memory.length mem)
          | None -> lifted_superset
        )

  let update_with_mem ?backend ?f superset mem =
    let update = Option.value f ~default:(fun (m, i) a -> a) in
    let f (mem, insn) superset =
      let superset = add superset mem insn in
      update (mem, insn) superset in
    disasm ?backend ~accu:superset ~f superset.arch mem |> ok_exn


  (* TODO this may not produce the one to one needed *)
  let rebalance superset =
    let insn_map = get_map superset in
    let superset_risg = get_graph superset in
    OG.iter_vertex (fun vert ->
        if not Map.(mem insn_map vert) then (
          mark_bad superset vert;
        )
      ) superset_risg;
    let insn_map = Map.filteri ~f:(fun ~key ~data -> 
        let vert = key in
        (*let (mem, insn) = data in
          Option.is_some insn && *)
        OG.(mem_vertex superset_risg vert)
      ) insn_map in
    { superset with insn_risg =superset_risg; insn_map; }

end

module Inspection = struct
  let contains_addr superset addr =
    Memmap.contains superset.sections addr
  (*let img = Option.value_exn superset.img in
    let segments = Table.to_sequence Image.(segments img) in
    Seq.fold segments ~init:false ~f:(fun status (mem, segment) ->
      status || Memory.contains mem addr)*)
  let total_bytes superset =
    Seq.fold (Memmap.to_sequence superset.sections) ~init:0
      ~f:(fun total (mem,_) -> (total + (Memory.length mem)))
  let count superset = OG.nb_vertex superset.insn_risg
  let count_unbalanced superset = Map.length superset.insn_map
  let unbalanced_diff superset =
    let kys = (Map.keys superset.insn_map) in
    let mapaddrs =
      List.fold kys ~init:Addr.Set.empty ~f:Set.add in
    let gaddrs = OG.fold_vertex (fun x s -> Set.add s x)
        superset.insn_risg Addr.Set.empty in
    Set.diff mapaddrs gaddrs, Set.diff gaddrs mapaddrs
  let get_memmap superset = superset.sections
  let get_main_entry superset = superset.main_entry
  let filename superset = superset.filename
  let static_successors superset mem insn =
    let brancher = superset.brancher in
    match insn with 
    | None -> [None, `Fall]
    | Some insn -> 
      try 
        Brancher.resolve brancher mem insn
      with _ -> (
          print_endline @@ 
          "Target resolve failed on memory at " ^ Memory.to_string mem; 
          [None, `Fall]
        )
  let len_at superset at = 
    let insn_map = get_map superset in
    match Map.find insn_map at with
    | None -> 0
    | Some(mem, _) -> Memory.length mem
  let get_base superset =
    let insn_map = get_map superset in
    let (base_addr, _)  = Addr.Map.min_elt insn_map |> Option.value_exn in
    base_addr
  let num_bad superset =
    Hash_set.length superset.bad

  let is_bad_at superset at = Hash_set.mem superset.bad at

  let get_segments superset =
    superset.sections
  (*Image.segments Option.(value_exn superset.img)*)

  let get_endianness superset = superset.endianness

  let get_arch superset = superset.arch

end

module Occlusion = struct
  let seq_of_addr_range addr len = 
    let open Seq.Generator in
    let rec gen_next_addr cur_addr = 
      if Addr.(cur_addr >= (addr ++ len)) then
        return ()
      else
        yield cur_addr >>=  fun () -> 
        let next_addr = Addr.succ cur_addr in
        gen_next_addr next_addr
    in run (gen_next_addr Addr.(succ addr))

  let range_seq superset =
    let insn_map = superset.insn_map in
    let map_seq = Addr.Map.to_sequence insn_map in
    Seq.bind map_seq (fun (addr, (mem, _)) -> 
        seq_of_addr_range addr (Memory.length mem)
      )

  let range_seq_of_conflicts ~mem addr len = 
    let range_seq = seq_of_addr_range addr len in
    Seq.filter range_seq ~f:mem

  let seq_of_all_conflicts superset =
    let insn_map = superset.insn_map in
    let insn_map_seq = Addr.Map.to_sequence insn_map in
    let check_mem = Addr.Map.(mem insn_map) in
    Seq.bind insn_map_seq (fun (addr, (mem, _)) -> 
        range_seq_of_conflicts ~mem:check_mem addr (Memory.length mem)
      )

  let conflict_seq_at superset addr =
    let insn_map = superset.insn_map in
    let check_mem = Addr.Map.(mem insn_map) in
    match Map.find insn_map addr with
    | Some(mem, _) -> 
      let len = Memory.length mem  in
      range_seq_of_conflicts ~mem:check_mem addr len
    | None -> Seq.empty

  let with_data_of_insn superset at ~f =
    let len = Inspection.len_at superset at in
    let body = seq_of_addr_range at len in
    Seq.iter body ~f

  let conflicts_within_insn_at superset ?mem ?conflicts addr =
    let default = (OG.mem_vertex superset.insn_risg) in
    let mem = Option.value mem ~default in
    let conflicts = Option.value conflicts ~default:Addr.Set.empty in
    match Core.lookup superset addr with
    | Some ((m, _)) ->
       let len = (Memory.length m) in
       let rng = range_seq_of_conflicts ~mem addr len in
       let conflicts =
         if not (Seq.is_empty rng) then
           Set.add conflicts addr
         else conflicts in
       Seq.fold rng ~init:conflicts ~f:Set.add
    | None -> conflicts

  let find_all_conflicts ?mem superset =
    let insn_map = superset.insn_map in
    List.fold Map.(keys insn_map) ~init:Addr.Set.empty
      ~f:(fun conflicts addr -> 
          conflicts_within_insn_at superset ?mem ~conflicts addr
        )

  let conflicts_within_insns superset keep =
    Set.fold keep ~init:Addr.Set.empty
      ~f:(fun conflicts addr -> 
          conflicts_within_insn_at superset
            ~conflicts addr
        )

  (* It is possible that scenarios admit an instruction that is not
     the parent of a shared child that contains this addr *)
  let parent_conflict_at superset addr =
    let children = ISG.descendants superset addr in
    List.fold children ~init:Addr.Set.empty ~f:(fun cparents child -> 
        let parents = ISG.ancestors superset child in
        List.fold parents ~init:cparents ~f:(fun cparents parent -> 
            if not Addr.(parent = addr) then
              match Core.lookup superset parent with
              | Some(mem, _) -> 
                 let len = Memory.length mem in
                 if Addr.(parent < addr) && Addr.(addr < (parent ++ len)) then
                   Set.add cparents parent
                 else cparents
              | None -> cparents
            else cparents
          )
      )
    
end

module Metrics = struct
  let record superset = ()
end

let is_entry superset addr =
  let insn_isg = superset.insn_risg in
  OG.in_degree insn_isg addr  = 0 &&
  OG.out_degree insn_isg addr > 0

let entries_of_isg superset =
  let insn_isg = superset.insn_risg in
  OG.fold_vertex (fun addr accu ->
      if is_entry superset addr then
        (Hash_set.add accu addr; accu)
      else accu)
    insn_isg (Addr.Hash_set.create ())

let mergers superset =
  let insn_risg= superset.insn_risg in
  OG.fold_vertex (fun addr mergers ->
      if OG.out_degree insn_risg addr > 1 then
        Addr.Set.add mergers addr
      else mergers) insn_risg Addr.Set.empty

let is_branch superset addr =
  OG.in_degree superset.insn_risg addr = 2

let get_branches superset =
  let branches = Addr.Hash_set.create () in
  ISG.iter_vertex superset (fun vert -> 
      if is_branch superset vert then
        Hash_set.add branches vert;
    );
  branches

let check_connected superset e1 e2 =
  match OG.find_all_edges
          superset.insn_risg e1 e2 with
  | [] -> false | _ -> true

(* This is a traversal
   val with_bad :
   t ->
   ?visited:'b Addr.Hash_set.t_ ->
   pre:('c -> addr -> 'd) ->
   post:('d -> addr -> 'c) -> 'c -> 'c
*)
let with_bad superset ?visited ~pre ~post accu =
  Hash_set.fold ~init:accu superset.bad ~f:(fun accu b -> 
      if OG.mem_vertex superset.insn_risg b then
        ISG.dfs_fold superset ?visited
          ~pre ~post accu b
      else accu
    )  

let fall_through_of superset addr =
  let len = Inspection.len_at superset addr in
  Addr.(addr ++ len)

let is_fall_through superset parent child = 
  let ft = fall_through_of superset parent in
  Addr.(child = ft)

let get_callers superset addr =
  let g = (get_graph superset) in
  if OG.mem_vertex g addr &&
     OG.out_degree g addr > 0 then
    let callers = OG.succ g addr in
    List.filter callers ~f:(fun caller ->
        not (is_fall_through superset caller addr))
  else []

let get_non_fall_through_edges superset = 
  let g = (get_graph superset) in
  OG.fold_edges
    (fun child parent jmps -> 
       if is_fall_through superset parent child then
         Map.set jmps child parent
       else jmps
    ) g Addr.Map.empty

let get_callsites ?(threshold=6) superset =
  let callsites = Addr.Hash_set.create () in
  ISG.iter_vertex superset
    (fun v -> 
       let callers = ISG.ancestors superset v in
       let num_callers = 
         List.fold callers ~init:0 ~f:(fun total caller -> 
             if not (is_fall_through superset caller v) then
               total + 1
             else total) in
       if num_callers > threshold then (
         Hash_set.add callsites v;
       )
    ) ;
  callsites

let with_depth ?pre ?post f =
  let depth = ref 0 in
  let pre x =
    depth := !depth + 1;
    Option.value_map pre
      ~f:(fun pre -> pre !depth x) ~default:()
  in
  let post x =
    depth := !depth - 1;
    Option.value_map post
      ~f:(fun post -> post !depth x) ~default:()
  in
  f ~pre ~post 

(* TODO Does this calculate the depth correctly? would BFS be more *)
(* correct, since there can be more than one descendant of an edge. *)
let get_depth f =
  let deepest = ref 0 in
  with_depth ~post:(fun depth x ->
      deepest := max !deepest depth;
    ) ~pre:(fun _ _ -> ()) f;
  !deepest

let dfs ?(terminator=(fun _ -> true))
    ?visited ?(pre=fun _ -> ()) ?(post=fun _ -> ()) explore g v =
  let visited = Option.value visited 
      ~default:(Addr.Hash_set.create ()) in
  let rec visit v =
    Hash_set.add visited v;
    pre v;
    List.iter (explore g v)
      ~f:(fun w ->
          if (not (Hash_set.mem visited w)) && (terminator w) then
            visit w) ;
    post v
  in visit v

let iter_component ?(terminator=(fun _ -> true))
    ?visited ?(pre=fun _ -> ()) ?(post=fun _ -> ())  =
  dfs ~terminator ?visited ~pre ~post ISG.ancestors

let with_descendents_at ?visited ?post ?pre superset addr =
  if Core.mem superset addr then
    dfs ?visited ?post ?pre ISG.descendants superset addr

let with_ancestors_at ?visited ?post ?pre superset addr =
  if Core.mem superset addr then
    dfs ?visited ?post ?pre ISG.ancestors superset addr


let mark_descendent_bodies_at ?visited ?datas superset addr =
  let datas = Option.value datas 
      ~default:(Addr.Hash_set.create ()) in
  let mark_bad = Core.mark_bad superset in
  with_descendents_at ?visited superset addr
    ~pre:(fun v ->
        Occlusion.with_data_of_insn superset v ~f:mark_bad;
        Occlusion.with_data_of_insn superset v ~f:(Hash_set.add datas);
      )

let rebuild ?insn_map ?insn_risg superset =
  let insn_map = Option.value insn_map ~default:superset.insn_map in
  let insn_risg = Option.value insn_risg ~default:superset.insn_risg in
  { superset with 
    insn_risg = insn_risg;
    insn_map  = insn_map;
  }

let sexp_of_mem mem = 
  let endianness = Memory.endian mem in
  let maddr = Memory.min_addr mem in
  let bstr_mem = Memory.to_string mem in
  Tuple3.sexp_of_t 
    Addr.sexp_of_endian
    Addr.sexp_of_t
    String.sexp_of_t (endianness, maddr, bstr_mem)

let mem_of_sexp sexp_mem =
  let (endianness, maddr, mem) = 
    Tuple3.t_of_sexp
      Addr.endian_of_sexp
      Addr.t_of_sexp
      String.t_of_sexp sexp_mem in
  let mem = Bigstring.of_string mem in
  Memory.create endianness maddr mem |> ok_exn

let insn_map_to_string insn_map =
  Sexp.to_string @@ Addr.Map.sexp_of_t 
    (fun (mem, _) -> sexp_of_mem mem) insn_map

let insn_map_of_string map_str = 
  let map_sexp = Sexp.of_string map_str in
  Addr.Map.t_of_sexp (fun m -> mem_of_sexp m, None) map_sexp

let meta_of_string meta_str = 
  let sexp_meta = Sexp.of_string meta_str in
  Arch.t_of_sexp sexp_meta

let meta_to_string superset = 
  Sexp.to_string (Arch.sexp_of_t superset.arch)

let import bin =
  let insn_risg = Gml.parse (bin ^ ".graph") in
  let map_str   = In_channel.read_all (bin ^ ".map") in
  let insn_map  = insn_map_of_string map_str in
  let meta_str  = In_channel.read_all (bin ^ ".meta") in
  let arch      = meta_of_string meta_str in
  let superset  = of_components ~insn_risg ~insn_map arch in
  superset

let export bin superset = 
  let graph_f   = Out_channel.create (bin ^ ".graph") in
  let formatter = Format.formatter_of_out_channel graph_f in
  let () = Gml.print formatter superset.insn_risg in
  let () = Out_channel.close graph_f in
  let insn_map = get_map superset in
  let map_str  = insn_map_to_string insn_map in
  Out_channel.write_all (bin ^ ".map") ~data:map_str;
  let meta_str  = meta_to_string superset in
  Out_channel.write_all (bin ^ ".meta") ~data:meta_str

let export_addrs bin superset =
  let insn_map = get_map superset in
  let addrs = Map.keys insn_map in
  let addrs = List.map addrs ~f:Addr.to_string in
  let base = Filename.basename bin in
  let addrs_file = Out_channel.create ("./" ^ base ^ "_addrs.txt") in
  Out_channel.output_lines addrs_file addrs

let with_img ~accu img ~f =
  let segments = Table.to_sequence @@ Image.segments img in
  Seq.fold segments ~init:accu ~f:(fun accu (mem, segment) ->
      if Image.Segment.is_executable segment then
        f ~accu mem
      else accu 
    )

let superset_of_img ?f ~backend img =
  let arch = Image.arch img in
  let segments =   Image.memory img in
  let main_entry = Image.entry_point img in
  let filename = Image.filename img in
  let superset =
    of_components ~main_entry ?filename ~segments arch in
  with_img ~accu:superset img
    ~f:(fun ~accu mem -> 
        Core.update_with_mem ~backend accu mem ?f
      )

let superset_disasm_of_file ?(backend="llvm") ?f binary =
  let img  = Common.img_of_filename binary in
  superset_of_img ~backend img ?f
