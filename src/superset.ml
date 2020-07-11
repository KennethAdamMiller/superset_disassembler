open Bap.Std
open Regular.Std
open Core_kernel
open Or_error


(*open Format*)

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

let add_to_graph superset mem insn =
  let addr = Memory.min_addr mem in
  Superset_risg.G.add_vertex superset.insn_risg addr;
  superset

module ISG = struct
  let ancestors superset =
    Superset_risg.G.succ superset.insn_risg
  let descendants superset =
    Superset_risg.G.pred superset.insn_risg
  let iter_vertex superset f =
    Superset_risg.G.iter_vertex f superset.insn_risg
  let fold_edges superset f =
    let insn_risg = superset.insn_risg in 
    Superset_risg.G.fold_edges f insn_risg
  let link superset = 
    Superset_risg.G.add_edge superset.insn_risg
  let unlink superset = 
    Superset_risg.G.remove_edge superset.insn_risg
  let remove superset addr =
    Superset_risg.G.remove_vertex superset.insn_risg addr
  let with_graph superset ~f =
    f superset.insn_risg
  let mem_vertex superset = Superset_risg.G.mem_vertex superset.insn_risg
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
      insn_risg = Superset_risg.G.create ();
      bad         = Addr.Hash_set.create ();
      keep        = Addr.Hash_set.create ();
    }

  let lookup superset addr =
    Map.find superset.insn_map addr

  let mem superset addr =
    Superset_risg.G.mem_vertex superset.insn_risg addr

  let fold superset =
    Addr.Map.fold superset.insn_map

  let clear_bad superset addr =
    Hash_set.remove superset.bad addr

  let clear_all_bad superset =
    Hash_set.clear superset.bad

  let mark_bad superset addr =
    Hash_set.add superset.bad addr

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
    Superset_risg.G.iter_vertex (fun vert ->
        if not Map.(mem insn_map vert) then (
          mark_bad superset vert;
        )
      ) superset_risg;
    let insn_map = Map.filteri ~f:(fun ~key ~data -> 
        let vert = key in
        (*let (mem, insn) = data in
          Option.is_some insn && *)
        Superset_risg.G.(mem_vertex superset_risg vert)
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
  let count superset = Superset_risg.G.nb_vertex superset.insn_risg
  let count_unbalanced superset = Map.length superset.insn_map
  let unbalanced_diff superset =
    let kys = (Map.keys superset.insn_map) in
    let mapaddrs =
      List.fold kys ~init:Addr.Set.empty ~f:Set.add in
    let gaddrs = Superset_risg.G.fold_vertex (fun x s -> Set.add s x)
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
    let default = (Superset_risg.G.mem_vertex superset.insn_risg) in
    let mem = Option.value mem ~default in
    let conflicts = Option.value conflicts ~default:Addr.Set.empty in
    (* TODO range seq at. here *)
    let rec within_insn conflicts cur_addr len =
      if Addr.(cur_addr >= (addr ++ len)) then
        conflicts
      else
        let conflicts = if mem cur_addr then
            let conflicts = Set.add conflicts addr in
            Set.add conflicts cur_addr
          else conflicts in 
        within_insn conflicts Addr.(cur_addr ++ 1) len in
    match Core.lookup superset addr with
    | Some ((mem, _)) ->
      (* look within the body for instructions *)
      let len = (Memory.length mem) in
      within_insn conflicts Addr.(addr ++ 1) len
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

end

module Metrics = struct
  let record superset = ()
end

let is_entry superset addr =
  let insn_isg = superset.insn_risg in
  Superset_risg.G.in_degree insn_isg addr  = 0 &&
  Superset_risg.G.out_degree insn_isg addr > 0

let entries_of_isg superset =
  let insn_isg = superset.insn_risg in
  Superset_risg.G.fold_vertex (fun addr accu ->
      if is_entry superset addr then
        (Hash_set.add accu addr; accu)
      else accu)
    insn_isg (Addr.Hash_set.create ())

let mergers superset =
  let insn_risg= superset.insn_risg in
  Superset_risg.G.fold_vertex (fun addr mergers ->
      if Superset_risg.G.out_degree insn_risg addr > 1 then
        Addr.Set.add mergers addr
      else mergers) insn_risg Addr.Set.empty

let is_branch superset addr =
  Superset_risg.G.in_degree superset.insn_risg addr = 2

let get_branches superset =
  let branches = Addr.Hash_set.create () in
  ISG.iter_vertex superset (fun vert -> 
      if is_branch superset vert then
        Hash_set.add branches vert;
    );
  branches

let check_connected superset e1 e2 =
  match Superset_risg.G.find_all_edges
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
      if Superset_risg.G.mem_vertex superset.insn_risg b then
        Superset_risg.fold_component accu superset.insn_risg
          ?visited ~pre ~post b
      else accu
    )  

let fall_through_of superset addr =
  let len = Inspection.len_at superset addr in
  Addr.(addr ++ len)

let is_fall_through superset parent child = 
  let ft = fall_through_of superset parent in
  (* TODO should check for edge *)
  Addr.(child = ft)

let get_callers superset addr =
  let g = (get_graph superset) in
  if Superset_risg.G.mem_vertex g addr &&
     Superset_risg.G.out_degree g addr > 0 then
    let callers = Superset_risg.G.succ g addr in
    List.filter callers ~f:(fun caller ->
        not (is_fall_through superset caller addr))
  else []

let get_non_fall_through_edges superset = 
  let g = (get_graph superset) in
  Superset_risg.G.fold_edges
    (fun child parent jmps -> 
       if is_fall_through superset parent child then
         Map.set jmps child parent
       else jmps
    ) g Addr.Map.empty

let get_callsites ?(threshold=6) superset =
  let g = (get_graph superset) in
  let callsites = Addr.Hash_set.create () in
  Superset_risg.G.iter_vertex
    (fun v -> 
       let callers = Superset_risg.G.succ g v in
       let num_callers = 
         List.fold callers ~init:0 ~f:(fun total caller -> 
             if not (is_fall_through superset caller v) then
               total + 1
             else total) in
       if num_callers > threshold then (
         Hash_set.add callsites v;
       )
    ) g;
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

(* TODO remove terminator for Graphlib filter *)
(* TODO also, rename this to reflect it is aimed *)
(* TODO keep default values of arguments? *)
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

(* TODO replace f with pre, and ~ with ? *)
let with_descendents_at ?visited ?post ~f superset addr =
  if Core.mem superset addr then
    dfs ?visited ?post ~pre:f ISG.descendants superset addr

(* TODO replace f with pre *)
let with_ancestors_at ?visited ?post ?f superset addr =
  if Core.mem superset addr then
    dfs ?visited ?post ?pre:f ISG.ancestors superset addr


let mark_descendent_bodies_at ?visited ?datas superset addr =
  let datas = Option.value datas 
      ~default:(Addr.Hash_set.create ()) in
  let mark_bad = Core.mark_bad superset in
  with_descendents_at ?visited superset addr
    ~f:(fun v ->
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

let format_isg ?format superset =
  let format = Option.value format ~default:Format.std_formatter in
  Superset_risg.Gml.print format superset.insn_risg

let isg_to_string superset = 
  let format = Format.str_formatter in
  format_isg ~format superset;
  Format.flush_str_formatter ()

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
  let insn_risg = Superset_risg.Gml.parse (bin ^ ".graph") in
  let map_str   = In_channel.read_all (bin ^ ".map") in
  let insn_map  = insn_map_of_string map_str in
  let meta_str  = In_channel.read_all (bin ^ ".meta") in
  let arch      = meta_of_string meta_str in
  let superset  = of_components ~insn_risg ~insn_map arch in
  superset

let export bin superset = 
  let graph_f   = Out_channel.create (bin ^ ".graph") in
  let formatter = Format.formatter_of_out_channel graph_f in
  let () = Superset_risg.Gml.print formatter superset.insn_risg in
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

let with_graph superset f =
  let insn_risg = superset.insn_risg in
  f insn_risg

let subgraph superset subgraph =
  let insn_risg = superset.insn_risg in
  let g = Superset_risg.G.create () in
  Hash_set.iter subgraph ~f:(fun addr ->
      Superset_risg.G.add_vertex g addr;
      Superset_risg.G.iter_succ
        (fun s ->
           if Hash_set.mem subgraph s then
             Superset_risg.G.add_edge g addr s
        ) insn_risg addr;
      Superset_risg.G.iter_pred
        (fun s ->
           if Hash_set.mem subgraph s then
             Superset_risg.G.add_edge g s addr
        ) insn_risg addr;
    );
  g


open Graphlib.Std
module InsnGraph = Graphlib.Make(Addr)(Unit)
