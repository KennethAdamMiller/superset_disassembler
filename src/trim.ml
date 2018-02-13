open Core_kernel.Std
open Bap.Std

let static_successors brancher mem insn =
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

let find_non_mem_accesses superset = 
  let check_return_addr r addr = 
    match addr with
    | Bil.Int(addr) -> 
      if Superset.contains_addr superset addr then
        r
      else r.return(Some(false))
    | _ -> r in
  (object(self) 
    inherit [bool] Stmt.finder
    method! enter_load ~mem ~addr _ _ r = 
      check_return_addr r addr
    method! enter_store ~mem ~addr ~exp _ _ r =
      check_return_addr r addr
  end)

let accesses_non_mem superset mem insn _ = 
  let arch = Superset.get_arch superset in
  let module Target = (val target_of_arch arch) in
  let lifter = Target.lift in
  try
    let bil = Superset.lift_insn lifter (mem, insn) in
    let _, bil = Option.value ~default:(mem,[]) bil in
    let status = List.fold bil ~init:None ~f:(fun status _stmt -> 
        Option.value_map status ~default:(Some(false)) ~f:(fun status ->
            if not status then
              Stmt.find (find_non_mem_accesses superset) _stmt
            else Some(status)
          )
      ) in
    Option.value status ~default:false
  with _ -> false 

(* TODO Does this belong in Superset? *)
let tag_with ~f (mem, insn) superset = 
  let open Superset in
  let targets = static_successors superset.brancher mem insn in
  f superset mem insn targets

let tag_target_not_in_mem superset mem insn targets =
  List.iter targets
    ~f:(fun (target,_) ->
        match target with 
        | Some(target) -> 
          if not (Superset.contains_addr superset target) then
            Superset.mark_bad superset target
        | None -> ()
      );
  superset

let tag_target_is_bad superset mem insn targets =
  let bad = Superset.get_bad superset in
  List.iter targets
    ~f:(fun (target,_) ->
        match target with 
        | Some(target) -> 
          if Addr.(target = bad) then
            Superset.mark_bad superset target
        | None -> ()
      );
  superset

(* TODO need to add a unit test *)
let tag_target_in_body superset mem insn targets =
  let src = Memory.min_addr mem in
  List.iter targets
    ~f:(fun (target,_) ->
        match target with 
        | Some(target) -> 
          if (Memory.contains mem target) && 
             not Addr.(src = target) then
            Superset.mark_bad superset src
        | None -> ()
      );
  superset

let tag_invalid_targets superset mem insn targets = 
  let superset = tag_target_not_in_mem superset mem insn targets in
  let superset = tag_target_is_bad superset mem insn targets in
  let superset = tag_target_in_body superset mem insn targets in
  superset

let tag_non_mem_access superset mem insn targets = 
  let src  = Memory.min_addr mem in
  if accesses_non_mem superset mem insn targets then (
    (* The instruction reads or writes to memory that is not mapped *)
    Superset.mark_bad superset src
  );
  superset

let tag_non_insn superset mem insn targets = 
  let src  = Memory.min_addr mem in
  if Option.is_none insn then (
    (* Wasn't a parseable instruction *)
    Superset.mark_bad superset src
  );
  superset


(* TODO This belongs in Superset *)
let tag_success superset mem insn targets =
  let src = Memory.min_addr mem in
  let insn_risg = Superset.get_graph superset in
  Superset_risg.G.add_vertex insn_risg src;
  List.iter targets ~f:(fun (target,_) -> 
      match target with
      | Some (target) -> 
        Superset_risg.G.add_edge insn_risg target src
      | None -> ());
  superset

(* pretty sure this belongs in superset *)
let add_to_map superset mem insn _ = 
  let insn_map = Superset.get_map superset in
  let insn_map = Superset.add_to_map insn_map (mem, insn) in
  Superset.rebuild ~insn_map superset

let default_tags = [tag_non_insn;
                    tag_non_mem_access;
                    tag_target_not_in_mem;
                    tag_target_is_bad;
                    tag_target_in_body;
                    tag_success]

let tag ?invariants =
  let invariants = Option.value invariants ~default:default_tags in
  let f superset mem insn targets =
    List.fold_left invariants ~init:superset ~f:(fun superset f -> 
        (f superset mem insn targets)) in
  tag_with ~f

let trim superset =
  let superset_risg = Superset.get_graph superset in
  let bad  = Superset.get_bad superset in
  let module G = Superset_risg.G in
  let insn_map = Superset.get_map superset in
  Superset_risg.G.iter_vertex (fun vert ->
      if not Map.(mem insn_map vert) then (
        Superset.mark_bad superset vert;
      )
    ) superset_risg;
  if G.mem_vertex superset_risg bad then (
    let f addr = 
      G.remove_vertex superset_risg addr in
    let orig_size = (G.nb_vertex superset_risg) in
    Superset_risg.Dfs.postfix_component f superset_risg bad;
    G.remove_vertex superset_risg bad;
    let trimmed_size = (G.nb_vertex superset_risg) in
    let num_removed = orig_size - trimmed_size in
    printf "%d vertices after trimming, removing %d\n" 
      trimmed_size num_removed;
    let insn_map = Map.filteri ~f:(fun ~key ~data -> 
        let vert = key in
        (*let (mem, insn) = data in
          Option.is_some insn && *)
        Superset_risg.G.(mem_vertex superset_risg vert)
      ) (Superset.get_map superset) in
    Superset.rebuild ~insn_risg:superset_risg ~insn_map superset
  ) else
    superset

let tag_superset ?invariants superset = 
  let invariants = Option.value invariants ~default:default_tags in
  let insn_map = Superset.get_map superset in
  Addr.Map.fold ~init:superset insn_map ~f:(fun ~key ~data superset -> 
      let mem, insn = data in
      List.fold ~init:superset invariants ~f:(fun superset f -> 
          tag_with ~f (mem, insn) superset
        )
    )

let tagged_disasm_of_file ~data ?f ?invariants ~backend file =
  let invariants = Option.value invariants ~default:default_tags in
  let f = Option.value f ~default:[] in
  let invariants = Some(List.append f invariants) in
  Superset.superset_disasm_of_file ~data ~backend file ~f:(tag ?invariants)

let trimmed_disasm_of_file ~data ?f ~backend file =
  trim (tagged_disasm_of_file ~data ?f ~backend file)
