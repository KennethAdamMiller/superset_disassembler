open Core_kernel.Std
open Bap.Std
open Superset

let bad_of_arch arch = 
  Addr.of_int ~width:(Size.in_bits @@ Arch.addr_size arch) 0

let bad_of_addr addr =
  Addr.of_int ~width:(Addr.bitwidth addr) 0


let static_successors brancher mem insn =
  try 
    Brancher.resolve brancher mem insn
  with _ -> (
      print_endline @@ 
      "Target resolve failed on memory at " ^ Memory.to_string mem; 
      [None, `Fall]
    )

let addr_in_mem superset addr =
  let segments = Table.to_sequence @@ Image.segments superset.img in
  Seq.fold segments ~init:false ~f:(fun status (mem, segment) ->
      status || Memory.contains mem addr)

let find_non_mem_accesses superset = 
  let check_return_addr r addr = 
    match addr with
    | Bil.Int(addr) -> 
      if addr_in_mem superset addr then
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

let accesses_non_mem superset mem insn = 
  let arch = Image.arch superset.img in
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

let tag_with superset ~f = 
  Addr.Map.fold superset.insn_map ~init:superset ~f:(fun ~key ~data superset -> 
      let (mem, insn) = data in
      let targets = static_successors superset.brancher mem insn in
      f superset mem insn targets
    )

let tag_invalid_targets superset mem insn targets = 
  let arch = Image.arch superset.img in
  let bad  = bad_of_arch arch in
  let src  = Memory.min_addr mem in
  List.iter targets
    ~f:(fun (target,_) ->
        match target with 
        | Some(target) -> 
          if not (addr_in_mem superset target) then
            Superset_rcfg.G.add_edge superset.insn_rcfg bad src
          else if Addr.(target = bad) then
            Superset_rcfg.G.add_edge superset.insn_rcfg bad src
        | None -> ()
      );
  superset

let tag_non_insn superset mem insn targets = 
  let arch = Image.arch superset.img in
  let bad  = bad_of_arch arch in
  let src  = Memory.min_addr mem in
  if accesses_non_mem superset mem (Some(insn)) then (
    (* The instruction reads or writes to memory that is not mapped *)
    Superset_rcfg.G.add_edge superset.insn_rcfg bad src
  ) else if Superset_rcfg.G.mem_vertex superset.insn_rcfg src
         && not (Addr.Map.mem superset.insn_map src) then (
    (* Wasn't a parseable instruction *)
    Superset_rcfg.G.add_edge superset.insn_rcfg bad src
  );
  superset

let tag_success superset mem insn targets =
  let arch = Image.arch superset.img in
  let bad  = bad_of_arch arch in
  let src = Memory.min_addr mem in
  List.iter targets ~f:(fun (target,_) -> 
      match target with
      | Some (target) -> 
        if not (Superset_rcfg.G.mem_edge superset.insn_rcfg bad src) then
          Superset_rcfg.G.add_edge superset.insn_rcfg target src
      | None -> ());
  superset

let default_tags = [tag_non_insn;
                    tag_invalid_targets;
                    tag_success]

let tag ?funcs superset =
  let funcs = Option.value funcs ~default:default_tags in
  let f superset mem insn targets =
    List.fold_left funcs ~init:superset ~f:(fun superset f -> 
        (f superset mem insn targets)) in
  tag_with superset ~f

let trim superset =
  print_endline "Trim.trim";
  let arch = Image.arch superset.img in
  let insn_map = superset.insn_map in
  let superset_rcfg = superset.insn_rcfg in
  let bad = bad_of_arch arch in
  let module G = Superset_rcfg.G in
  (* TODO Probably would do better with to_drop being a list *)
  let to_drop = Addr.Hash_set.create () in
  if G.mem_vertex superset_rcfg bad then (
    Superset_rcfg.Dfs.prefix_component (Hash_set.add to_drop) superset_rcfg bad;
    let insn_map = Hash_set.fold to_drop ~init:insn_map 
        ~f:(fun insn_map v -> 
            G.remove_vertex superset_rcfg v;
            Addr.Map.remove insn_map v
          ) in
    G.remove_vertex superset_rcfg bad;
    printf "%d vertices after trimming, removing %d\n" 
      (G.nb_vertex superset_rcfg)
      (Hash_set.length to_drop);
    let insn_map = Addr.Map.remove insn_map bad in
    {
      insn_rcfg = superset_rcfg;
      img = superset.img;
      insn_map = insn_map;
      brancher = superset.brancher;
    }
  ) else
    superset

let tagged_disasm_of_file ?funcs ~backend file =
  let superset = Superset.superset_disasm_of_file ~backend file in
  let superset = tag ?funcs superset in
  superset


let trimmed_disasm_of_file ~backend file =
  trim (tagged_disasm_of_file ~backend file)

