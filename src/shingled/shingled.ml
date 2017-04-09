open Core_kernel.Std
open Regular.Std
open Bap.Std
open Or_error.Monad_infix
open Insn_cfg

type t = Insn_cfg.t
(* TODO need to use a table for is_exec_ok for some binaries *)
let is_exec_ok gmem_min gmem_max addr =
  Addr.(addr >= gmem_min && addr <= gmem_max)

let static_successors brancher ~min ~max mem insn =
  if is_exec_ok min max (Memory.min_addr mem)
  then 
    try 
      Brancher.resolve brancher mem insn
    with _ -> (
        print_endline @@ 
        "Target resolve failed on memory at " ^ Memory.to_string mem; 
        [None, `Fall]
      )
  else [None, `Fall]

(* TODO need to test cfg_of_superset in unit tests further *)
let rcfg_of_superset ?superset_cfg ?brancher superset gmem arch =
  let brancher = Option.value brancher ~default:(Brancher.of_bil arch) in
  let superset_cfg = Option.value superset_cfg ~default:(G.create ()) in
  let min,max = Memory.(min_addr gmem, max_addr gmem) in
  let target_in_mem = is_exec_ok min max in
  let bad = Insn_cfg.bad_of_arch arch in
  (* TODO touches non-mem *)
  let module Target = (val target_of_arch arch) in
  let lifter = Target.lift in
  let accesses_non_mem mem insn = 
    try
      let bil = Superset.lift_insn lifter (mem, insn) in
      let _, bil = Option.value ~default:(mem,[]) bil in
      let check_return_addr r addr = 
        match addr with
        | Bil.Int(addr) -> 
          if target_in_mem addr then
            r
          else r.return(Some(false))
        | _ -> r in
      let non_mem_accesses = 
        (object(self) 
          inherit [bool] Stmt.finder
          method! enter_load ~mem ~addr _ _ r = 
            check_return_addr r addr
          method! enter_store ~mem ~addr ~exp _ _ r =
            check_return_addr r addr
        end) in
      let status = List.fold bil ~init:None ~f:(fun status _stmt -> 
          Option.value_map status ~default:(Some(false)) ~f:(fun status ->
              if not status then
                Stmt.find non_mem_accesses _stmt
              else Some(status)
            )
        ) in
      Option.value status ~default:false
    with _ -> false in
  let get_targets = static_successors brancher ~min ~max in
  let is_non_code addr = not (target_in_mem addr) in
  List.iter superset ~f:(fun (mem, insn) ->
      let src = Memory.min_addr mem in
      match insn with
      | Some(insn) ->
        let targets = get_targets mem insn in
        List.iter targets
          ~f:(fun (target,_) ->
              match target with 
              | Some(target) -> 
                if is_non_code target then
                  G.add_edge superset_cfg bad src
                else if accesses_non_mem mem (Some(insn)) then
                  G.add_edge superset_cfg bad src
                else if Addr.(target = bad) then
                  G.add_edge superset_cfg bad src
                else 
                  G.add_edge superset_cfg target src
              | None -> ()
            )
      | None -> 
        G.add_edge superset_cfg bad src
    );
  superset_cfg

  print_endline "Shingled.sheer";
let trim insn_map superset_cfg arch =
  let bad = bad_of_arch arch in
  (* TODO Probably would do better with to_drop being a list *)
  let to_drop = Addr.Hash_set.create () in
  if Insn_cfg.G.mem_vertex superset_cfg bad then (
    Dfs.prefix_component (Hash_set.add to_drop) superset_cfg bad;
    let insn_map = Hash_set.fold to_drop ~init:insn_map 
        ~f:(fun insn_map v -> 
            G.remove_vertex superset_cfg v;
            Addr.Map.remove insn_map v
          ) in
    G.remove_vertex superset_cfg bad;
    Addr.Map.remove insn_map bad, superset_cfg
  ) else
    insn_map, superset_cfg

  print_endline "shingled_to_map";
let superset_to_map superset insn_map insn_cfg = 
  print_endline "superset_to_map";
  List.fold_left ~init:insn_map superset
    ~f:(fun insn_map (mem, insn) ->
        let addr = (Memory.min_addr mem) in
        if Insn_cfg.G.mem_vertex insn_cfg addr then
          Addr.Map.add insn_map addr (mem, insn)
        else insn_map
      )

(* TODO refactor superset_cfg_of_mem to use with_mem *)
let superset_cfg_of_mem 
    ?insn_map ?superset_cfg ?brancher ?backend arch mem =
  print_endline "shingled_cfg_of_mem";
  Superset.disasm ?backend ~accu:[] ~f:List.cons arch mem >>|
  fun insns -> (
    let insn_map = Option.value insn_map ~default:Addr.Map.empty in
    let cfg = rcfg_of_superset ?superset_cfg ?brancher insns mem arch in
    let insn_map = superset_to_map insns insn_map cfg in
    insn_map, cfg
  )

let disasm ?superset_cfg ?brancher ?backend arch mem =
  superset_cfg_of_mem ?superset_cfg ?brancher ?backend arch mem >>|
  fun (insn_map, superset_cfg) -> trim insn_map superset_cfg arch

module With_exn = struct
  let disasm ?brancher ?backend arch mem =
    disasm ?brancher ?backend arch mem |> ok_exn
end

let with_img ~accu ~backend img ~f = 
  let arch = Image.arch img in
  let segments = Table.to_sequence @@ Image.segments img in
  Seq.fold segments ~init:accu ~f:(fun accu (mem, segment) ->
      if Image.Segment.is_executable segment then
        (f ~accu ~backend arch mem |> ok_exn)
      else accu 
    )

  print_endline "shingled_cfg_of_img";
let superset_cfg_of_img ?superset_cfg ~backend img =
  let superset_cfg = Option.value superset_cfg ~default:(G.create ()) in
  with_img ~accu:(Addr.Map.empty, superset_cfg) ~backend img
    ~f:(fun ~accu ~backend arch mem -> 
        let (insn_map, superset_cfg) = accu in
        superset_cfg_of_mem ~insn_map ~superset_cfg ~backend arch mem
      )

let superset_cfg_of_file ~backend binary = 
  let img  = Common.img_of_filename binary in
  let arch = Image.arch img in
  let (insn_map, cfg) = superset_cfg_of_img ~backend img in
  (arch, insn_map, cfg)

let trimmed_cfg_of_file ~backend file =
  let (arch, insn_map, cfg) = superset_cfg_of_file ~backend file in
  let insn_map, cfg = trim insn_map cfg arch in
  arch, insn_map, cfg
