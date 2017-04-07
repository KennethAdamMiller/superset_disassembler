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
        "Lift failed on memory at " ^ Memory.to_string mem; 
        [None, `Fall]
      )
  else [None, `Fall]

(* TODO need to test cfg_of_shingles in unit tests further *)
let cfg_of_shingles ?superset_cfg ?brancher shingles gmem arch =
  print_endline "cfg_of_shingles";
  (* depth first search for all that bad points to *)
  let brancher = Option.value brancher ~default:(Brancher.of_bil arch) in
  let superset_cfg = Option.value superset_cfg ~default:(G.create ()) in
  let min,max = Memory.(min_addr gmem, max_addr gmem) in
  let target_in_mem = is_exec_ok min max in
  let bad = Insn_cfg.bad_of_arch arch in
  (* TODO touches non-mem *)
  let accesses_non_mem insn = false in
  let get_targets = static_successors brancher ~min ~max in
  let is_non_code addr insn = 
    not (target_in_mem addr) ||
    accesses_non_mem insn in
  List.iter shingles ~f:(fun (mem, insn) ->
      let src = Memory.min_addr mem in
      match insn with
      | Some(insn) ->
        let targets = get_targets mem insn in
        List.iter targets
          ~f:(fun (target,_) ->
              match target with 
              | Some(target) -> 
                if is_non_code target insn then
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

let sheer insn_map superset_cfg arch =
  print_endline "Shingled.sheer";
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

let shingled_to_map shingle_superset insn_map insn_cfg = 
  print_endline "shingled_to_map";
  List.fold_left ~init:insn_map shingle_superset
    ~f:(fun insn_map (mem, insn) ->
        let addr = (Memory.min_addr mem) in
        if Insn_cfg.G.mem_vertex insn_cfg addr then
          Addr.Map.add insn_map addr (mem, insn)
        else insn_map
      )

(* TODO refactor shingled_cfg_of_mem to use with_mem *)
let shingled_cfg_of_mem 
    ?insn_map ?superset_cfg ?brancher ?backend arch mem =
  print_endline "shingled_cfg_of_mem";
  Superset.disasm ?backend ~accu:[] ~f:List.cons arch mem >>|
  fun insns -> (
    let insn_map = Option.value insn_map ~default:Addr.Map.empty in
    let cfg = cfg_of_shingles ?superset_cfg ?brancher insns mem arch in
    let insn_map = shingled_to_map insns insn_map cfg in
    insn_map, cfg
  )

let disasm ?superset_cfg ?brancher ?backend arch mem =
  shingled_cfg_of_mem ?superset_cfg ?brancher ?backend arch mem >>|
  fun (insn_map, superset_cfg) -> sheer insn_map superset_cfg arch

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

let shingled_cfg_of_img ?superset_cfg ~backend img =
  print_endline "shingled_cfg_of_img";
  let superset_cfg = Option.value superset_cfg ~default:(G.create ()) in
  with_img ~accu:(Addr.Map.empty, superset_cfg) ~backend img
    ~f:(fun ~accu ~backend arch mem -> 
        let (insn_map, superset_cfg) = accu in
        shingled_cfg_of_mem ~insn_map ~superset_cfg ~backend arch mem
      )

let shingled_cfg_of_file ~backend binary = 
  let img  = Common.img_of_filename binary in
  let arch = Image.arch img in
  let (insn_map, cfg) = shingled_cfg_of_img ~backend img in
  (arch, insn_map, cfg)

let sheered_cfg_of_file ~backend file =
  let (arch, insn_map, cfg) = shingled_cfg_of_file ~backend file in
  let insn_map, cfg = sheer insn_map cfg arch in
  arch, insn_map, cfg
