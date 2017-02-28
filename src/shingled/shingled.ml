open Core_kernel.Std
open Regular.Std
open Bap.Std
open Or_error.Monad_infix
open Insn_cfg

type t = Insn_cfg.t
let is_exec_ok gmem_min gmem_max addr =
  Addr.(addr >= gmem_min && addr <= gmem_max)

let static_successors brancher ~min ~max mem insn =
  if is_exec_ok min max (Memory.min_addr mem)
  then Brancher.resolve brancher mem insn
  else [None, `Fall]

let cfg_of_shingles ?superset_cfg ?brancher shingles gmem arch =
  (* depth first search for all that bad points to *)
  let brancher = Option.value brancher ~default:(Brancher.of_bil arch) in
  let superset_cfg = Option.value superset_cfg ~default:(G.create ()) in
  let min,max = Memory.(min_addr gmem, max_addr gmem) in
  let target_in_mem = is_exec_ok min max in
  (* TODO touches non-mem *)
  let bad = Insn_cfg.bad_of_arch arch in
  let accesses_non_mem insn = false in
  let get_targets = static_successors brancher ~min ~max in
  let is_non_code addr insn = 
    not (target_in_mem addr) ||
    accesses_non_mem insn in
  let add_shingles superset_cfg shingles =
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
                  else 
                    G.add_edge superset_cfg target src
                | _ -> ()
              )
        | None -> 
          G.add_edge superset_cfg bad src
      )
  in add_shingles superset_cfg shingles;
  superset_cfg

let sheer superset_cfg arch =
  let bad = bad_of_arch arch in
  let to_drop = Addr.Hash_set.create () in
  Dfs.prefix_component (Hash_set.add to_drop) superset_cfg bad;
  Hash_set.iter to_drop ~f:(G.remove_vertex superset_cfg);
  G.remove_vertex superset_cfg bad; 
  superset_cfg


(* TODO refactor shingled_cfg_of_mem to use with_mem *)
let shingled_cfg_of_mem ?superset_cfg ?brancher ?backend arch mem =
  Superset.disasm ?backend ~accu:[] ~f:List.cons arch mem >>|
  fun insns ->
  cfg_of_shingles ?superset_cfg ?brancher insns mem arch

let disasm ?superset_cfg ?brancher ?backend arch mem =
  shingled_cfg_of_mem ?superset_cfg ?brancher ?backend arch mem >>|
  fun  superset_cfg -> sheer superset_cfg arch

module With_exn = struct
  let disasm ?brancher ?backend arch mem =
    disasm ?brancher ?backend arch mem |> ok_exn
end

let with_img ~accu ~backend img ~f = 
  let arch = Image.arch img in
  Memmap.to_sequence (Image.memory img)
  |> Seq.fold ~init:accu ~f:(fun accu (mem,_) ->
      (f ~accu ~backend arch mem |> ok_exn))

let shingled_cfg_of_img ?superset_cfg ~backend img =
  let superset_cfg = Option.value superset_cfg ~default:(G.create ()) in
  with_img ~accu:superset_cfg ~backend img
    ~f:(fun ~accu ~backend arch mem -> 
        shingled_cfg_of_mem ~superset_cfg:accu ~backend arch mem)

let shingled_cfg_of_file ~backend binary = 
  let img  = Common.img_of_filename binary in
  let arch = Image.arch img in
  let cfg = shingled_cfg_of_img ~backend img in
  (arch, cfg)

let sheered_cfg_of_file ~backend file =
  let (arch, cfg) = shingled_cfg_of_file ~backend file in
  arch, sheer cfg arch

let shingled_to_map shingle_superset insn_map = 
  List.fold ~init:insn_map shingle_superset
    ~f:(fun insn_map (mem, insn) ->
        Map.add insn_map (Memory.min_addr mem) (mem, insn)
      )
