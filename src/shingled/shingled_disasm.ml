open Core_kernel.Std
open Regular.Std
open Graphlib.Std
open Bap.Std
open Or_error.Monad_infix

module Dis = Disasm_expert.Basic
module Cfg = Graphs.Cfg


let prev_chunk mem ~addr =
  let prev_addr = Addr.pred addr in
  Memory.view ~from:prev_addr mem

let targ_in_mem gmem_min gmem_max addr =
  Addr.(addr >= gmem_min && addr < gmem_max)

let is_exec_ok gmem_min gmem_max lmem  =
  Addr.(Memory.min_addr lmem >= gmem_min) &&
  Addr.(Memory.max_addr lmem <= gmem_max)

let static_successors brancher ~min ~max mem insn =
  if is_exec_ok min max mem
  then Brancher.resolve brancher mem insn
  else [None, `Fall]


(* Sheer simply prunes known unacceptable receiving state points by
   calculating the transitive closure of a node set of x, if it
   contains an invalid instruction, this x is invalid too  *)
let sheer brancher shingles gmem =
  (* depth first search for all that bad points to *)
  let mark_data shingles mem =
    Memmap.add (Memmap.remove shingles mem) mem None in
  let min,max = Memory.(min_addr gmem, max_addr gmem) in
  let target_in_mem = targ_in_mem min max in
  let get_targets = static_successors brancher ~min ~max in
  let lookup addr = Memmap.lookup shingles addr |>
                    Seq.find ~f:(fun (mem,_) ->
                        Addr.equal addr (Memory.min_addr mem)) in
  let find_mem addr f default= match lookup addr with
    | Some (mem, _) -> f default mem
    | _ -> default in
  let is_data addr = not (target_in_mem addr) ||
                     match lookup addr with
                     | Some (mem, (Some _)) -> false
                     | _ -> true in
  let visited = Array.create ~len:(Addr.to_int max|>ok_exn) false in
  (* TODO refactor this to use the control flow graph and DFS on bad *)
  let rec sheer_data shingles addr =
    if not (target_in_mem addr)
    then find_mem addr mark_data shingles
    else if visited.(Addr.to_int addr |> ok_exn) then
      sheer_data shingles (Addr.succ addr)
    else (
      visited.(Addr.to_int addr |> ok_exn) <- true;
      match lookup addr with
      | Some (mem, Some insn) ->
        let targets = get_targets mem insn in
        let shingles, result =
          List.fold ~init:(shingles,false) targets
            ~f:(fun (shingles, so_far) (target,_) -> match target with
                | Some target ->
                  if so_far || is_data target
                  then mark_data shingles mem, true
                  else sheer_data shingles target, false
                | None -> mark_data shingles mem, true) in
        let shingles, final_result =
          List.fold ~init:(shingles, result) targets
            ~f:(fun (shingles, so_far) (target,_) -> match target with
                | Some target ->
                  if (so_far || is_data target) then
                    mark_data shingles mem, true
                  else shingles, so_far
                | None -> shingles, true) in
        if final_result
        then sheer_data (mark_data shingles mem) (Addr.succ addr)
        else sheer_data shingles (Addr.succ addr)
      | Some (_, None)
      | None -> sheer_data shingles (Addr.succ addr)) in
  sheer_data shingles min

let dests_of_shingles brancher shingles =
  let dests = Addr.Table.create () in
  Memmap.to_sequence shingles |> Seq.iter ~f:(function
      | _,None -> ()
      | mem,Some insn ->
        Brancher.resolve brancher mem insn |> List.iter ~f:(function
            | None,_ -> ()
            | Some addr,kind ->
              Hashtbl.add_multi dests ~key:(Memory.min_addr mem)
                ~data:(addr,kind)));
  dests

let find_barriers dests =
  let barriers = Addr.Hash_set.create () in
  let mark_barrier = Hash_set.add barriers in
  Hashtbl.iteri dests ~f:(fun ~key:src ~data:dsts -> match dsts with
      | _ :: _ :: _ ->
        List.iter dsts ~f:(fun (addr,_) -> mark_barrier addr)
      | _ -> ());
  barriers

let lift lift (mem,insn) =
  match insn with
  | None -> None
  | Some insn ->
    let bil = match lift mem insn with
      | Ok bil -> Some bil
      | Error _ -> None in
    Some (mem,Insn.of_basic ?bil insn)

let run ?cfg ?brancher shingles arch mem : cfg =
  let cfg = Option.value cfg ~default:Cfg.empty in
  let brancher = Option.value brancher ~default:(Brancher.of_bil arch) in
  let module Target = (val target_of_arch arch) in
  let lifter = Target.lift in
  let shingles = sheer brancher shingles mem in
  let dests  = dests_of_shingles brancher shingles in
  let nodes = Addr.Table.create () in
  let barriers = find_barriers dests in
  let is_barrier mem = Hash_set.mem barriers (Memory.min_addr mem) in
  let insert_node rev_insns cfg =
    let blk = Block.create mem (List.rev rev_insns) in
    Hashtbl.set nodes ~key:(Block.addr blk) ~data:blk;
    Cfg.Node.insert blk cfg in
  let node = Hashtbl.find nodes in
  let cfg,rest =
    Memmap.to_sequence shingles |>
    Seq.filter_map ~f:(lift lifter) |>
    Seq.fold ~init:(cfg,[]) ~f:(fun (cfg,insns) (mem,insn) ->
        if is_barrier mem then match insns with
          | [] -> cfg, [mem, insn]
          | insns -> insert_node ((mem,insn) :: insns) cfg, []
        else cfg, (mem,insn) :: insns) in
  let cfg = match rest with
    | [] -> cfg
    | insns -> insert_node insns cfg in
  Hashtbl.fold dests ~init:cfg ~f:(fun ~key:addr ~data:dests cfg ->
      match node addr with
      | None -> cfg
      | Some src ->
        List.fold dests ~init:cfg ~f:(fun cfg (addr,kind) ->
            match node addr with
            | None -> cfg
            | Some dst ->
              let e = Cfg.Edge.create src dst kind in
              Cfg.Edge.insert e cfg))

module Conservative = struct
  open Disasm_expert.Basic
  type maybe_insn = mem * (asm, kinds) insn option

  let run dis mem =
    let rec disasm accu cur_mem =
      let elem = match Dis.insn_of_mem dis cur_mem with
        | Ok (m, insn, _) -> m, insn
        | Error _ -> cur_mem, None in
      match prev_chunk mem ~addr:(Memory.min_addr cur_mem) with
      | Ok next -> disasm (elem :: accu) next
      | Error _ -> elem :: accu in
    match Memory.view mem ~from:(Memory.max_addr mem) with
    | Ok cur_mem -> Ok (disasm [] cur_mem)
    | Error err -> Error err

  let disasm ?(backend="llvm") arch mem =
    with_disasm ~backend (Arch.to_string arch) ~f:(fun d -> run d mem)

  module With_exn = struct
    let disasm ?backend arch mem = disasm ?backend arch mem |> ok_exn
  end
end

let to_memmap insns =
  List.fold insns ~init:Memmap.empty ~f:(fun shingles -> function
      | mem,None -> Memmap.add shingles mem None
      | mem,Some insn -> Memmap.add shingles mem (Some insn))

let disasm ?brancher ?backend arch mem =
  Conservative.disasm ?backend arch mem >>| fun insns ->
  run ?brancher (to_memmap insns) arch mem

module With_exn = struct
  let disasm ?brancher ?backend arch mem =
    disasm ?brancher ?backend arch mem |> ok_exn
end
