open Core_kernel.Std
open Regular.Std
open Bap.Std
open Or_error.Monad_infix
module Dis = Disasm_expert.Basic

open Disasm_expert.Basic

let prev_chunk mem ~addr =
  let prev_addr = Addr.pred addr in
  Memory.view ~from:prev_addr mem

let run dis ~accu ~f mem =
  let rec disasm accu cur_mem =
    let elem = match Dis.insn_of_mem dis cur_mem with
      | Ok (m, insn, _) -> m, insn
      | Error _ -> cur_mem, None in
    match prev_chunk mem ~addr:(Memory.min_addr cur_mem) with
    | Ok next -> disasm (f elem accu) next
    | Error _ -> (f elem accu) in
  match Memory.view mem ~from:(Memory.max_addr mem) with
  | Ok cur_mem -> Ok (disasm accu cur_mem)
  | Error err -> Error err

let disasm ?(backend="llvm") ~accu ~f arch mem =
  with_disasm ~backend (Arch.to_string arch) ~f:(fun d -> run d ~accu ~f mem)

let memmap_all ?superset_cfg ?brancher ?backend arch mem =
  let add = fun elem memmap -> 
    let (mem, insn) = elem in 
    Memmap.add memmap mem insn in
  disasm ?backend ~accu:Memmap.empty ~f:add arch mem

module With_exn = struct
  let disasm ?backend ~accu ~f arch mem = 
    disasm ?backend ~accu ~f arch mem |> ok_exn
end

(* component returns the set of jump points and destinations *)
let all_indirections brancher shingles =
  let dests = Addr.Table.create () in
  Seq.iter shingles ~f:(function
      | _,None -> ()
      | mem,Some insn ->
        Brancher.resolve brancher mem insn |> List.iter ~f:(function
            | None,_ -> ()
            | Some addr,kind ->
              Hashtbl.add_multi dests ~key:(Memory.min_addr mem)
                ~data:(addr,kind)));
  dests

let barriers_of_dests dests =
  let barriers = Addr.Hash_set.create () in
  let mark_barrier = Hash_set.add barriers in
  Hashtbl.iteri dests ~f:(fun ~key:src ~data:dsts -> match dsts with
      | _ :: _ :: _ ->
        List.iter dsts ~f:(fun (addr,_) -> mark_barrier addr)
      | _ -> ());
  barriers
