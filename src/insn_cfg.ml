open Bap.Std
open Graph
open Core_kernel.Std

module G = Imperative.Digraph.ConcreteBidirectional(struct 
    type t = Addr.t 
    let compare = Addr.compare
    let hash = Addr.hash
    let equal = Addr.equal
  end)
type t = G.t

module Oper = Oper.I(G)
module StrongComponents = Components.Make(G)
module DiscreteComponents = Components.Undirected(G)
module Dfs        = Traverse.Dfs(G)
module Path       = Path.Check(G)
module Gml        = Gml.Print(G)(struct 
    let node (label : G.V.label) = 
      [ "addr", Gml.String (Addr.to_string label)  ]
    let edge (label : G.E.label) = [ ]
  end)

let bad_of_arch arch = 
  G.V.create (Addr.of_int
                ~width:(Size.in_bits @@ Arch.addr_size arch) 0)

let bad_of_addr addr =
  G.V.create (Addr.of_int
                ~width:(Addr.bitwidth addr) 0)


let find_conflicts_with conflicts insn_map addr len =
  let rec within_insn conflicts insn_map cur_addr =
    if Addr.(cur_addr = (addr++len)) || len=1 then
      conflicts
    else
      let conflicts = match Map.find insn_map cur_addr with
        | Some ((mem, _)) -> 
          Hash_set.add conflicts cur_addr; conflicts
        | None ->  conflicts in 
      within_insn conflicts insn_map Addr.(cur_addr ++ 1) in
  within_insn conflicts insn_map addr

let conflicts_within_insn_at conflicts insn_map addr f = 
  match Map.find insn_map addr with
  | Some ((mem, _)) ->
    (* look within the body for instructions *)
    f conflicts insn_map 
      (Memory.min_addr mem) 
      (Memory.length mem) 
  | None -> conflicts

(* TODO make conflicts optional *)
let conflicts_within_insn insn_map insn = 
  let conflicts = Addr.Hash_set.create () in
  conflicts_within_insn_at conflicts insn_map insn find_conflicts_with

let find_conflicts conflicts insn_map insn = 
  conflicts_within_insn_at conflicts insn_map insn find_conflicts_with

let find_all_conflicts insn_map insn_cfg =
  G.fold_vertex (fun vert conflicts -> 
      match Map.find insn_map vert with
      | Some (mem, _) -> 
        let insn_addr = Memory.min_addr mem in
        find_conflicts conflicts insn_map insn_addr
      | None ->
        (* Not supposed to be a vert that isn't *)
        (* in the map *)
        assert false 
    ) insn_cfg (Addr.Hash_set.create ())
