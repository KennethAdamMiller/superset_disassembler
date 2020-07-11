open Bap.Std
open Graph
open Core_kernel

module G = Imperative.Digraph.ConcreteBidirectional(struct 
    type t = Addr.t 
    let compare = Addr.compare
    let hash = Addr.hash
    let equal = Addr.equal
  end)
type t = G.t

module P = Persistent.Digraph.ConcreteBidirectional(struct
    type t = Addr.t
    let compare = Addr.compare
    let hash = Addr.hash
    let equal = Addr.equal
  end)

module Topological = Topological.Make(G)
module Dominator = Dominator.Make(G)
module Oper = Oper.I(G)
module StrongComponents = Components.Make(G)
(*module DiscreteComponents = Components.Undirected(G)*)
module Dfs        = Graph.Traverse.Dfs(G)
module Bfs        = Graph.Traverse.Bfs(G)
module Path       = Path.Check(G)
module GmlOut     = Gml.Print(G)(struct 
    let node (label : G.V.label) = 
      [ "addr", Gml.String (Addr.to_string label) ]
    let edge _ = []
  end)
module B = struct
  module G = struct
    include P
  end
  include P
  let copy g = g
  let empty () = P.empty
end
module GmlIn      = Gml.Parse(B)(struct
    let node (labels : Gml.value_list) = 
      match labels with
      | [] -> assert false
      | fail :: [] -> assert false
      | (id, idval) :: (s, gmlval) :: _ -> 
        match idval, gmlval with
        | Gml.Int(idval), Gml.String(addr) -> 
          B.G.V.label Addr.(of_string addr)
        | _ -> assert false

    let edge (labels : Gml.value_list) = ()
  end)
module Gml = struct
  include GmlIn
  include GmlOut
  let parse gmlstr = 
    let pgraph = parse gmlstr in
    let igraph = G.create () in
    P.iter_edges (fun src target -> 
        let src    = B.G.V.create src in
        let target = B.G.V.create target in
        G.add_edge igraph src target;
      ) pgraph;
    igraph
end

let add ?superset_risg mem insn =
  let superset_risg =
    Option.value superset_risg ~default:(G.create ()) in
  let src = Memory.min_addr mem in
  let bad = Addr.of_int ~width:(Addr.bitwidth src) 0 in
  match insn with
  | Some(insn) ->
    G.add_vertex superset_risg src;
  | None -> G.add_edge superset_risg bad src

let risg_of_raw_superset ?superset_risg raw_superset =
  let superset_risg = Option.value superset_risg ~default:(G.create ()) in
  List.iter raw_superset ~f:(fun (mem, insn) ->
      add ~superset_risg mem insn
    );
  superset_risg

let parent_conflict_at insn_risg insn_map addr =
  let children = G.pred insn_risg addr in
  List.fold children ~init:Addr.Set.empty ~f:(fun cparents child -> 
      let parents = G.succ insn_risg child in
      List.fold parents ~init:cparents ~f:(fun cparents parent -> 
          if not Addr.(parent = addr) then
            match Map.find insn_map parent with
            | Some(mem, _) -> 
              let len = Memory.length mem in
              if Addr.(parent < addr) && Addr.(addr < (parent ++ len)) then
                Set.add cparents parent
              else cparents
            | None -> cparents
          else cparents
        )
    )

let get_loop_addrs insn_risg = 
  let loop_addrs = 
    StrongComponents.scc_list insn_risg in
  List.fold_left loop_addrs ~init:Addr.Set.empty 
    ~f:(fun loop_addrs loop -> 
        List.fold_left ~init:loop_addrs loop ~f:(fun loop_addrs addr -> 
            Set.add loop_addrs addr
          )
      )

let fold_component ?visited ~pre ~post i g v0 =
  let visited = Option.value visited
      ~default:(Addr.Hash_set.create ()) in
  let s = Stack.create () in
  (* invariant: [h] contains exactly the vertices which have been pushed *)
  let push v =
    if not (Hash_set.mem visited v) then begin
      Hash_set.add visited v;
      Stack.push s v
    end
  in
  push v0;
  let rec loop acc =
    match Stack.pop s with
    | Some v ->
      let acc = pre acc v in
      G.iter_succ push g v;
      loop @@ post acc v
    | None -> acc
  in
  loop i

