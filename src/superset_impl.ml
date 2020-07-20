open Bap.Std
open Regular.Std
open Core_kernel
open Or_error
open Graph

module Dis = Disasm_expert.Basic

type elem = mem * (Dis.full_insn option)

module G = 
  Imperative.Digraph.ConcreteBidirectional(struct 
      type t = Addr.t 
      let compare = Addr.compare
      let hash = Addr.hash
      let equal = Addr.equal
    end)

          
type t = {
  arch        : arch;
  filename    : string option;
  main_entry  : addr option;
  sections    : value memmap;
  brancher    : Brancher.t;
  endianness  : endian option;
  lifter      : lifter;
  balanced    : bool;
  insn_map    : (mem * (Dis.full_insn option)) Addr.Map.t;
  insn_risg   : G.t;
  bad         : Addr.Hash_set.t;
  keep        : Addr.Hash_set.t;
  (* marked data  *)
  (* visited *)
  (* union_find *)
}


let of_components
    ?main_entry ?insn_map ?insn_risg ?segments ?endianness ?filename arch =
  let insn_risg =
    match insn_risg with
    | Some insn_risg -> insn_risg
    | None -> G.create () in
  let segments = Option.value segments ~default:Memmap.empty in
  let insn_map  = Option.value insn_map ~default:Addr.Map.empty in
  let balanced =
    Map.(length insn_map) = (G.nb_vertex insn_risg) in
  let module Target = (val target_of_arch arch) in
  let lifter = Target.lift in
  {
    arch        = arch;
    filename;
    sections    = segments;
    brancher    = Brancher.of_bil arch;
    endianness  = None;
    lifter      = lifter;
    main_entry;
    balanced;
    insn_map;
    insn_risg;
    bad         = Addr.Hash_set.create ();
    keep        = Addr.Hash_set.create ();
  }


module P = Persistent.Digraph.ConcreteBidirectional(struct
    type t = Addr.t
    let compare = Addr.compare
    let hash = Addr.hash
    let equal = Addr.equal
  end)
         
module Topological = Topological.Make(G)
module Oper = Oper.I(G)
module StrongComponents = Components.Make(G)
(*module DiscreteComponents = Components.Undirected(G)*)
module Dfs        = Graph.Traverse.Dfs(G)
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

  
type colored_superset = G.t * Addr.Hash_set.t String.Map.t
                        * elem Addr.Map.t

module Make(T : sig val instance : colored_superset end) = struct
  open T
  module Dottable = struct
    type t = colored_superset

    module V = struct
      type t = G.V.t
    end

    module E = struct
      type t = G.E.t
      let src (s,_) = s
      let dst (_,d) = d
    end

    let iter_vertex f (g, _, _) =
      G.iter_vertex f g

    let iter_edges_e f (g, _, _) =
      G.iter_edges_e f g

    let graph_attributes _ = [
      `Fontsize 14;
    ]
    let default_vertex_attributes gr = [
      `Shape `Box; 
      (*`Height 1.0*.Memory.(length mem);*)
      `Fontsize 14;
      `Fontcolor 0x666699;
      `Fontname "Monospace";
      `Width 1.0
    ]

    let red = 0xff0000
    let green = 0x009900
    let yellow = 0xffff00
    let blue = 0x0000ff
    let orange = 0xff6600
    let purple = 0x660066
    let brown = 0x663300
    let cyan = 0x0099cc

    let vertex_name name =
      let fmt = Format.str_formatter in
      Addr.(pp_generic ~prefix:`none ~suffix:`none ~format:`dec
              fmt name);
      Format.flush_str_formatter ()

    let vertex_attributes v =
      let default_attrs =
        [
          `Label ((vertex_name v));
        ] in
      let g, colors, insn_map = instance in
      let contains name =
        match Map.find colors name with
        | Some(s) ->
          Hash_set.mem s v
        | None -> false in
      let find_update default_attrs name color =
        if contains name then
          `Color color :: default_attrs
        else default_attrs in
      let default_attrs =
        find_update default_attrs "False Negatives" red in
      let default_attrs =
        find_update default_attrs "True Positives" green in
      let default_attrs =
        find_update default_attrs "False Positives" yellow in
      let default_attrs =
        match List.hd default_attrs with
        | Some (`Color _) -> 
          default_attrs
        | _ -> `Color 0X660000 :: default_attrs  in
      match Map.find insn_map v with
      | Some(mem,insn) ->
        let len = float_of_int Memory.(length mem) in
        `Height (1.0 *. len) ::
        default_attrs
      | None -> default_attrs


    let get_subgraph _ = None
    let default_edge_attributes _ = [
      `Penwidth 1.0;
      `Arrowsize 0.5;
      `Headport `N;
      `Tailport `S;
      `Labelfloat true;
    ]

    let edge_attributes (src,dst) =
      (*let color,weight = match kind,arity with
        | `Fall,`Many -> 0x660000, 4
        | `Fall,`Mono -> 0x000066, 8
        | `Cond,_ -> 0x006600, 2
        | `Jump,_ -> 0x000066, 2 in*)
      [
        (*`Color color;*)
        (*`Weight weight;*)
      ]
  end
  module Dot = Graph.Graphviz.Dot(Dottable)
  include Dot
end

let fold_component ?visited ~pre ~post g accu addr =
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
  push addr;
  let rec loop acc =
    match Stack.pop s with
    | Some v ->
      let acc = pre acc v in
      G.iter_succ push g v;
      loop @@ post acc v
    | None -> acc
  in
  loop accu

