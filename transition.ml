open Core_kernel.Std
open Bin_prot.Std
open Regular.Std
open Bap.Std

module Dis = Disasm_expert.Basic
module Cfg = Graphs.Cfg

module Mat = struct
  open Bigarray
  include Array2
  let create n = create Float64 fortran_layout n n
  let size m = dim1 m
end

module Corpus = struct
  module T = struct
    type t = {
      mat : mat;
      ids : int String.Table.t;
    } with bin_io, sexp
    let version = "0.1"
  end
  include T

  let grow x : mat =
    let n = Mat.size x in
    let y = Mat.create (n+1) in
    for i = 1 to n do
      for j = 1 to n do
        y.{i,j} <- x.{i,j}
      done;
    done;
    for i = 1 to n + 1 do y.{i,n+1} <- 0. done;
    for j = 1 to n + 1 do y.{n+1,j} <- 0. done;
    y

  let add_insn t insn =
    let id = Hashtbl.find_or_add t.ids (Insn.name insn)
        ~default:(fun () -> Hashtbl.length t.ids) in
    if Hashtbl.length t.ids > Mat.size t.mat
    then {t with mat = grow t.mat}, id
    else t, id

  let update_link t (src,dst) =
    let t,i = add_insn t src in
    let t,j = add_insn t dst in
    t.mat.{i,j} <- t.mat.{i,j} +. 1.0;
    t

  let link_of_edge e =
    Block.terminator (Cfg.Edge.src e),
    Block.leader     (Cfg.Edge.dst e)

  let links_of_block blk =
    match Block.insns blk with
    | (_,x) :: xs ->
      Seq.unfold ~init:(x,xs) ~f:(fun (x,xs) ->
          match xs with
          | (_,y) :: xs -> Some ((x,y), (y,xs))
          | [] -> None)
    | _ -> Seq.empty

  let links_of_cfg cfg =
    Seq.append
      (Cfg.nodes cfg |> Seq.concat_map ~f:links_of_block)
      (Cfg.edges cfg |> Seq.map ~f:link_of_edge)

  let deadends cfg =
    Cfg.nodes cfg |> Seq.filter_map ~f:(fun blk ->
        if Seq.is_empty (Cfg.Node.outputs blk cfg)
        then Some (Block.terminator blk)
        else None)

  let update_data t cfg =
    deadends cfg |> Seq.fold ~init:t ~f:(fun t insn ->
        let t,i = add_insn t insn in
        t.mat.{i,1} <- t.mat.{i,1} +. 1.0;
        t)

  let update t cfg : t =
    links_of_cfg cfg |>
    Seq.fold ~init:(update_data t cfg) ~f:update_link

  let empty = {
    mat = Mat.create 0;
    ids = String.Table.create ()
  }

  let init cfg = update empty cfg

  include Data.Make(T)

  let () =
    let desc = "binprot format" and ver = version and name = "bin" in
    add_reader ~desc ~ver name (Data.bin_reader (module T));
    add_writer ~desc ~ver name (Data.bin_writer (module T))
end

type corpus = Corpus.t
type t = insn -> insn option -> float

let row_sum m i =
  let sum = ref 0. in
  for j = 1 to Mat.size m do
    sum := !sum +. m.{i,j}
  done;
  !sum

let estimate_probabilities p x n =
  for i = 1 to n do
    let s = row_sum x i in
    for j = 1 to n do
      p.{i,j} <- x.{i,j} /. s
    done
  done

(** [average i j m n p] is a
    $\over{\Sum_{i,j}^{i <= m,j<=n}p(i,j)}{m-i+1 \times n-j+1}$
*)
let average ?(i=1) ?(j=1) ?m ?n p =
  let size = Mat.size p in
  let m = Option.value m ~default:size in
  let n = Option.value n ~default:size in
  let sum = ref 0. in
  for i = i to m do
    for j = j to n do
      sum := !sum +. p.{i,j}
    done
  done;
  let m = m - i + 1 in
  let n = n - j + 1 in
  !sum /. (float m *. float n)

let estimate t : t =
  let open Corpus in
  let n = Mat.size t.mat in
  let p = Mat.create n in
  let id = function
    | None -> Some 1
    | Some insn -> Hashtbl.find t.ids (Insn.name insn) in
  estimate_probabilities p t.mat n;
  fun src dst -> match id (Some src), id dst with
    | Some i, Some j -> p.{i,j}
    | None,None    -> average ~i:2 p
    | Some i, None -> average ~i ~m:i p
    | None, Some j -> average ~i:2 ~j ~n:j p


let probability t = t
