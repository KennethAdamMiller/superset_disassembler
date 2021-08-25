open Core_kernel
open Bap.Std
open Regular.Std
open Format

let typ_size t =
  match t with
  | Bil.Types.Imm s -> s
  | Bil.Types.Mem (_,s) -> Size.in_bits s
  | Bil.Types.Unk -> 0
   
let exp_size e =
  let o = object(self)
            inherit [int] Exp.visitor as super
            method visit_var v s =
              let vsize = typ_size @@ Var.typ v in
              max vsize s
            method visit_int w s =
              let wsize = Addr.bitwidth w in
              max wsize s
          end in
  o#visit_exp e 0  

let vname () = sprintf "v%s" Tid.(to_string @@ Tid.create ()) 
  
let make_var e =
  let sz = exp_size e in
  Var.create (vname ()) Bil.(Imm sz)

let make_def ?tid e newbil r =
  let v1 = (make_var e) in
  let open Bil in
  let d = Move(v1, e) in
  d :: newbil,Map.set r e v1

let transform defmap =
  object(self)
    inherit Exp.mapper as super
    method map_exp e =
      match Map.find defmap (super#map_exp e) with
      | Some (v) -> Bil.Var v
      | None -> super#map_exp e
  end

let is_var e =
  match e with
  | Bil.Var _ -> true
  | _ -> false

let is_int e =
  match e with
  | Bil.Int _ -> true 
  | _ -> false

let get_depth =
  object(self)
    inherit [int * int] Exp.visitor as super
    method enter_exp e (md,cd) =
      let cd = cd+1 in
      max md cd, cd
    method leave_exp e (md,cd) =
      let cd = cd-1 in
      max md cd, cd
  end
       
let is_terminator e = 
  let depth,_ = get_depth#visit_exp e (0,0) in
  depth <= 1
       
let bil_tac =
  object(self)
    inherit [bil * var Exp.Map.t * tid option] Stmt.visitor as super
    method leave_concat l r (newbil,replace,tid) =
      let l = (transform replace)#map_exp l in
      let r = (transform replace)#map_exp r in
      let newbil, replace,tid =
        if not ((is_var l) || (is_int l)) then
          let nb,rep = make_def ?tid l newbil replace in nb,rep,None
        else newbil,replace,tid in
      let newbil, replace,tid =
        if not ((is_var r) || (is_int r)) then
          let nb,rep = make_def ?tid r newbil replace in nb,rep,None
        else newbil,replace,tid in
      newbil,replace,tid

    method leave_cast c i e (newbil,replace,tid) =
      let e = (transform replace)#map_exp e in
      let newbil, replace,tid =
        if not (is_terminator e) then
          let nb,r = make_def ?tid e newbil replace in nb,r,None
        else newbil,replace,tid in
      newbil,replace,tid

    method leave_unop u e (newbil,replace,tid) =
      let e = (transform replace)#map_exp e in
      let newbil, replace,tid =
        if not (is_var e) then
          let nb,rep = make_def ?tid e newbil replace in nb,rep,None
        else newbil,replace,tid in
      newbil,replace,tid

    method leave_binop b e1 e2 (newbil,replace,tid) =
      let e1 = (transform replace)#map_exp e1 in
      let e2 = (transform replace)#map_exp e2 in
      if is_terminator e1 && not (is_terminator e2) then
        let newbil,replace = make_def ?tid e2 newbil replace in
        newbil,replace,None
      else if not (is_terminator e1) && is_terminator e2 then
        let blk_bdr,replace = make_def ?tid e1 newbil replace in
        newbil,replace,None
      else if not (is_terminator e1) && not (is_terminator e2) then
        let blk_bdr,replace = make_def ?tid e1 newbil replace in
        let blk_bdr,replace = make_def e2 blk_bdr replace in
        newbil,replace,None
      else
        newbil, replace,tid

    method leave_load ~mem ~addr en s (blk_bdr,replace,tid) =
      let mem = (transform replace)#map_exp mem in
      let addr = (transform replace)#map_exp addr in
      let blk_bdr,replace,tid = 
        if not (is_terminator mem) then
          let b,r = make_def ?tid mem blk_bdr replace in b,r,None
        else blk_bdr,replace,tid in
      let blk_bdr,replace,tid =
        if not (is_terminator addr) then
          let b,r = make_def ?tid addr blk_bdr replace in b,r,None
        else blk_bdr,replace,tid in
      blk_bdr,replace,tid

    method leave_store ~mem ~addr ~exp en s (blk_bdr,replace,tid) =
      let mem = (transform replace)#map_exp mem in
      let addr = (transform replace)#map_exp addr in
      let exp = (transform replace)#map_exp exp in
      let blk_bdr,replace,tid = 
        if not (is_terminator mem) then
          let b,r = make_def ?tid mem blk_bdr replace in b,r,None
        else blk_bdr,replace,tid in
      let blk_bdr,replace,tid =
        if not (is_terminator addr) then
          let b,r = make_def ?tid addr blk_bdr replace in b,r,None
        else blk_bdr,replace,tid in
      let blk_bdr,replace,tid = 
        if not (is_terminator exp) then
          let b,r = make_def ?tid exp blk_bdr replace in b,r,None
        else blk_bdr,replace,tid in
      blk_bdr, replace,tid

  end   

let map_bil bil =
  let (l,_,_) =
    List.fold bil ~init:([], Exp.Map.empty, None) ~f:(fun accu e ->
        bil_tac#visit_stmt e accu
      ) in
  Stmt.normalize @@ List.rev l
