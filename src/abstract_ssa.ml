open Bap.Std
open Core_kernel
open Graphlib.Std

let stmt_def_mem =
  object(self)
    inherit [Exp.Set.t] Stmt.visitor
    method enter_load ~mem ~addr e s accu =
      Set.add accu mem
    method enter_store ~mem ~addr ~exp e s accu =
      Set.add (Set.add accu addr) mem
  end

let stmt_use_mem =
  object(self)
    inherit [Exp.Set.t] Stmt.visitor
    method enter_store ~mem ~addr ~exp e s accu =
      Set.add (Set.add (Set.add accu exp) addr) mem
  end

let stmt_def_vars =
  object(self)
    inherit [Exp.Set.t] Stmt.visitor
    method enter_move def use accu =
      Set.add accu Exp.(Bil.Var def)
    method enter_let v ~exp ~body accu =
      Set.add accu Bil.(Var v)
  end

let stmt_use_vars =
  object(self)
    inherit [Exp.Set.t] Stmt.visitor
    method visit_var v accu =
      Set.add accu Bil.(Var v)
    method visit_let v ~exp ~body accu =
      Set.add (Set.add accu exp) body
  end

let stmt_def_freevars =
  object(self)
    inherit [Var.Set.t] Stmt.visitor
    method enter_move def use accu =
        Set.add accu def
    method enter_load ~mem ~addr e s accu =
      match mem with
      | Bil.Var v -> Set.add accu v
      | _ -> accu
    method enter_store ~mem ~addr ~exp e s accu =
      match addr with
      | Bil.Var v -> Set.add accu v
      | _ -> accu
  end

let stmt_use_freevars =
  object(self)
    inherit [Var.Set.t] Stmt.visitor
    method enter_move def use accu =
      let free_vars = (Exp.free_vars use)
        (*Set.filter ~f:(fun v -> not Var.(is_virtual v)) (Exp.free_vars use)*)
      in Set.union accu free_vars
    method enter_load ~mem ~addr e s accu =
      match addr with
      | Bil.Var v -> Set.add accu v
      | _ -> accu
    method enter_store ~mem ~addr ~exp e s accu =
      match exp with
      | Bil.Var v -> Set.add accu v
      | _ -> accu
  end

type rev_ssa = {
    defs   : Var.Set.t;
    uses   : Var.Set.t;
  }

let transitions superset =
  Superset.ISG.fold_vertex superset (fun addr fs ->
      match Superset.Core.lift_at superset addr with
      | Some bil ->
         Addr.Map.add_exn fs addr {
             defs = stmt_def_freevars#run bil Var.Set.empty;
             uses = List.fold bil ~init:Var.Set.empty
                      ~f:(fun fvars stmt ->
                        Set.union fvars (Stmt.free_vars stmt));
           }
      | None -> fs
    ) Addr.Map.empty

let (++) = Set.union and (--) = Set.diff

let compute_liveness superset =
  let _exit = Addr.of_int ~width:1 0 in
  let start = Addr.of_int ~width:1 1 in
  let init = Solution.create Addr.Map.empty Var.Set.empty in
  let tran = transitions superset in
  let module G = Superset_impl.G in
  Superset.ISG.fixpoint superset ~init ~start ~rev:true
    ~merge:Set.union
    ~equal:Var.Set.equal
    ~f:(fun n vars ->
        if Addr.equal n _exit || Addr.equal n start then vars
        else
          match Map.find tran n with
          | Some {defs; uses} ->
             vars -- defs ++ uses
          | None -> vars
    )

let def_mem_ssa bil = 
  stmt_def_mem#run bil Exp.Set.empty

let use_mem_ssa bil =
  stmt_use_mem#run bil Exp.Set.empty
  
let def_ssa bil =
  stmt_def_vars#run bil Exp.Set.empty

let use_ssa bil =
  stmt_use_vars#run bil Exp.Set.empty 

let def_freevars bil =
  stmt_def_freevars#run bil Var.Set.empty

let use_freevars bil =
  stmt_use_freevars#run bil Var.Set.empty 
