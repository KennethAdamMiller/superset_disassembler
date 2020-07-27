open Core_kernel
open Bap.Std


(** A reducer is a modular way to extend the propagation of bad
    insns with other functionalities, such as metrics collection, but
    is openly extensible. *)
module type Reducer = sig
  type acc
  val accu : acc
  val check_pre : Superset.t -> acc -> addr -> acc
  val check_elim : Superset.t -> acc -> addr -> bool
  val check_post : Superset.t -> acc -> addr -> acc
  val mark  : Superset.t -> acc -> addr -> unit
end

(** A functor that weaves into the trim traversal the reducer of choice. *)
module Reduction(R : Reducer) = struct

  let visited = Addr.Hash_set.create () 

  let post superset accu addr =
    let module G = Superset.ISG in
    if R.check_elim superset accu addr then (
      R.mark superset accu addr;
    );
    R.check_post superset accu addr

  let trim superset =
    let superset = Superset.Core.rebalance superset in
    (* let orig_size = (G.nb_vertex superset_risg) in *)
    let post = post superset in
    let pre = R.check_pre superset in
    let _ = Superset.with_bad superset ~visited ~pre ~post R.accu in
    let superset = 
      Hash_set.fold visited ~init:superset ~f:(fun superset addr ->
          Superset.ISG.remove superset addr
        ) in
    Hash_set.clear visited;
    Superset.Core.clear_all_bad superset;
    (*let trimmed_size = (G.nb_vertex superset_risg) in
      let num_removed = orig_size - trimmed_size in
      printf "%d vertices after trimming, removing %d\n" 
      trimmed_size num_removed;*)
    Superset.Core.rebalance superset

end

(** A tiny partial Reducer instance that doesn't trim. *)
module Disabled = struct
  let post _ accu _ = accu
  let trim superset = superset
end

(** The default trimming reducer, doesn't introduce any complication
    or feature to the surrounding functor facilities. *)
module Default = Reduction(struct
    type acc = unit
    let accu = ()
    let check_pre _ accu _ = accu
    let check_post _ accu _ = accu
    let check_elim _ _ _ = true
    let mark _ _ _ = ()
  end)


(** A reducer that tries to work around dead blocks. It blocks
    trimming from occurring except in places where a given address
    may be a control flow affecting instruction. *)
module DeadblockTolerantReducer : Reducer = struct
  type acc = Superset.elem option
  let accu = None
  let check_pre superset accu addr =
    match accu with 
    | Some _ -> accu
    | None -> (
        match Superset.Core.lookup superset addr with
        | Some (mem,insn) -> (
            match insn with
            | Some i -> 
              let i = Insn.of_basic i in
              if Insn.may Insn.affect_control_flow i then
                Some (mem,insn) else None
            | None -> None
          )
        | None -> None
      )

  let check_post superset accu addr =
    match accu with
    | Some(mem,insn) ->
      if Memory.(min_addr mem) = addr then
        None
      else accu
    | None -> accu

  let check_elim superset accu addr =
    Option.is_none accu

  let mark _ _ _ = ()
end

module DeadblockTolerant = Reduction(DeadblockTolerantReducer)
