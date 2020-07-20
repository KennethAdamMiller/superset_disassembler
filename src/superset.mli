open Bap.Std
open Core_kernel

module Dis = Disasm_expert.Basic
type elem = mem * Dis.full_insn option

type t = Superset_impl.t

module ISG : sig
  val ancestors : t -> addr -> addr list
  val descendants : t -> addr -> addr list
  val mem_vertex : t -> addr -> bool
  val iter_vertex : t -> (addr -> unit) -> unit
  val fold_edges : t -> (addr -> addr -> 'a -> 'a) -> 'a -> 'a
  val link : t -> addr -> addr -> t
  val unlink : t -> addr -> addr -> t
  val remove : t -> addr -> t
  val raw_loops : t -> addr list list
  val dfs_fold :
    ?visited:Addr.Hash_set.t -> t -> pre:('a -> addr -> 'a) ->
    post:('a -> addr -> 'a) -> 'a -> addr -> 'a
  val print_dot : ?colorings:Addr.Hash_set.t String.Map.t -> t -> unit
  val filter : t -> Addr.Hash_set.t -> t
  val format_isg : ?format:Format.formatter -> t -> unit
  val isg_to_string : t -> string
end

module Core : sig

  val add : t -> mem -> Dis.full_insn option -> t
  val empty : arch -> t

  (** This primary core function is the core of disassembly, and
  simply reads each byte consecutively in memory by address
  successor. *)
  val run_seq :
    ('a, 'b) Dis.t ->
    mem ->
    (mem * (Dis.asm, Dis.kinds) Dis.insn option) seq

  (** This primary core function is the core of disassembly, and simply 
   reads each byte consecutively in memory by address successor. It 
   is alike to run_seq, but it hides the sequence part, and accepts 
   a parameter lambda. *)
  val run :
    ('a, 'b) Dis.t ->
    accu:'c ->
    f:(mem * (Dis.asm, Dis.kinds) Dis.insn option -> 'c -> 'c) ->
    mem -> 'c
  (** This primary core function is the core of disassembly, and simply 
   reads each byte consecutively in memory by address successor. It 
   builds the disassembler and runs the superset behind the 
   scenes. One can accumulate with any arbitrary type. Later 
   accessories tuck the results into a custom superset 
   representation with graph specializations suited to the 
   invariants, heuristics and performance that are vital to good 
   operation. *)
  val disasm :
    ?backend:string ->
    accu:'a ->
    f:(mem * (Dis.asm, Dis.kinds) Dis.insn option -> 'a -> 'a) ->
    Arch.t -> mem -> 'a Or_error.t
  val lift_insn : t -> (mem * Dis.full_insn option) -> (mem * bil) option
  val lift : t -> 
    (mem * Dis.full_insn option) List.t ->
    (bil * int) Addr.Map.t

  (** The primary disassembler design interface. Implementing a
      disassembler from the ground up almost certainly uses this as
      it loads the memory images into the superset. *)
  val update_with_mem :
    ?backend:string ->
    ?f:(mem * (Dis.asm, Dis.kinds) Dis.insn option -> t -> t) ->
    t -> mem -> t
  (** Marking an address bad means that it is temporarily maintained
      until a later phase in which it is removed, together with as
      many other other instructions that might have accessed it as
      possible. *)
  val mark_bad : t -> addr -> unit
  (** Internally, performance is important, and it may be the case
      that after many bad instructions are marked and removed, that
      there is some mismatch between the internal tracking that is
      done. So, this library imposes that after a trim that this be
      called. *)
  val rebalance : t -> t
  (** This removes bad entries from the tracker without pruning them
      from the superset. If this is called before trimming the
      superset, then the bad instructions that were marked are no
      longer distinguishable as previously. *)
  val clear_bad : t -> addr -> unit
  val clear_all_bad : t -> unit
  (** Returns a copy of the set of addresses that have been marked
      bad *)
  val copy_bad : t -> Addr.Hash_set.t
  val lookup : t -> addr -> (mem * Dis.full_insn option) option
  val fold : t -> init:'a -> f:(key:addr -> data:elem -> 'a -> 'a) -> 'a
  val mem : t -> addr -> bool
end

module Inspection : sig
  (** Returns if the addr is still in the container representing the 
   superset. Note, an addr may be marked as bad for removal, but isn't 
   removed until the trim module traverses for cleaning. Marking for 
   removal can be reversed by calling clear_bad. *)
  val contains_addr : t -> addr -> bool
  val get_endianness : t -> endian option
  val get_arch : t -> arch
  (** Mark and track the argument address for removal upon trimming. *)
  val num_bad : t -> int
  val count : t -> int
  val count_unbalanced : t -> int
  val unbalanced_diff : t -> (Addr.Set.t * Addr.Set.t)
  val is_bad_at : t -> addr -> bool
  val get_base : t -> addr
  val len_at : t -> addr -> int
  val total_bytes : t -> int
  (** A carefully written function that visits the address in the body 
   of any instruction that is longer than one byte. So, addr + 1, 
   addr + 2, ... addr + n. *)
  val static_successors : t -> mem -> Dis.full_insn option ->
    Brancher.dests
  val get_memmap : t -> value memmap
  val get_main_entry : t -> addr option
  val filename : t -> string option
end

module Metrics : sig
  val record : t -> unit
end

module Occlusion : sig
  val conflicts_within_insns : t -> Addr.Set.t -> Addr.Set.t

  val find_all_conflicts : ?mem:(addr -> bool) -> t -> Addr.Set.t

  val seq_of_addr_range : addr -> int -> addr seq 

  val range_seq : t -> addr seq

  val range_seq_of_conflicts : mem:(addr -> bool) -> addr -> int -> addr seq

  val seq_of_all_conflicts : t -> addr seq

  val conflict_seq_at : t -> addr -> addr seq

  val with_data_of_insn :
    t -> addr -> f:(addr -> unit) -> unit

  val conflicts_within_insn_at :
    t -> ?mem:(addr -> bool) -> ?conflicts:Addr.Set.t -> addr -> Addr.Set.t
end

(** The instruction immediately after argument addr. *)
val fall_through_of : t -> addr -> addr
(** A helper function meant to tuck away the representation 
 underneath that tracks bad addresses. *)
val is_fall_through :
  t -> addr -> addr -> bool

val is_entry : t -> addr -> bool

val entries_of_isg : t -> Addr.Hash_set.t

val mergers : t -> Addr.Set.t

val is_branch : t -> addr -> bool

val get_branches : t -> Addr.Hash_set.t

val check_connected : t -> addr -> addr -> bool

(* TODO move to features *)
val get_non_fall_through_edges :
  t ->
  (addr, addr,
   Addr.Map.Key.comparator_witness)
    Map.t
val get_callsites : ?threshold:int -> t -> 'b Addr.Hash_set.t_

val with_bad :
  t -> ?visited:Addr.Hash_set.t -> pre:('b -> addr -> 'c) ->
  post:('c -> addr -> 'b) -> 'b -> 'b

val iter_component : ?terminator:((addr -> bool)) ->
  ?visited:Addr.Hash_set.t -> ?pre:(addr -> unit) -> ?post:(addr -> unit) ->
  t -> addr -> unit

(** This function starts at a given address and traverses toward 
 every statically visible descendant. It is used to maximally 
 propagate a given function application. For speed, an isg can 
 be provided, in which case the reverse does not have to be computed 
 repeatedly. *)
(* TODO belongs in traverse *)
val with_descendents_at :
  ?visited:'a Addr.Hash_set.t_ ->
  ?post:(addr -> unit) -> ?pre:(addr -> unit) ->
  t -> addr ->  unit

val with_ancestors_at :
  ?visited:'a Addr.Hash_set.t_ ->
  ?post:(addr -> unit) -> ?pre:(addr -> unit)  ->
  t -> addr -> unit

(** From the starting point specified, this reviews all descendants 
 and marks their bodies as bad. For speed, an isg can be provided, 
 in which case the reverse does not have to be computed repeatedly. *)
val mark_descendent_bodies_at :
  ?visited:'a Addr.Hash_set.t_ ->
  ?datas:'b Addr.Hash_set.t_ ->
  t -> addr -> unit

val import : string -> t
val export : string -> t -> unit
val export_addrs : string -> t -> unit

val superset_disasm_of_file :
  ?backend:string ->
  ?f:(mem * (Dis.asm, Dis.kinds) Dis.insn option -> t -> t) ->
  string -> t

