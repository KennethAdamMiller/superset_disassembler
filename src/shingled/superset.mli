open Core_kernel.Std
open Bap.Std
module Dis = Disasm_expert.Basic

open Dis

type maybe_insn = (mem * (Dis.asm, Dis.kinds) Dis.insn option)
type maybe_full_insn = (mem * Dis.full_insn option)
type t = maybe_insn list
type t_full = maybe_full_insn list

val disasm : ?backend:string -> accu:'a -> f:(maybe_insn -> 'a -> 'a) ->
  arch -> mem -> 'a Or_error.t

val memmap_all : ?backend:string -> arch -> mem
  -> (Dis.asm, Dis.kinds) Dis.insn memmap

val lift : arch -> t_full -> (bil * int) Addr.Map.t

module With_exn : sig
  val disasm : ?backend:string -> accu:'a -> f:(maybe_insn -> 'a -> 'a)
    -> arch -> mem  -> 'a
end

type indirections = (addr * edge) list Addr.Table.t
val all_indirections : Brancher.t -> t -> indirections

type barriers = addr Hash_set.t
val barriers_of_dests : indirections -> barriers
