open Core_kernel.Std
open Graphlib.Std
open Bap.Std


(** [disasm ?backend arch mem] performs a shingled disassembly of a
    given memory region and returns a mapping of memory regions to
    corresponding instructions. *)
val disasm :
  ?brancher:brancher ->
  ?backend:string ->
  arch -> mem -> cfg Or_error.t

module With_exn : sig
  val disasm :
    ?brancher:brancher ->
    ?backend:string ->
    arch -> mem -> cfg
end

(** Conservative disassembler  *)
module Conservative : sig
  open Disasm_expert.Basic
  type maybe_insn = mem * (asm, kinds) insn option

  (** [disasm ?backend arch mem] returns an conservative disassembly
      of the given memory region [mem]. The result contains all
      possible disassemblies of the given memory, *)
  val disasm : ?backend:string -> arch -> mem -> maybe_insn seq Or_error.t

  module With_exn : sig
    val disasm : ?backend:string -> arch -> mem -> maybe_insn seq
  end
end
