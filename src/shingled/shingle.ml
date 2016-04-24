open Bap.Std
open Disasm_expert.Basic

type maybe_insn = mem * (asm, kinds) insn option
type shingle = mem * (asm, kinds) insn
