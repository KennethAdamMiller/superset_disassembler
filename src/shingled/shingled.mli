open Bap.Std
open Core_kernel.Std

type t = Insn_cfg.t
val rcfg_of_superset :
  ?superset_cfg:t -> ?brancher:Brancher.t -> Superset.t -> mem -> arch -> t

val trim : Superset.maybe_insn Addr.Map.t -> t -> arch
  -> (Superset.maybe_insn Addr.Map.t * t)

val superset_to_map : Superset.maybe_insn list ->
  Superset.maybe_insn Addr.Map.t -> t -> Superset.maybe_insn Addr.Map.t

val superset_cfg_of_mem : ?insn_map:(Superset.maybe_insn Addr.Map.t) ->
  ?superset_cfg:t -> ?brancher:Brancher.t -> ?backend:string ->
  arch -> mem ->  (Superset.maybe_insn Addr.Map.t * t) Or_error.t

val disasm : ?superset_cfg:t -> ?brancher:Brancher.t -> ?backend:string
  -> arch -> mem -> (Superset.maybe_insn Addr.Map.t * t) Or_error.t

module With_exn : sig
  val disasm : ?brancher:Brancher.t -> ?backend:string -> arch -> mem ->
    Superset.maybe_insn Addr.Map.t * t
end

val with_img :  accu:'a -> backend:string -> Image.t -> 
  f:(accu:'a -> backend:string -> Bap.Std.arch -> Bap.Std.mem ->
     'a Or_error.t) -> 'a

val superset_disasm_of_img : ?superset_cfg:t -> backend:string -> Image.t ->
  Superset.maybe_insn Addr.Map.t * t

val superset_disasm_of_file : backend:string -> Image.path -> 
  (arch * Superset.maybe_insn Addr.Map.t * t)
val trimmed_disasm_of_file : backend:string -> Image.path -> 
  (arch * Superset.maybe_insn Addr.Map.t * t)
