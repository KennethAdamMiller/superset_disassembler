open Bap.Std
open Core_kernel.Std

type t = Insn_cfg.t
val cfg_of_shingles :
  ?superset_cfg:t -> ?brancher:Brancher.t -> Superset.t -> mem -> arch -> t

val sheer : Superset.maybe_insn Addr.Map.t -> t -> arch
  -> (Superset.maybe_insn Addr.Map.t * t)

val shingled_to_map : Superset.maybe_insn list ->
  Superset.maybe_insn Addr.Map.t -> t -> Superset.maybe_insn Addr.Map.t

val shingled_cfg_of_mem : ?insn_map:(Superset.maybe_insn Addr.Map.t) ->
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

val shingled_cfg_of_img : ?superset_cfg:t -> backend:string -> Image.t ->
  Superset.maybe_insn Addr.Map.t * t

val shingled_cfg_of_file : backend:string -> Image.path -> 
  (arch * Superset.maybe_insn Addr.Map.t * t)
val sheered_cfg_of_file : backend:string -> Image.path -> 
  (arch * Superset.maybe_insn Addr.Map.t * t)
