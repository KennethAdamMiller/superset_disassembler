open Bap.Std
open Core_kernel.Std

type t = Insn_cfg.t
val cfg_of_shingles :
  ?superset_cfg:t -> ?brancher:Brancher.t -> Superset.t -> mem -> arch -> t

val sheer : t -> arch -> t

val shingled_cfg_of_mem : ?superset_cfg:t -> ?brancher:Brancher.t -> ?backend:string ->
  arch -> mem -> t Or_error.t

val disasm : ?superset_cfg:t -> ?brancher:Brancher.t -> ?backend:string
  -> arch -> mem -> t Or_error.t

module With_exn : sig
  val disasm : ?brancher:Brancher.t -> ?backend:string -> arch -> mem -> t
end

val with_img :  accu:'a -> backend:string -> Image.t -> 
  f:(accu:'a -> backend:string -> Bap.Std.arch -> Bap.Std.mem ->
     'a Or_error.t) -> 'a

val shingled_cfg_of_img : ?superset_cfg:t -> backend:string -> Image.t -> t

val shingled_cfg_of_file : backend:string -> Image.path -> (arch * t)
val sheered_cfg_of_file : backend:string -> Image.path -> (arch * t)

val shingled_to_map : (mem * 'a) list -> (mem * 'a) Addr.Map.t -> 
  (mem * 'a) Addr.Map.t

