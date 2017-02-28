open Cmdliner
open Core_kernel.Std
open Metrics

exception Bad_user_input
exception Unknown_arch
exception No_input

type disasmarg   = | Corpora_folder of string
                   | Binary of string
  [@@deriving sexp]

type 'a t = {
  disassembler   : string;
  input_kind     : disasmarg;
  disasm_method  : 'a;
  stats          : string option;
  metrics_format : format_as;
} [@@deriving sexp, fields]

module type Provider = sig
  type kind
  val options : kind t
  (* TODO require program, and provide parse *)
end


let backend = 
  let doc = "" in
  Cmdliner.Arg.(value & opt string "llvm"
                & info ["backend"] ~docv:"Disassembler" ~doc)

let parse_input f = 
  if Sys.file_exists f then (
    if Sys.is_directory f then
      `Ok (Corpora_folder f)
    else 
      `Ok (Binary f)
  ) else `Error "does not exist"
let input_type_printer p
  = function | Binary f | Corpora_folder f -> Format.fprintf p "%s" f
let input_kind = 
  let doc = "Specify target binary or corpora folder." in
  Cmdliner.Arg.(
    required & opt (some (parse_input, input_type_printer)) None
    & info ["target"] ~docv:"Target" ~doc
  )
