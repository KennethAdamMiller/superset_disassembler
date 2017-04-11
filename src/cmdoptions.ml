open Cmdliner
open Core_kernel.Std
open Metrics

exception Bad_user_input
exception Unknown_arch
exception No_input

type disasmarg   = 
  | Corpora_folder of string
  | Binary of string
[@@deriving sexp]

type content =
  | Cfg
  | Insn_map
[@@deriving sexp]

type 'a t = {
  content        : content list option;
  disassembler   : string;
  ground_truth   : string option;
  input_kind     : disasmarg;
  disasm_method  : 'a;
  metrics_format : format_as;
} [@@deriving sexp, fields]

module type Provider = sig
  type kind
  val options : kind t
end


let backend = 
  let doc = "The particular backend disassembler to use; llvm/IDA/ect" in
  Cmdliner.Arg.(value & opt string "llvm"
                & info ["backend"] ~docv:"Disassembler" ~doc)


let possible_content = [
  "Cfg", Cfg;
  "Insn_map" , Insn_map;
]
let possible_content_doc = sprintf
    "Select of the the following disassembly methods: %s" @@ 
  Cmdliner.Arg.doc_alts_enum possible_content

let content = 
  let doc =
    "The analysis output desired; one of cfg/insn map/decision tree"
  in
  Cmdliner.Arg.(value & opt (some (list (enum possible_content))) (None)
                & info ["output"] ~docv:"" ~doc)

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
