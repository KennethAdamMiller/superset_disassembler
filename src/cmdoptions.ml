open Cmdliner
open Core_kernel.Std
open Metrics

exception Bad_user_input
exception Unknown_arch
exception No_input

type phases = 
  | Insn_invariants
  | Invalid_memory_access
  | Non_instruction
  | Cross_layer_invalidation
  | Component_body

type content =
  | Cfg
  | Insn_map
[@@deriving sexp]

type checkpoint = 
  | Import
  | Export
[@@deriving sexp]

type 'a t = {
  checkpoint     : checkpoint option;
  content        : content list option;
  disassembler   : string;
  ground_truth   : string option;
  target         : string;
  disasm_method  : 'a;
  metrics_format : format_as;
  phases         : string list option;
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
  Cmdliner.Arg.(value & opt (some (list (enum possible_content))) None
                & info ["output"] ~docv:"" ~doc)

let list_checkpoints = [
  "Import", Import;
  "Export", Export;
]
let checkpoint = 
  let doc = "Import or Export the disassembly graph and map." in
  Cmdliner.Arg.(
    value & opt (some (enum list_checkpoints)) None & info ["checkpoint"] ~doc
  )

let parse_input f = 
  if Sys.file_exists f then (
    `Ok (f)
  ) else `Error "does not exist"
let input_type_printer p
  = Format.fprintf p "%s"
let target = 
  let doc = "Specify target binary or corpora folder." in
  Cmdliner.Arg.(
    required & opt (some (parse_input, input_type_printer)) None
    & info ["target"] ~docv:"Target" ~doc
  )


let list_phases = [
  "All Instruction invariants", Insn_invariants;
  "Invalid memory accesses", Invalid_memory_access;
  "Non instruction opcode", Non_instruction;
  "Cross Layer Invalidation", Cross_layer_invalidation;
  "Strongly Connected Component Data", Component_body;
]
let phases_doc = sprintf "Select from the following trim phase(s): %s"
    List.(to_string ~f:fst list_phases)
let phases =
  let doc = "Specify the desired trim phases to run." in
  Cmdliner.Arg.(
    value & opt (some (list string)) None
    & info ["phases"] ~docv:phases_doc ~doc
  )
