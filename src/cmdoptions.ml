open Cmdliner
open Core_kernel.Std
open Metrics

exception Bad_user_input
exception Unknown_arch
exception No_input

type phase = 
  | Default
  | Target_not_in_memory
  | Target_within_body
  | Invalid_memory_access
  | Non_instruction
  | Component_body
  | Cross_layer_invalidation
  | Grammar_convergent
  | Tree_set
[@@deriving sexp]

type checkpoint = 
  | Import
  | Export
  | Update
[@@deriving sexp]

type t = {
  checkpoint     : checkpoint option;
  disassembler   : string;
  ground_truth   : string option;
  target         : string;
  metrics_format : format_as;
  phases         : phase list option;
} [@@deriving sexp, fields]

module type Provider = sig
  val options : t
end


let backend = 
  let doc = "The particular backend disassembler to use; llvm/IDA/ect" in
  Cmdliner.Arg.(value & opt string "llvm"
                & info ["backend"] ~docv:"Disassembler" ~doc)

let list_checkpoints = [
  "Import", Import;
  "Export", Export;
  "Update", Update;
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
  "All Instruction invariants", Default;
  "Target_out_of_bounds", Target_not_in_memory;
  "Invalid memory accesses", Invalid_memory_access;
  "Target_within_body", Target_within_body;
  "Non instruction opcode", Non_instruction;
  "Strongly Connected Component Data", Component_body;
  "Cross Layer Invalidation", Cross_layer_invalidation;
  "Grammar convergent", Grammar_convergent;
]
let phases_doc = sprintf "Select from the following trim phase(s): %s"
    List.(to_string ~f:fst list_phases)
let phases =
  let doc = "Specify the desired trim phases to run." in
  Cmdliner.Arg.(
    value & opt (some (list (enum list_phases))) (Some([Default]))
    & info ["phases"] ~docv:phases_doc ~doc
  )
