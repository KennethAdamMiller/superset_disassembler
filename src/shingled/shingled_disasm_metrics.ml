open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std
open Or_error
open Format
module Dis = Disasm_expert.Basic
module Cfg = Graphs.Cfg

let () = Pervasives.ignore(Plugins.load ())

let shingled_disasm ?cfg ?brancher arch mem =
  Disasm_expert.Basic.with_disasm
    ~backend:"llvm" (Arch.to_string arch) ~f:(fun dis ->
        Shingled_disasm.Conservative.run dis mem >>| fun insns ->
        Shingled_disasm.run ?cfg ?brancher (Shingled_disasm.to_memmap insns) arch mem
      )

let () =
  let img_of_filename filename = 
    let img, errs = Image.create filename |> ok_exn in img in
  let files = Sys.readdir (Sys.getcwd () ^ "/corpora") in
  Array.iter files ~f:(fun f ->
      let img  = img_of_filename (Sys.getcwd () ^ "/corpora/" ^ f) in
      let arch = Image.arch img in
      let cfg  = Memmap.to_sequence (Image.memory img)
                 |> Seq.fold ~init:None ~f:(fun cfg (mem,_) -> 
                     Some (shingled_disasm ?cfg arch mem |> ok_exn))
                 |> Option.value ~default:Cfg.empty
      in
      print_endline ("Total instructions recovered: "
                     ^ (string_of_int (Seq.length @@ Cfg.nodes cfg)));
      print_endline ("Total instructions sheered: "
                     ^ (string_of_int (Seq.length @@ Cfg.nodes cfg)));
      ())
