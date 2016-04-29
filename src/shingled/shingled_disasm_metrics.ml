open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std
open Or_error
open Format
module Dis = Disasm_expert.Basic
module Cfg = Graphs.Cfg

let () = Pervasives.ignore(Plugins.load ())



let () =
  let img_of_filename filename = 
    let img, errs = Image.create filename |> ok_exn in img in
  let files = Sys.readdir (Sys.getcwd () ^ "/corpora") in
  Array.iter files ~f:(fun f ->
      let img  = img_of_filename (Sys.getcwd () ^ "/corpora/" ^ f) in
      let arch = Image.arch img in
      let cfg = Memmap.to_sequence (Image.memory img)
                |> Seq.fold ~init:None ~f:(fun accu (mem,_) ->
                    match accu with 
                    | Some (bad, superset_cfg) ->
                      Some (Shingled_disasm.superset_of
                              ~superset_cfg arch mem |> ok_exn)
                    | None -> None) |> 
                function | Some (_, cfg) -> cfg
                         | None -> Shingled_disasm.G.empty
      in
      let total_insn_count = Shingled_disasm.G.nb_vertex cfg in
      print_endline ("Total instructions recovered: "
                     ^ (string_of_int total_insn_count));
      let sheered = Shingled_disasm.sheer cfg arch in
      print_endline ("Total instructions recovered: "
                     ^ (string_of_int (total_insn_count - (Shingled_disasm.G.nb_vertex cfg))))
    )
