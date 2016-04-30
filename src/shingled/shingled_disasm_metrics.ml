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
  let files = Sys.readdir (Sys.getcwd () ^ "/corpora/") in
  Array.iter files ~f:(fun f ->
      let img  = img_of_filename (Sys.getcwd () ^ "/corpora/" ^ f) in
      let arch = Image.arch img in
      let cfg = Memmap.to_sequence (Image.memory img)
                |> Seq.fold ~init:None ~f:(fun accu (mem,_) ->
                    let superset_cfg = match accu with 
                      | Some (superset_cfg) ->
                        superset_cfg
                      | None -> Shingled.G.create () in
                    Some (Shingled.superset_cfg_of
                            ~superset_cfg arch mem |> ok_exn)) |> 
                function | Some (cfg) -> cfg
                         | None -> Shingled.G.create ()
      in
      let total_insn_count = Shingled.G.nb_vertex cfg in
      print_endline ("Total instructions recovered: "
                     ^ (string_of_int total_insn_count));
      let sheered = Shingled.sheer cfg arch in
      print_endline ("Total sheered: "
                     ^ (string_of_int (total_insn_count
                                       - (Shingled.G.nb_vertex
                                            sheered))));
      print_endline ("Final insn count: " ^ 
                     (string_of_int @@ Shingled.G.nb_vertex
                        sheered));
    )
