open Bap.Std
open Core_kernel.Std

let img_of_filename filename = 
  let img, errs = Image.create filename |> ok_exn in img

let create_memory arch min_addr data =
  let data = Bigstring.of_string data in
  Memory.create (Arch.endian arch) min_addr data

let process_corpora ?corpdir ~backend process =
  let corpdir = Option.value corpdir
      ~default:(Sys.getcwd () ^ "/corpora/") in
  let files = Sys.readdir corpdir in
  Array.fold ~init:None files ~f:(fun accu f ->
      process accu backend (corpdir ^ f)
    )
