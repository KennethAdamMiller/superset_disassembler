open Bap.Std
open Core_kernel.Std

let create_memory arch min_addr data =
  let data = Bigstring.of_string data in
  Memory.create (Arch.endian arch) min_addr data
