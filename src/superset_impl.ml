open Bap.Std
open Regular.Std
open Core_kernel
open Or_error

module Dis = Disasm_expert.Basic

type elem = mem * (Dis.full_insn option)

type t = {
  arch        : arch;
  filename    : string option;
  main_entry  : addr option;

  sections    : value memmap;
  brancher    : Brancher.t;
  endianness  : endian option;
  lifter      : lifter;
  balanced    : bool;
  (* TODO registerable per-feature info, perhaps using the polymorphic
     data??  *)
  (* Image.words functionality *)
  (* TODO: needs to become an hash map *)
  insn_map    : (mem * (Dis.full_insn option)) Addr.Map.t;
  insn_risg   : Superset_risg.t;
  bad         : Addr.Hash_set.t;
  keep        : Addr.Hash_set.t;
  (* marked data  *)
  (* visited *)
  (* union_find *)
}


let of_components
    ?main_entry ?insn_map ?insn_risg ?segments ?endianness ?filename arch =
  let insn_risg =
    match insn_risg with
    | Some insn_risg -> insn_risg
    | None -> Superset_risg.G.create () in
  let segments = Option.value segments ~default:Memmap.empty in
  let insn_map  = Option.value insn_map ~default:Addr.Map.empty in
  let balanced =
    Map.(length insn_map) = (Superset_risg.G.nb_vertex insn_risg) in
  let module Target = (val target_of_arch arch) in
  let lifter = Target.lift in
  {
    arch        = arch;
    filename;
    sections    = segments;
    brancher    = Brancher.of_bil arch;
    endianness  = None;
    lifter      = lifter;
    main_entry;
    balanced;
    insn_map;
    insn_risg;
    bad         = Addr.Hash_set.create ();
    keep        = Addr.Hash_set.create ();
  }
