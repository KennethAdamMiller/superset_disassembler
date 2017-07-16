open Bap.Std
open Core_kernel.Std


(* Unfortunately, I can't build this with functional programming in *)
(* mind, because the ocamlgraph function (fold) required to do so is *)
(* missing from the DFS module. *)
let tag_cross_layer_jmps superset decision_trees =
  let open Superset in
  let insn_map = Superset.get_data superset in
  let with_data_of_insn at ~f =
    let len = match Map.find insn_map at with
      | Some(mem, _) -> Memory.length mem 
      | None -> 0 in
    let opt_data_addrs = Superset_rcfg.seq_of_addr_range 
        at len in
    let () = Seq.iter
        ~f opt_data_addrs in () in
  let add_data_of_insn dataset at = 
    with_data_of_insn at ~f:(Hash_set.add dataset)
  in
  let remove_data_of_insn dataset at =
    with_data_of_insn at ~f:(Hash_set.remove dataset)
  in
  let deltas = ref Addr.Map.empty in
  let delta = ref None in
  let make_deltas decision_tree addr =
    let insns, datas = 
      match !delta with
      | Some (insns, datas) -> (insns, datas) 
      | None ->     
        let insns = Addr.Hash_set.create () in
        let datas = Addr.Hash_set.create () in
        delta := Some(insns, datas);
        insns, datas in
    add_data_of_insn datas addr;
    Hash_set.add insns addr;
    if Superset_rcfg.G.mem_vertex decision_tree addr then (
      deltas := Addr.Map.add !deltas addr (insns, datas);
      delta := None
    )
  in
  let skip _ _ = () in
  delta := None;
  let insns = Addr.Hash_set.create () in
  let datas = Addr.Hash_set.create () in
  List.iter (Map.keys insn_map) ~f:(Hash_set.add insns);
  Seq.iter (Superset_rcfg.range_seq insn_map) ~f:(Hash_set.add datas);
  let module G = Superset_rcfg.G in
  let tag_violators decision_tree addr = 
    (match Map.find !deltas addr with
     | Some (insn_delta, data_delta) -> 
       printf "found delta (%s) at %s of size %d\n" 
         (List.to_string (Hash_set.to_list insn_delta) ~f:Addr.to_string)
         (Addr.to_string addr)
         (Hash_set.length insn_delta);
       delta := Some(insn_delta, data_delta)
     | None -> ());
    match !delta with
    | Some(insn_delta, data_delta) -> 
      Hash_set.iter data_delta ~f:(fun data -> 
          if Map.mem !deltas data then (
            printf "found self conflict between %s and %s; %b\n" 
              (Addr.to_string data)
              (Addr.to_string addr)
              (Superset_rcfg.G.mem_edge superset.insn_rcfg addr data);
            Superset.mark_bad superset addr
          )
        )
    | None -> ()
  in
  let update_sets decision_trees addr =
    match Map.find !deltas addr with
    | Some (insn_delta, data_delta) -> 
      ()
    | None -> ();
      Hash_set.remove insns addr;
      remove_data_of_insn datas addr;
      ()
  in
  let open Superset in
  let insn_rcfg = superset.insn_rcfg in
  Decision_tree_set.visit_all insn_rcfg decision_trees 
    ~pre:skip ~post:make_deltas;
  Decision_tree_set.visit_all insn_rcfg decision_trees 
    ~pre:tag_violators ~post:update_sets

