open Core_kernel
open Bap.Std

let () = Random.self_init ()

type map_report = {
  tp_to_tp                 : int;
  tp_to_fp                 : int;
  fp_to_tp                 : int;
  fp_to_fp                 : int;
} [@@deriving sexp]

module Distribution = struct
  type t = {
      (* The ith elem says the number (int) of insns with i
       accumulated total feature instances *)
      fp_at             : int array;
      tp_at             : int array;
      (* The ith list elem (x, y list) says the number (x) of true
       positives that have i many features in their favor and how
       many false positives (y) they occlude with j number of features. *)
      fp_competitors_at : (int * int array) array;
    } [@@deriving sexp]

  let empty threshold =
    let len = threshold + 1 in
    {
      fp_at             = Array.create ~len 0;
      tp_at             = Array.create ~len 0;
      fp_competitors_at = Array.create ~len (0,Array.create ~len 0);
    }

  let add dist tps inst =
    let (p,addr,feature) = inst in
    let p = min p @@ ((Array.length dist.fp_at) - 1) in
    let insert c =
      let cur = Array.get c p  in
      Array.set c p (cur+1) in
    let _ = 
      if Hash_set.mem tps addr then
        insert dist.tp_at
      else insert dist.fp_at in
    dist
end
   
type report = {
  actual_tp_cases          : int;
  outbound_cases           : int;
  outbound_inflicted_fn    : int;
  raw_cases                : int;
  total_cases_identified   : int;
  overlap_space            : int;
  overlap_max              : int;
  overlap_min              : int;
  analysis_fp              : int;
  analysis_cleansed        : int;
  analysis_fn              : int;
  tp_cases_reported        : int;
  fp_cases_reported        : int;
  extp                     : addr option;
  exfp                     : addr option;
  map_details              : map_report option;
} [@@deriving sexp]

let format_map_details md = 
  match md with
  | None -> "None"
  | Some(md) -> sprintf "tptp %d tpfp %d fptp %d fpfp %d"
                  md.tp_to_tp md.tp_to_fp md.fp_to_tp md.fp_to_fp

let format_report r =
  sprintf "\tReport: \n%s %d \n%s%d\n%s%d\n%s%d\n%s%d\n%s%d\n%s%d\n%s %d\n%s %s\n%s%d\n%s%d\n%s%d\n%s%s\n%s%s\n%s\n\n"
    "actual tp cases: " r.actual_tp_cases
    "raw_cases: " r.raw_cases
    "total identified: " r.total_cases_identified
    "overlap: " r.overlap_space
    "tp reported: " r.tp_cases_reported
    "fp reported: " r.fp_cases_reported
    "outbound cases: " r.outbound_cases
    "outbound inflicted fn: " r.outbound_inflicted_fn
    "map results: " (format_map_details r.map_details)
    "analysis removed: " r.analysis_cleansed
    "analysis fn: " r.analysis_fn
    "analysis fp: " r.analysis_fp
    "extp: " Option.(value_map r.extp ~f:Addr.to_string ~default:"None")
    "exfp: " Option.(value_map r.exfp ~f:Addr.to_string ~default:"None")
    "================="

(* TODO duplicates in other places *)
(* TODO belongs in fixpoint, probably named as converge *)
let mark_threshold_with_pmap ?visited ?datas superset pmap threshold = 
  let visited = Option.value visited 
      ~default:(Addr.Hash_set.create ()) in
  let datas = Option.value datas
      ~default:(Addr.Hash_set.create ()) in
  Map.iteri pmap ~f:(fun ~key ~data ->
      let addr = key in
      let (p,_,_) = data in
      if (p > threshold) then (
        if Superset.Core.mem superset addr then
          Traverse.mark_descendent_bodies_at
            ~datas ~visited superset addr;
      )
    )
  
let collect_set_report
    (superset : 's) (extractf : 's -> 'e) (filterf : 's -> 'e -> 'e) tps ro fps pmap = 
  let extracted = extractf superset in
  let actual_tp_cases = 
    Addr.Set.fold ~init:0 extracted ~f:(fun actual e -> 
        if Hash_set.mem tps e then actual+1 else actual
      ) in
  let raw_cases = Addr.Set.length extracted in
  let filtered = filterf superset extracted in
  let tp_cases_reported, fp_cases_reported = 
    Addr.Set.fold ~init:(0,0) filtered ~f:(fun (tpr, fpr) e -> 
        let t = if Hash_set.mem tps e then 1 else 0 in
        let f = if Hash_set.mem fps e then 1 else 0 in
        (tpr + t, fpr + f)
      ) in
  let outbound_cases = 
    Addr.Set.fold ~init:0 filtered ~f:(fun outbound e -> 
        if not Hash_set.(mem tps e) && not Hash_set.(mem fps e) 
        then outbound + 1 else outbound
      ) in
  let visited = Addr.Hash_set.create () in
  let outbound = Addr.Hash_set.create () in
  Addr.Set.iter filtered 
    ~f:(fun e -> 
        if not Hash_set.(mem tps e) && not Hash_set.(mem fps e) then (
          if not (Hash_set.mem visited e) && 
             Superset.Core.mem superset e then
            Traverse.with_descendents_at ~pre:(fun tp -> 
                let mark_bad addr = 
                  if Superset.Core.mem superset addr then
                    Superset.Core.mark_bad superset addr in
                Superset.Occlusion.with_data_of_insn
                  superset tp ~f:mark_bad;
                Superset.Occlusion.with_data_of_insn
                  superset tp ~f:(Hash_set.add outbound);
                Hash_set.add visited tp;
              ) ?post:None superset e
        )
      );
  let outbound_inflicted_fn =
    Hash_set.fold ~init:0 outbound ~f:(fun inflicted o -> 
        if Hash_set.mem tps o then inflicted+1 else inflicted
      ) in
  let mx = max (*Float.max in*) in
  let mn = min (*Float.min in*) in
  let (tp_max, tp_min, fp_max, fp_min) = Map.fold pmap ~init:(0, 0, 0, 0) 
      ~f:(fun ~key ~data (tp_max, tp_min, fp_max, fp_min) -> 
          let (data,_,_) = data in
          let tp_max, tp_min = 
            if Hash_set.mem tps key then
              mx tp_max data, mn tp_min data
            else tp_max, tp_min in
          let fp_max, fp_min = 
            if Hash_set.mem fps key then
              mx fp_max data, mn fp_min data
            else fp_max, fp_min in
          tp_max, tp_min, fp_max, fp_min
        ) in
  let overlap_max =
    if (tp_min > fp_max) then tp_min 
    else if (fp_min > tp_max) then fp_min 
    else mx tp_min fp_min in
  let overlap_min =
    if (tp_min < fp_max) then tp_min
    else if (fp_min < tp_max) then fp_min 
    else mn tp_min fp_min in
  let overlap_space = overlap_max - overlap_min in
  let visited = Addr.Hash_set.create () in
  let datas = Addr.Hash_set.create () in
  (* TODO a hardcoded threshold has been used *)
  (* report collection should not be trimming, but only traversing *)
  mark_threshold_with_pmap ~visited ~datas superset pmap 50;
  let removed = Superset.Core.copy_bad superset in
  let analysis_cleansed = Hash_set.length removed in
  let (analysis_fn,analysis_fp) =
    Hash_set.fold removed ~init:(0,0) ~f:(fun (fn,fp) addr -> 
        let fn = if Hash_set.mem tps addr then fn+1 else fn in
        let fp = if Hash_set.mem fps addr then fp+1 else fp in
        fn,fp
      ) in
  let pick_addr s = 
    let s = Hash_set.to_array s in 
    let len = Array.(length s) in
    if len = 0 then None else Some(Array.get s Random.(int len))
  in
  let inter s1 s2 = 
    let r = Addr.Hash_set.create () in
    Hash_set.iter s1 ~f:(fun x -> 
        if Hash_set.mem s2 x then Hash_set.add r x
      ); r in
  let filtered = 
    let r = Addr.Hash_set.create () in
    Set.iter filtered ~f:Hash_set.(add r);
    r in
  let extp = pick_addr (inter tps filtered) in
  let exfp = pick_addr (inter fps filtered) in
  Superset.Core.clear_all_bad superset;
  let total_cases_identified = Hash_set.length filtered in
  {
    actual_tp_cases;
    outbound_cases;
    outbound_inflicted_fn;
    raw_cases;
    total_cases_identified;
    overlap_space;
    overlap_max;
    overlap_min;
    analysis_cleansed;
    analysis_fp;
    analysis_fn;
    extp;
    exfp;
    tp_cases_reported;
    fp_cases_reported;
    map_details= None;
  }


let collect_map_report
    (superset : 's) (extractf : 's -> 'e) (filterf : 's -> 'e -> 'e) tps ro fps pmap =
  let m = extractf superset in
  let set_extractf superset = 
    Map.fold ~init:Addr.Set.empty m ~f:(fun ~key ~data s -> 
        Set.add Set.(add s key) data) in
  let r = 
    collect_set_report superset set_extractf (fun _ x -> x) tps ro fps pmap in
  let (tptp,tpfp,fptp,fpfp) = Map.fold ~init:(0,0,0,0) m
      ~f:(fun ~key ~data (tptp,tpfp,fptp,fpfp) ->
          if Hash_set.mem tps key then
            if Hash_set.mem tps data then
              (tptp+1,tpfp,fptp,fpfp)
            else if Hash_set.mem fps data then
              (tptp,tpfp+1,fptp,fpfp)
            else
              (tptp,tpfp,fptp,fpfp)
          else if Hash_set.mem fps key then
            if Hash_set.mem tps data then
              (tptp,tpfp,fptp+1,fpfp)
            else if Hash_set.mem fps data then
              (tptp,tpfp,fptp,fpfp+1)
            else (tptp,tpfp,fptp,fpfp)
          else (tptp,tpfp,fptp,fpfp)
        ) in
  let map_details = Some {
      tp_to_tp = tptp;
      tp_to_fp = tpfp;
      fp_to_tp = fptp;
      fp_to_fp = fpfp;
    } in
  {
    actual_tp_cases = r.actual_tp_cases;
    outbound_cases = r.outbound_cases;
    outbound_inflicted_fn = r.outbound_inflicted_fn;
    raw_cases = r.raw_cases;
    total_cases_identified = r.total_cases_identified;
    overlap_space = r.overlap_space;
    overlap_max = r.overlap_max;
    overlap_min = r.overlap_min;
    analysis_cleansed = r.analysis_cleansed;
    analysis_fp = r.analysis_fp;
    analysis_fn = r.analysis_fn;
    tp_cases_reported = r.tp_cases_reported;
    fp_cases_reported = r.fp_cases_reported;
    extp  = r.extp;
    exfp  = r.exfp;
    map_details;
  }
    
