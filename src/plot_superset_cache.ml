open Core_kernel
open Bap.Std
open Regular.Std
open Bap_knowledge
open Bap_core_theory
open Monads.Std
open Owl_plplot

   
let () = match Bap_main.init () with
  | Ok () -> ()
  | Error err -> 
     let open Bap_main in
     Bap_main.Extension.Error.pp Format.std_formatter err;
     exit 1
   
let mat_of_list l =
  let num = List.length l in
  let d = Owl_dense_matrix.D.create num 1 0.0 in
  List.iteri l ~f:(fun idx e ->
      Owl_dense_matrix.D.set d idx 1 (float_of_int e)
    );
  d
  
(* Plots:
   binary size to occlusive rate (occlusion by occ space)
   occlusive space to occlusion
   scatter plot occlusive count and number of occ functions
   size and processing time
   least value required for safe convergence
   number of binaries and occ rate
 *)
let make_plots summaries =
  let open Metrics in
  let summaries =
    List.filter_map summaries ~f:(fun s ->
        match s.size, s.occ, s.occ_space, s.fe, s.clean, s.fns, s.fps,
              s.tps, s.time with
        | None, _, _, _, _, _, _, _, _ -> None
        | _, None, _, _, _, _, _, _, _ -> None
        | _, _, None, _, _, _, _, _, _ -> None
        | _, _, _, None, _, _, _, _, _ -> None
        | _, _, _, _, None, _, _, _, _ -> None
        | _, _, _, _, _, None, _, _, _ -> None
        | _, _, _, _, _, _, None, _, _ -> None
        | _, _, _, _, _, _, _, None, _ -> None
        | _, _, _, _, _, _, _, _, None -> None
        | Some size, Some occ, Some occ_space, Some fe, Some clean,
          Some fns, Some fps, Some tps, Some time ->
           Some (size, occ, occ_space, fe, clean, fns, fps, tps, time)
      ) in
  let s = sprintf "Have %d summmaries" List.(length summaries) in
  print_endline s;
  let sz_occ = Plot.create "size_and_occlusion.png" in
  let occ_occspace = Plot.create "occlusion_and_occspace.png" in
  let occcnt_occfuncs  = Plot.create "occcnt_occfuncs.png" in
  let size_time = Plot.create "size_time.png" in
  let safe_conv = Plot.create "safe_conv.png" in
  let occr_numbins = Plot.create "occr_numbins.png" in
  let sizes,occ,occ_space,fe,clean,fns,fps,tps,time =
    List.fold summaries ~init:([],[],[],[],[],[],[],[],[])
      ~f:(fun (sizes,occ,occ_space,fe,clean,fns,fps,tps,time) s ->
        let _size,_occ,_occ_space,_fe,_clean,_fns,_fps,_tps,_time = s in
        _size :: sizes, _occ :: occ, _occ_space :: occ_space,
        _fe :: fe,_clean :: clean,_fns :: fns,_fps :: fps,_tps :: tps,
        _time :: time
      ) in
  Plot.scatter ~h:sz_occ (mat_of_list sizes) (mat_of_list occ);
  Plot.output sz_occ;
  Plot.scatter ~h:occ_occspace (mat_of_list occ)
    (mat_of_list occ_space);
  Plot.output occ_occspace;
  match List.map2 fe clean (fun x y -> x - y) with
  | List.Or_unequal_lengths.Ok occfuncs ->
     Plot.scatter ~h:occcnt_occfuncs (mat_of_list occ)
       (mat_of_list occfuncs);
     Plot.output occcnt_occfuncs;
  | _ -> ();
  Plot.scatter ~h:size_time (mat_of_list sizes) (mat_of_list time);
  Plot.output size_time;
  Plot.scatter ~h:occr_numbins (mat_of_list sizes) (mat_of_list time);
  Plot.output occr_numbins
  

(* TODO plot cache, show_cache *)
let () = 
  let summaries =
    Metadata.with_digests Metadata.cache_corpus_metrics in
  make_plots summaries;
