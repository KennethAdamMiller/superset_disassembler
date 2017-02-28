open Bap.Std
open Core_kernel.Std
open Insn_cfg

type format_as   = | Latex
                   | Standard
[@@deriving sexp]

type metrics = {
  detected_insn_count : int;
  false_negatives     : int;
  false_positives     : int;
  extenuating_loss    : int option;
  total_actual_insns  : int option;
}

let format_standard metrics =
  match metrics with 
  | Some metrics -> 
    sprintf "Total instructions recovered: %d\n"
      metrics.detected_insn_count
  | None -> sprintf "No metrics gathered!"

let format_latex metrics = ""


(* TODO remove  *)
let read arch ic : (string * addr * addr) list =
  let sym_of_sexp x = [%of_sexp:string * int64 * int64] x in
  let addr_of_int64 x =
    let width = Arch.addr_size arch |> Size.in_bits in
    Addr.of_int64 ~width x in
  List.(Sexp.input_sexps ic >>| sym_of_sexp >>| (fun (s, es, ef) ->
      s, addr_of_int64 es, addr_of_int64 ef))

let read_addrs ic : addr list =
  try List.t_of_sexp Addr.t_of_sexp @@ Sexp.input_sexp ic
  with End_of_file -> []

let from_unstripped_bin bin : addr seq Or_error.t =
  let open Or_error in
  let tmp = Filename.temp_file "bw_" ".symbol" in
  let cmd = sprintf "bap-byteweight dump -i %s %S > %S" "symbols"
      bin tmp in
  if Sys.command cmd = 0
  then return (Seq.of_list @@ In_channel.with_file tmp ~f:read_addrs)
  else errorf
      "failed to fetch symbols from unstripped binary, command `%s'
  failed" cmd
(* ****** *)

let gather_metrics bin metrics cfg =
  let syms = from_unstripped_bin bin |> ok_exn in
  let (ground_truth, total_actual_insns) = Seq.fold ~init:(Addr.Set.empty, 0)
      syms
      ~f:(fun (all, total) tp -> Set.add all tp,total+1) in
  let detected_insns = 
    G.fold_vertex 
      (fun vert detected_insns -> Set.add detected_insns vert) 
      cfg Addr.Set.empty in
  let false_negatives =
    Set.length (Set.diff ground_truth detected_insns) in
  let false_positives =
    Set.length (Set.diff detected_insns ground_truth) in
  let detected_insn_count = G.nb_vertex cfg in
  let extenuating_loss = None in
  let total_actual_insns = Some (total_actual_insns) in
  match metrics with 
  | Some metrics -> 
    Some ({
        detected_insn_count = 
          detected_insn_count + 
          metrics.detected_insn_count;
        false_positives=false_positives + 
                        metrics.false_positives;
        false_negatives=false_negatives + 
                        metrics.false_negatives;
        extenuating_loss;
        total_actual_insns;
      })
  | None -> Some ({
      detected_insn_count;
      false_positives;
      false_negatives;
      extenuating_loss;
      total_actual_insns;
    })


module Opts = struct 
  open Cmdliner

  let list_content_doc = sprintf
      "Metrics may be collected against a symbol file"
  let content_type = 
    Arg.(value &
         opt (some string) (None)
         & info ["metrics_data"] ~doc:list_content_doc)

  let list_formats_types = [
    "standard", Standard;
    "latex", Latex;
  ]
  let list_formats_doc = sprintf
      "Available output metrics formats: %s" @@ 
    Arg.doc_alts_enum list_formats_types
  let metrics_format = 
    Arg.(value & opt (enum list_formats_types) Standard
         & info ["metrics_format"] ~doc:list_formats_doc)

end
