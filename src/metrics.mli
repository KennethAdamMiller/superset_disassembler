open Core_kernel
open Bap.Std

type metrics
type format_as = | Latex
                 | Standard
[@@deriving sexp]
val format_standard : metrics option -> string
val format_latex : metrics option -> string

val make_mark_tracker :
  Addr.Set.t Addr.Map.t ref -> Superset.t -> Superset.t
val make_gatherer :
  Addr.Hash_set.t -> Superset.t -> Superset.t

(** Given a file with debug information, use the byteweight
    symbolizer to retrieve the set of declared entrances. *)
val ground_truth_of_unstripped_bin :
  string -> addr seq Or_error.t

(** Given a superset, and a string to a file with debug information,
    expand the set of addresses to include all reachable instances *)
val true_positives :
  Superset.t -> string -> Addr.Hash_set.t

(** Given a file location to a debug binary and a superset, collect
    metrics on the disassembly. *)
val gather_metrics :
  bin:string -> Superset.t -> metrics option
  
module Opts : sig
  (** Allow to specify a symbol file or binary *)
  val content_type : string option Cmdliner.Term.t
  (** Output the metrics either in human readable form or to a latex
      table *)
  val metrics_fmt : format_as Cmdliner.Term.t
end
