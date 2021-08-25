open Core_kernel
open Bap.Std

(* TODO remove in favor of knowledge base *)
val ground_truth_of_unstripped_bin : string -> addr seq Or_error.t

(* TODO remove in favor of knowledge base *)
val true_positives_of_ground_truth : Superset.t -> Addr.Set.t -> Addr.Hash_set.t

(* TODO remove in favor of knowledge base *)
val true_positives : Superset.t -> string -> Addr.Hash_set.t
  
module Cache : sig
  open Bap_knowledge
  open Bap_core_theory
  open Theory

  val ground_truth_source : (program, string) Knowledge.slot

  val function_entrances : (program, Addr.Set.t option) Knowledge.slot

  val ground_truth : (program, Addr.Set.t option) Knowledge.slot

  val occlusive_space : (program, int option) Knowledge.slot

  val reduced_occlusion : (program, int option) Knowledge.slot

  val false_negatives : (program, int option) Knowledge.slot

  val false_positives : (program, int option) Knowledge.slot

  val true_positives : (program, int option) Knowledge.slot

  val clean_functions : (program, Addr.Set.t option) Knowledge.slot
    
end

(** Given a file location to a debug binary and a superset, collect
    metrics on the disassembly. *)
val compute_metrics : Superset.t -> unit
