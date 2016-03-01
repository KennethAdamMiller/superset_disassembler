open Core_kernel.Std
open Regular.Std
open Bap.Std
open Bigarray

type t
type corpus


module Corpus : sig
  type t = corpus with bin_io, sexp
  val init  : cfg -> t
  val update : t -> cfg -> t
  include Data with type t := t
end

(** [create corpus] estimates transition probabilities based on
    training data [corpus] *)

val estimate : corpus -> t

(** [probability t insn succ] returns an emperical probability that
    [insn] is followed by [Some dest = succ], or by data when [succ] is
    [None]. If [insn] has never occured in the training corpus, then
    the return probability is an average probability of transition to
    the destination if destination did occur in the corpus, otherwise
    it is just an average probability of transition.
*)
val probability : t -> insn -> insn option -> float
