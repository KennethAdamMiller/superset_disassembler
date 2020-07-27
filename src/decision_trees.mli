open Bap.Std
open Core_kernel

type decision_tree
type 'a possibility
type 'a choice
type 'a consequence
type tail

val decision_trees_of_superset : Superset.t -> decision_tree
module Speculate : sig
  val weigh_possibilities : Superset.t -> decision_tree ->
                            unit
  val make_choices : Superset.t -> decision_tree
                     -> (('a possibility * 'a consequence) list ->
                         'a choice)
                     -> Superset.t

end
val conflicts_of_entries :
    Superset.t -> Addr.Hash_set.t -> Addr.Hash_set.t list
val calculate_deltas :
  Superset.t -> ?entries:Addr.Hash_set.t -> (addr -> bool)
  -> (Addr.Hash_set.t * Addr.Hash_set.t) Addr.Map.t
val tails_of_conflicts :
  Superset.t -> Addr.Set.t -> addr list Addr.Map.t
