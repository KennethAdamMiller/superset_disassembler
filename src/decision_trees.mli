open Bap.Std
open Core_kernel

type decision_tree
type decision_forest
type 'a possibility
type 'a choice
type 'a consequence
type tail

val decision_trees_of_superset : Superset.t -> decision_forest
module Speculate : sig
  val weigh_possibilities : Superset.t -> decision_forest ->
                            unit
  val make_choices : Superset.t -> decision_forest
                     -> (('a possibility * 'a consequence) list ->
                         'a choice)
                     -> Superset.t

end

module DecisionTree : sig
  val count : decision_tree -> int
  val mem : addr -> decision_tree -> bool
end

val count : decision_forest -> int
val with_trees : decision_forest -> init:'a -> f:('a -> decision_tree -> 'a) -> 'a


val conflicts_of_entries :
    Superset.t -> Addr.Hash_set.t -> Addr.Hash_set.t list
val calculate_deltas :
  ?entries:Addr.Hash_set.t -> ?is_option:(addr -> bool) ->
  Superset.t -> (Addr.Hash_set.t * Addr.Hash_set.t) Addr.Map.t
val tails_of_conflicts :
  Superset.t -> Addr.Set.t -> addr list Addr.Map.t
val visit_with_deltas :
  ?pre:(('a Addr.Hash_set.t_ * 'b Addr.Hash_set.t_)
          Addr.Map.t -> addr -> unit) ->
  ?post:(('a Bap.Std.Addr.Hash_set.t_ * 'b Bap.Std.Addr.Hash_set.t_)
           Addr.Map.t -> addr -> unit) ->
  is_option:(addr -> bool) ->
  Superset.t -> Addr.Hash_set.t -> unit
