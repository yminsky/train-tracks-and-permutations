open Core.Std

type t with sexp, bin_io

val length : t -> int

(** {9 Permutateion constructors} *)

val id      : int -> t
val rot1    : int -> t
val rand    : int -> t

(* Composes two permutations of the same length. *)
val compose : t -> t -> t

(** Finds a permutation such that [compose t (inverse t)] is the identity *)
val inverse : t -> t

val is_cycle : t -> bool
