open Core.Std

type t with sexp
include Pretty_printer.S with type t := t

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

(** [involution n] is a random involution that exchanges 1..n/2 with
    n/2+1...n *)
val involution : int -> t

module Infix : sig
  (** compose *)
  val (@) : t -> t -> t
end
