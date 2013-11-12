open Core.Std

type 'a t = { top: 'a; bot: 'a } with sexp

val get : 'a t -> Side.t -> 'a
val set : 'a t -> Side.t -> 'a -> 'a t
val of_fn : (Side.t -> 'a) -> 'a t
