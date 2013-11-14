open Core.Std

type t = Top | Bot with sexp

val flip : t -> t

