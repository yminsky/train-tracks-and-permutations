open Core.Std

type t = Top | Bot with sexp

let flip = function Top -> Bot | Bot -> Top
