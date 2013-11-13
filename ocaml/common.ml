open Core.Std

(* The id of a given branch *)
module Branch : Private_int.S = Private_int.M
(* A strand identifier on the interval *)
module Strand : Private_int.S = Private_int.M

let sexp_print conv v =
  v |> conv |> Sexp.to_string_hum |> print_endline
