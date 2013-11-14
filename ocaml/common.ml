open Core.Std

let strand = Strand.of_int
let branch = Branch.of_int

let sexp_print conv v =
  v |> conv |> Sexp.to_string_hum |> print_endline
