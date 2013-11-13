open Core.Std
open Common

let iet =
  Iet.create_simple
    { top = [0;1;2;0]; bot = [3;2;4;3;1;4] }
    ~widths:[6;2;3;4;2]

let () =
  iet
  |> <:sexp_of<Iet.t>>
  |> Sexp.to_string_hum
  |> print_endline

let print_cycle (strand,side) =
  Iet_utils.find_cycle iet (Strand.of_int strand,side) 
  |> <:sexp_of<(Strand.t * Side.t) list>>
  |> Sexp.to_string_hum
  |> print_endline

let () =
  print_cycle (0,Top);
  print_cycle (0,Bot);
  print_cycle (8,Top);
  print_cycle (8,Bot);
