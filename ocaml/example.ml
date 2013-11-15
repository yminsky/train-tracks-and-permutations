open Core.Std
open Common

let run () =
(*
  let iet =
    Iet.create_simple
      { top = [0;1;2;0]; bot = [3;2;4;3;1;4] }
      ~widths:[6;2;3;4;2]
  in
*)
  let iet =
    Iet.create
      { top = [0;1;2]; bot = [1;0;2] }
      ~widths:[1;2;1]
  in
  sexp_print <:sexp_of<Iet.t>> iet;
  let print_cycle (strand,side) =
    Iet_utils.find_cycle iet (Strand.of_int strand,side) 
    |> sexp_print <:sexp_of<(Strand.t * Side.t) list>>
  in
  print_cycle (0,Top);
  print_cycle (0,Bot);
  print_cycle (8,Top);
  print_cycle (8,Bot)

let () =
  Exn.handle_uncaught ~exit:true run
