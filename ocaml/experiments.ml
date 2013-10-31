open Core.Std

open Permutation.Infix
module P = Permutation

let count times f =
  let rec loop ~times count =
    if times = 0 then count
    else loop ~times:(times - 1)
           (if f () then count + 1 else count)
  in
  loop ~times 0

let count1 n m =
  count m (fun () ->
    let scramble = P.rand n in
    P.is_cycle (scramble <<< P.rot1 n <<< P.inverse scramble)
  )

let count2 n m =
  let scramble = P.rand n in
  let cyc = scramble <<< P.rot1 n <<< P.inverse scramble in
  let scramble2 = P.rand n in
  let cyc2 = scramble2 <<< P.rot1 n <<< P.inverse scramble2 in
  count m (fun () ->
    let inv = P.involution n in
    P.is_cycle (inv <<< cyc)
    && P.is_cycle (inv <<< cyc2)
  )

let count1r n m =
  let scramble = P.rand n in
  count m (fun () -> P.is_cycle (P.involution n <<< scramble))


(* Experiment 1 *)

type exp1 = { n: int;
              c1: decimal;
              c2: decimal;
              c1_sq: decimal;
            }
with sexp

let exp1 m =
  printf "Cycle counting experiment: M=%d\n" m;
  List.iter (List.range 100 120 ~stride:4) ~f:(fun n ->
    let c1 = count1 n m in
    let c2 = count2 n m in
    let sq x = x *. x in
    { n
    ; c1 = c1 // m
    ; c2 = c2 // m
    ; c1_sq = sq (c1 // m)
    }
    |> sexp_of_exp1 |> Sexp.to_string_hum |> print_endline
  )

let () = exp1 10_000
