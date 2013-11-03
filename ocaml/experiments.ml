open Core.Std

open Permutation.Infix
module P = Permutation

(** [true_ratio n f] Runs [f] [n] times, and returns the ratio of those times
    for which [f] returned [true] *)
let true_ratio times f =
  let rec loop ~times count =
    if times = 0 then count
    else loop ~times:(times - 1)
           (if f () then count + 1 else count)
  in
  loop ~times 0 // times

let ratio1 n m =
  let scramble = P.rand n in
  let cyc = scramble ++ P.rot1 n ++ P.inverse scramble in
  true_ratio m (fun () ->
    P.is_cycle (P.involution n ++ cyc)
  )

let ratio2 n m =
  let scramble = P.rand n in
  let cyc = scramble ++ P.rot1 n ++ P.inverse scramble in
  let scramble2 = P.rand n in
  let cyc2 = scramble2 ++ P.rot1 n ++ P.inverse scramble2 in
  true_ratio m (fun () ->
    let inv = P.involution n in
    P.is_cycle (inv ++ cyc)
    && P.is_cycle (inv ++ cyc2)
  )

let ratio1r n m =
  let scramble = P.rand n in
  true_ratio m (fun () -> P.is_cycle (P.involution n ++ scramble))


(* Experiment 1 *)

type exp1 = { n: int
            ; r1: decimal
            ; r2: decimal
            ; r1_sq: decimal
            }
with sexp

let exp1 m =
  printf "Cycle counting experiment: M=%d\n" m;
  List.iter (List.range 100 120 ~stride:4) ~f:(fun n ->
    let r1 = ratio1 n m in
    let r2 = ratio2 n m in
    let r1_sq = r1 *. r1 in
    { n; r1; r2; r1_sq }
    |> sexp_of_exp1 |> Sexp.to_string_hum |> print_endline
  )

let () =
  Random.self_init ();
  exp1 10_000
