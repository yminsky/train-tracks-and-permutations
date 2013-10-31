open Core.Std

type t = int array with sexp, compare

let length = Array.length

let id n =
  Array.init n ~f:Fn.id

let rot1 n =
  Array.init n ~f:(fun i -> (i + 1) % n)

let rand n =
  let t = id n in
  Array.permute t;
  t

let compose p q =
  assert (Array.length p = Array.length q);
  Array.init (Array.length p) ~f:(fun i -> p.(q.(i)))

let inverse t =
  let inv = id (Array.length t) in
  for i = 0 to Array.length t - 1 do
    inv.(t.(i)) <- i
  done;
  inv

let next t pos = t.(pos)

let cycle_length t start =
  let rec how_long_until ~target ~current ~so_far =
    if current = target then so_far
    else how_long_until ~target ~current:(next t current) ~so_far:(so_far + 1)
  in
  how_long_until ~target:start ~current:(next t start) ~so_far:1

let is_cycle t =
  cycle_length t 0 = Array.length t

module Infix = struct
  let (@) = compose
end

include Pretty_printer.Register (struct
  type nonrec t = t
  let module_name = "Permutation"
  let to_string t =
    Array.sexp_of_t Int.sexp_of_t t
    |> Sexp.to_string_hum
end)
