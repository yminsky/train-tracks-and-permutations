open Core.Std

(* Used for creating private integer types, to avoid confusion between
   different kinds of int identifiers *)

module type S = sig
  type t with sexp
  include Hashable   with type t := t
  include Comparable with type t := t
  include Pretty_printer.S with type t := t
  val (+:) : t -> int -> t
  val (-:) : t -> int -> t
  val (-) : t -> t -> int
  val zero : t
  val of_int : int -> t
  val to_int : t -> int
end

module M = struct
  include Int
  let zero = 0
  let (+:) x y = x + y
  let (-:) x y = x - y
  let (-) x y = x - y
end
