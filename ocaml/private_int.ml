open Core.Std

module type S = sig
  type t with sexp
  include Hashable   with type t := t
  include Comparable with type t := t
  val (+:) : t -> int -> t
  val (-:) : t -> int -> t
  val (-) : t -> t -> int
  val of_int : int -> t
  val to_int : t -> int
end

module M = struct
  include Int
  let (+:) x y = x + y
  let (-:) x y = x - y
  let (-) x y = x - y
end
