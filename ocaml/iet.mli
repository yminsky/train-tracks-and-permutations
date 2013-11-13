open Core.Std
open Common

type t with sexp

val create
  :  Branch.t list Side_pair.t
  -> widths : int Branch.Map.t
  -> t

val create_simple
  : int list Side_pair.t
  -> widths:int list
  -> t

val lookup_branch
  :  t
  -> Strand.t
  -> Side.t
  -> Branch.t

type branch_info =
  { strands: Strand.t Interval.t
  ; width: int
  ; side: Side.t
  }

val lookup_branch_info
  :  t
  -> Branch.t
  -> branch_info * branch_info
