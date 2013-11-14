open Core.Std
open Common

type t with sexp
include Pretty_printer.S with type t := t

(** {2 Creation functions} *)

val create
  :  Branch.t list Side_pair.t
  -> widths : int Branch.Map.t
  -> t

val create_simple
  :  int list Side_pair.t
  -> widths:int list
  -> t

(** {2 Accessors} *)

val num_strands : t -> int

type attachment =
  { strand_range : Strand.t * Strand.t
  ; side : Side.t
  }
with sexp

type strand_info =
  { branch : Branch.t
  ; this   : attachment
  ; other  : attachment
  }
with sexp

val lookup_strand_info : t -> (Strand.t * Side.t) -> strand_info

val next : t -> (Strand.t * Side.t) -> (Strand.t * Side.t)
