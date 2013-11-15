open Core.Std
open Common

type t with sexp
include Pretty_printer.S with type t := t

(** {2 Creation functions} *)

val create
  :  int list Side_pair.t
  -> widths:int list
  -> t

(** {2 Accessors} *)

val diagnostic_sexp : t -> Sexp.t

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

(* Given an oriented trand, finds the next oriented strand under this
   IET *)
val next : t -> (Strand.t * Side.t) -> (Strand.t * Side.t)
