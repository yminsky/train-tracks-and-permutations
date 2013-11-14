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

val lookup_branch
  :  t
  -> Strand.t
  -> Side.t
  -> Branch.t

type attachment =
  { strand_range : Strand.t * Strand.t
  ; side    : Side.t
  }
with sexp

val is_in_attachment : attachment -> (Strand.t * Side.t) -> bool

val lookup_attachments
  :  t
  -> Branch.t
  -> attachment * attachment
