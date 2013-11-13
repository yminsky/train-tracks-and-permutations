open Core.Std
open Common

val find_next  : Iet.t -> Strand.t * Side.t -> Strand.t * Side.t
val find_cycle : Iet.t -> Strand.t * Side.t -> (Strand.t * Side.t) list
val cycle_is_complete : Iet.t -> (Strand.t * Side.t) list -> bool
