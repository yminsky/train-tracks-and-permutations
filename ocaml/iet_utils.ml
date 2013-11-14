open Core.Std
open Common


(* Given a strand and a side in an IET, find the strand/side pair
   that it is connected to by the branch in question *)
let find_next iet (strand, side) =
  let (attach1,attach2) =
    Iet.lookup_branch iet strand side
    |> Iet.lookup_attachments iet
  in
  (* Figure out the branch associated with this strand/side pair,
     and the branch it's connected to *)
  let (mine,other) =
    if Iet.is_in_attachment attach1 (strand,side)
    then (attach1,attach2)
    else (
      if not (Iet.is_in_attachment attach2 (strand,side)) then
        failwiths "Strand is not in either range"
          (attach1,attach2,(strand,side))
        <:sexp_of<Iet.attachment * Iet.attachment * (Strand.t * Side.t)>>
      ;
      (* assert (in_range attach2.strand_range strand); *)
      (attach2,attach1)
    )
  in
  (* We want an orientable surface, so we flip the strand order when
     reconnecting to the same side *)
  let should_flip =
    mine.side = other.side
  in
  let other_strand =
    let branch_start = fst mine.strand_range in
    let pos_in_branch = Strand.(strand - branch_start) in
    if not should_flip then
      let other_branch_start = fst other.strand_range in
      Strand.(other_branch_start +: pos_in_branch)
    else
      let other_branch_end = snd other.strand_range in
      Strand.(other_branch_end   -: pos_in_branch)
  in
  (other_strand, Side.flip other.side)

let find_cycle iet start =
  let rec loop current accum =
    let next = find_next iet current in
    if next = start then List.rev (current :: accum)
    else loop next (current :: accum)
  in
  loop start []

let cycle_is_complete iet cycle =
  List.length cycle = 2 * Iet.num_strands iet
