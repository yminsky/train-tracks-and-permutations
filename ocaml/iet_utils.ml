open Core.Std
open Common


(* Given a strand and a side in an IET, find the strand/side pair
   that it is connected to by the branch in question *)
let find_next iet (strand, side) =
  let (binfo1,binfo2) =
    Iet.lookup_branch iet strand side
    |> Iet.lookup_branch_info iet
  in
  (* Figure out the branch associated with this strand/side pair,
     and the branch it's connected to *)
  let my_binfo,other_binfo =
    if Interval.contains binfo1.strands strand
    then (binfo1,binfo2)
    else (
      assert (Interval.contains binfo2.strands strand);
      (binfo2,binfo1)
    )
  in
  (* We want an orientable surface, so we flip the strand order when
     reconnecting to the same side *)
  let should_flip =
    my_binfo.side = other_binfo.side
  in
  let other_strand =
    let branch_start = Interval.lbound_exn my_binfo.strands in
    let pos_in_branch = Strand.(strand - branch_start) in
    if not should_flip then
      let branch_start = Interval.lbound_exn other_binfo.strands in
      Strand.(branch_start +: pos_in_branch)
    else
      let branch_end = Interval.ubound_exn other_binfo.strands in
      Strand.(branch_end   -: pos_in_branch)
  in
  (other_strand,other_binfo.side)

let find_cycle iet start =
  let rec loop current accum =
    let next = find_next iet current in
    if next = start then List.rev (current :: accum)
    else loop next (current :: accum)
  in
  loop start []
