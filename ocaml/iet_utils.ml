open Core.Std
open Common

let find_cycle iet start =
  let rec loop current accum =
    let next = Iet.next iet current in
    if next = start then List.rev (current :: accum)
    else loop next (current :: accum)
  in
  loop start []

let cycle_is_complete iet cycle =
  List.length cycle = 2 * Iet.num_strands iet
