open Core.Std

type dir = Up | Down
type side = Top | Bottom

module Int_id = struct
  include Int
  let (+:) x y = x + y
  let (-:) x y = x - y
  let (-) x y = x - y
end

module type Private_int = sig
  type t with sexp
  include Hashable   with type t := t
  include Comparable with type t := t
  val (+:) : t -> int -> t
  val (-:) : t -> int -> t
  val (-) : t -> t -> int
  val of_int : int -> t
  val to_int : t -> int
end

(* The id of a given branch *)
module Branch : Private_int = Int_id
(* A strand identifier on the interval *)
module Strand  : Private_int = Int_id
(* A position within a branch *)
module Branch_position : Private_int = Int_id

(* The order of the branches for a given IET *)
module Branches = struct
  type t =
    { top: Branch.t array
    ; bot: Branch.t array
    }
  with sexp

  let side t dir =
    match dir with
    | Up   -> t.top
    | Down -> t.bot

  let validate t =
    let fail s = failwiths ("Branches: "^s) t sexp_of_t in
    let counts =
      (Array.to_list t.top @ Array.to_list t.bot)
      |> List.map ~f:(fun i -> (i,1))
      |> Branch.Map.of_alist_fold ~init:0 ~f:(+)
    in
    let keys = Map.keys counts |> Branch.Set.of_list in
    match Set.max_elt keys with
    | None -> fail "empty Branch_order.t"
    | Some max ->
      let range =
        List.range 0 (Branch.to_int max + 1)
        |> List.map ~f:Branch.of_int
        |> Branch.Set.of_list
      in
      if not (Set.equal range keys) then
        fail "Set of indices should be range from 0..n";
      Map.iter counts ~f:(fun ~key:_ ~data:count ->
          if count <> 2 then fail "All indices should appear exactly twice")
end

module type IET = sig
  type t

  val create
    :  top : Branch.t list
    -> bot : Branch.t list
    -> widths : int Branch.Map.t
    -> t

  val lookup_branch
    :  t
    -> Strand.t
    -> side
    -> Branch.t

  type branch_info =
    { strands: Strand.t Interval.t
    ; width: int
    ; side: side
    }

  val lookup_branch_info
    :  t
    -> Branch.t
    -> branch_info * branch_info
end

module M(IET:IET) = struct

  (* Given a strand and a side in an IET, find the strand/side pair
     that it is connected to by the branch in question *)
  let find_next iet (strand, side) =
    let (binfo1,binfo2) =
      IET.lookup_branch iet strand side
      |> IET.lookup_branch_info iet
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
      let pos_in_branch = Strand.(strand  - branch_start) in
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
    
end

(*


(* A full IET is specified by the branches and the set of widths *)
type t =
  { branches: Branches.t
  ; widths: int array
  }



(* Returns the width of the given branch *)
let width t id =
  t.widths.(Branch_id.to_int id)


(* Converts an IET to a map from an index to the branch and the
   position within that branch *)
let build_index_map t dir
  : (Position.t, Branch_id.t * Branch_position.t) Map.Poly.t
  =
  (* Get the branches *)
  Branches.side t.branches dir
  (* convert each branch id into an array of indexed id/idx pairs,
     where the index tells you where you are in the branch *)
  |> Array.map ~f:(fun id -> 
      Array.init (width t id) ~f:(fun idx -> 
          (id,Branch_position.of_int idx)))
  (* Flatten the array of arrays into a single array *)
  |> Array.to_list |> Array.concat
  (* Annotate the array with positions *)
  |> Array.mapi ~f:(fun pos data -> (Position.of_int pos,data))
  (* Convert into a map from position to branch_id and
     branch_position *)
  |> Array.to_list |> Map.Poly.of_alist_exn

let reverse_map m =
  Map.to_alist m
  |> List.map ~f:(fun (x,y) -> (y,x))
  |> Map.Poly.of_alist_exn


let reverse_map m =
  Map.fold m ~init:Map.Poly.empty ~f:(fun ~key ~data accum ->
      Map.add accum ~key:data ~data:key)


(*
let compute_permutation t
  : (Position.t * dir, Position.t * dir) Map.Poly.t
  = 
  let top_map     = build_index_map t Up   in
  let top_map_rev = reverse_map top_map    in
  let bot_map     = build_index_map t Down in
  let bot_map_rev = reverse_map bot_map    in
  (* Here, we're assuming that top and bot maps have the same max index *)
  let max_pos = 
    Position.max
      (Map.max_elt_exn top_map |> fst)
  in
  let image_of_index dir i =
    let map = match dir with Up -> top_map | Down -> bot_map in
    (* Which branch, and which branch location are we on here? *)
    let (branch_id,branch_pos) = Map.find_exn map i in
*)
*)
