open Core.Std

type dir = Up | Down

module Int_id = struct
  let to_int (x:int) = x
  let of_int (x:int) = x
  include Int
end

module type Private_int = sig
  type t = private int with sexp
  include Int_intf.S with type t := t
  val of_int : int -> t
  val to_int : t -> int
end

(* The id of a given branch *)
module Branch_id : Private_int = Int_id
(* A position on the interval *)
module Position  : Private_int = Int_id
(* A position within a branch *)
module Branch_position : Private_int = Int_id

(* The order of the branches for a given IET *)
module Branches = struct
  type t =
    { top: Branch_id.t array
    ; bot: Branch_id.t array
    }
  with sexp

  let side t = function Up -> t.top | Down -> t.bot

  let validate t =
    let fail s = failwiths ("Branches: "^s) t sexp_of_t in
    let counts =
      (Array.to_list t.top @ Array.to_list t.bot)
      |> List.map ~f:(fun i -> (i,1))
      |> Branch_id.Map.of_alist_fold ~init:0 ~f:(+)
    in
    let keys = Map.keys counts |> Branch_id.Set.of_list in
    match Set.max_elt keys with
    | None -> fail "empty Branch_order.t"
    | Some max ->
      let range =
        List.range 0 ((max :> int) + 1)
        |> List.map ~f:Branch_id.of_int
        |> Branch_id.Set.of_list
      in
      if not (Set.equal range keys) then
        fail "Set of indices should be range from 0..n";
      Map.iter counts ~f:(fun ~key:_ ~data:count ->
          if count <> 2 then fail "All indices should appear exactly twice")
end

(* A full IET is specified by the branches and the set of widths *)
type t =
  { branches: Branches.t
  ; widths: int array
  }

let width t (id : Branch_id.t) = t.widths.( (id :> int) )


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
