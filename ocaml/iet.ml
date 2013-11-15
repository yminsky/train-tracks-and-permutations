open Core.Std
open Common

type attachment =
  { strand_range : Strand.t * Strand.t
  ; side    : Side.t
  }
with sexp

type strand_info =
  { branch : Branch.t
  ; this   : attachment
  ; other  : attachment
  }
with sexp

let is_in_attachment {strand_range=(lo,hi); side = att_side} (strand,side) =
  Strand.(strand >=lo && strand <= hi)
  && side = att_side

type annotated_branch =
  { start  : Strand.t
  ; branch : Branch.t
  ; width  : int
  ; side   : Side.t
  }
with sexp

module Format = struct
  type t = { branches : Branch.t list Side_pair.t
           ; widths : int array
           }
  with sexp
end

type t = { branch_by_strand : Branch.t array Side_pair.t
         ; attachments_by_branch : (attachment * attachment) array
         ; format : Format.t
         }
with sexp

let num_strands t = Array.length t.branch_by_strand.top

let lookup_branch t (strand,side) =
  let strand_num = Strand.to_int strand in
  if strand_num > Array.length t.branch_by_strand.top
  then failwithf "Unknown strand: %d" strand_num ()
  else (Side_pair.get t.branch_by_strand side).(Strand.to_int strand)

let lookup_attachments t branch =
  t.attachments_by_branch.(Branch.to_int branch)

let lookup_strand_info t ostrand =
  let branch = lookup_branch t ostrand in
  let (att1,att2) = lookup_attachments t branch in
  let (this,other) =
    if is_in_attachment att1 ostrand then 
      (att1,att2)
    else (
      if not (is_in_attachment att2 ostrand) then
        failwiths "Strand is not in either range"
          (att1,att2,ostrand)
        <:sexp_of<attachment * attachment * (Strand.t * Side.t)>>
      ;
      (att2,att1)
    )
  in
  { branch; this; other } 

let index_branches_by_strand annotated_branches =
  List.fold annotated_branches
    ~init:(Side_pair.of_fn (fun _ -> Strand.Map.empty))
    ~f:(fun maps {side;start;branch;width} ->
        Side_pair.change maps side ~f:(fun map ->
            List.fold (List.range 0 width)
              ~init:map
              ~f:(fun map i ->
                  let strand = Strand.(start +: i) in
                  Map.add map ~key:strand ~data:branch
                )))

let annotate_branches branches ~widths side = 
  List.fold (Side_pair.get branches side) ~init:(Strand.zero,[])
    ~f:(fun (start,acc) branch ->
        let width = Map.find_exn widths branch in
        let start' = Strand.(start +: width) in
        (start',
         {side; start; branch; width} :: acc)
      )
  |> snd

(* Converts a map with a Private_int as its index into the
   corresponding array.  Requires that the map indices go from 0 to
   len - 1. *)
let map_to_array
    (type a)
    map
    (module Id : Private_int.S with type t = a)
    id_name
  =
  match Map.max_elt map with
  | None -> [| |]
  | Some (max_branch,_) ->
    Array.init (Id.to_int max_branch + 1) ~f:(fun branch_num ->
        match Map.find map (Id.of_int branch_num) with
        | None -> failwithf "%s number gap at %d" id_name branch_num ()
        | Some x -> x)


let of_format (format:Format.t) =
  let branches = format.branches in
  let widths =
    Array.foldi format.widths
      ~init:Branch.Map.empty
      ~f:(fun idx acc width ->
          Map.add acc ~key:(Branch.of_int idx) ~data:width)
  in
  let annotated_branches =
    let of_side side = annotate_branches branches ~widths side in
    of_side Top @ of_side Bot
  in
  let branch_by_strand =
    index_branches_by_strand annotated_branches
  in
  let attachments =
    annotated_branches
    |> List.map ~f:(fun ({branch;_} as annot) ->
        (branch,annot))
    |> Branch.Map.of_alist_multi
    |> Map.mapi ~f:(fun ~key:branch ~data:annots ->
        match annots with
        | [a1;a2] ->
          let info ({start;branch=_;width;side} : annotated_branch) : attachment =
            { strand_range = (start,Strand.(start +: Int.(width - 1)))
            ; side
            }
          in
          (info a1, info a2)
        | _ -> failwiths "Branch should appear exactly twice" branch
                 <:sexp_of<Branch.t>>
      )
  in
  let attachments_by_branch =
    map_to_array attachments (module Branch) "branch"
  in
  let branch_by_strand = 
    Side_pair.map branch_by_strand ~f:(fun map ->
        map_to_array map (module Strand) "strand")
  in
  let t = { branch_by_strand
          ; attachments_by_branch
          ; format }
  in
  let top_strands = Array.length branch_by_strand.top in
  let bot_strands = Array.length branch_by_strand.bot in
  if top_strands <> bot_strands then
    failwiths "Mismatch between number of top_strands and bot_strands"
      (top_strands,bot_strands,t)
      <:sexp_of<int * int * t>>;
  t
    

let diagnostic_sexp = sexp_of_t

let sexp_of_t t = Format.sexp_of_t t.format
let t_of_sexp sexp = 
  let format = Format.t_of_sexp sexp in
  try of_format format
  with exn -> of_sexp_error_exn exn sexp

let create branches ~widths =
  let branches = Side_pair.map branches ~f:(List.map ~f:Branch.of_int) in
  let widths = Array.of_list widths in
  of_format { branches; widths }

(* Given a strand and a side in an IET, find the strand/side pair
   that it is connected to by the branch in question *)
let next iet ostrand =
  (* Figure out the branch associated with this strand/side pair,
     and the branch it's connected to *)
  let ({this;other;branch=_} : strand_info) =
    lookup_strand_info iet ostrand
  in
  (* We want an orientable surface, so we flip the strand order when
     reconnecting to the same side *)
  let should_flip =
    this.side = other.side
  in
  let other_strand =
    let branch_start = fst this.strand_range in
    let pos_in_branch = Strand.(fst ostrand - branch_start) in
    if not should_flip then
      let other_branch_start = fst other.strand_range in
      Strand.(other_branch_start +: pos_in_branch)
    else
      let other_branch_end = snd other.strand_range in
      Strand.(other_branch_end   -: pos_in_branch)
  in
  (other_strand, Side.flip other.side)




include Pretty_printer.Register (struct
    type z = t
    type t = z
    let module_name = "Iet"
    let to_string t =
      sexp_of_t t |> Sexp.to_string_hum
  end)
