open Core.Std
open Common

type attachment =
  { strand_range : Strand.t * Strand.t
  ; side    : Side.t
  }
with sexp

type strand_info =
  { branch : Branch.t
  ; this   :  attachment
  ; other  : attachment
  }

let is_in_attachment {strand_range=(lo,hi); side = att_side} (strand,side) =
  Strand.(strand >=lo && strand <= hi)
  && side = att_side

type annotated_branch =
  { start: Strand.t
  ; branch: Branch.t
  ; width: int
  ; side: Side.t
  }
with sexp

type t = { branch_by_strand : Branch.t Strand.Map.t Side_pair.t
         ; attachments : (attachment * attachment) Branch.Map.t
         ; num_strands : int
         }
with sexp

let lookup_branch t (strand,side) =
  Map.find_exn (Side_pair.get t.branch_by_strand side) strand

let lookup_attachments t branch =
  Map.find_exn t.attachments branch

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

let num_strands t = t.num_strands

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

let create branches ~widths =
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
  let top_strands = Map.length branch_by_strand.top in
  let bot_strands = Map.length branch_by_strand.bot in
  let t = { branch_by_strand
          ; attachments
          ; num_strands = top_strands }
  in
  if top_strands <> bot_strands then
    failwiths "Mismatch between number of top_strands and bot_strands"
      (top_strands,bot_strands,t)
      <:sexp_of<int * int * t>>;
  t
    

let create_simple branches ~widths =
  let branches = Side_pair.map branches ~f:(List.map ~f:Branch.of_int) in
  let widths =
    List.foldi widths ~init:Branch.Map.empty ~f:(fun idx map width ->
        let branch = Branch.of_int idx in
        Map.add map ~key:branch ~data:width)
  in
  create branches ~widths

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
