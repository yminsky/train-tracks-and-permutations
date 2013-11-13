open Core.Std
open Common

type attachment =
  { strand_range : Strand.t * Strand.t
  ; side    : Side.t
  }
with sexp

type t = { branch_by_strand : Branch.t Strand.Map.t Side_pair.t
         ; attachments : (attachment * attachment) Branch.Map.t
         }
with sexp

let lookup_branch t strand side =
  Map.find_exn (Side_pair.get t.branch_by_strand side) strand

let lookup_attachments t branch =
  Map.find_exn t.attachments branch

type annotated_branch =
  { start: Strand.t
  ; branch: Branch.t
  ; width: int
  ; side: Side.t
  }

let create branches ~widths =
  let annotate_branches side = 
    List.fold (Side_pair.get branches side) ~init:(Strand.zero,[])
      ~f:(fun (start,acc) branch ->
          let width = Map.find_exn widths branch in
          let start' = Strand.(start +: Int.(width - 1)) in
          (start',
           {side; start; branch; width} :: acc)
        )
    |> snd
  in
  let annotated_branches =
    annotate_branches Top @ annotate_branches Bot
  in
  let branch_by_strand = 
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
  { branch_by_strand; attachments }
    

let create_simple branches ~widths =
  let branches = Side_pair.map branches ~f:(List.map ~f:Branch.of_int) in
  let widths =
    List.foldi widths ~init:Branch.Map.empty ~f:(fun idx map width ->
        let branch = Branch.of_int idx in
        Map.add map ~key:branch ~data:width)
  in
  create branches ~widths

