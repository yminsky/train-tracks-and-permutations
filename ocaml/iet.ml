open Core.Std
open Common

type branch_info =
  { strands: Strand.t Interval.t
  ; width: int
  ; side: Side.t
  }

type t = { branch_by_strand : Branch.t Strand.Map.t Side_pair.t
         ; branch_info : (branch_info * branch_info) Branch.Map.t
         }

let lookup_branch t strand side =
  Map.find_exn (Side_pair.get t.branch_by_strand side) strand

let lookup_branch_info t branch =
  Map.find_exn t.branch_info branch

let create branches ~widths =
  let widths = Branch.Map.of_alist_exn widths in
  let index_branches side =
    Side_pair.get branches side
    |> List.map ~f:(fun branch ->
        let width = Map.find_exn widths branch in
        List.init width ~f:(fun _ -> branch)
      )
    |> List.concat
    |> List.mapi ~f:(fun idx branch -> (Strand.of_int idx,branch))
    |> Strand.Map.of_alist_exn
  in
  let branch_info =
    assert false
  in
  { branch_by_strand = Side_pair.of_fn index_branches
  ; branch_info
  }
    
