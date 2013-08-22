open Std_internal

let rec freshen ty pat =
  let perm =
    Binders.binders ty pat
    |! Set.to_list
    |! List.map ~f:(fun x -> (x, Name.Univ.freshen x))
    |! Name.Univ.Perm.of_alist
  in
  (Swap.swap ty perm pat, perm)
