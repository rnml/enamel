open Std_internal

include Type_fold.Make (struct
  type acc = Name.Univ.Set.t
  let generic_name = "Binders.binders"
  include Name.Registry.Binders
end)

let binders ty x = fold ty Name.Univ.Set.empty x
include Name.Registry.Binders
