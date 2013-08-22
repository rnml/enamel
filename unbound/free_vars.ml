open Std_internal

type acc = Name.Univ.Set.t

module type Registry = sig
  val generic_name : string
  include Generic.S with type 'a t = acc -> 'a -> acc
end

module Make (Registry : Registry) = struct
  include Registry
  include Type_fold.Make (struct
    type nonrec acc = acc
    include Registry
  end)
  let fv ty x = fold ty Name.Univ.Set.empty x
end

module Term = Make (struct
  let generic_name = "Term.fvs"
  include Name.Registry.Free_vars.Term
end)

module Pat = Make (struct
  let generic_name = "Pat.fvs"
  include Name.Registry.Free_vars.Pat
end)
