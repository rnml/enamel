open Std_internal

module Term : sig
  val fv : 'a Type.Rep.t -> 'a -> Name.Univ.Set.t
  val fv_aux : 'a Type.Rep.t -> Name.Univ.Set.t -> 'a -> Name.Univ.Set.t
  val register : 'a Type.Name.t -> (Name.Univ.Set.t -> 'a -> Name.Univ.Set.t) -> unit
end

module Pat : sig
  val fv : 'a Type.Rep.t -> 'a -> Name.Univ.Set.t
  val fv_aux : 'a Type.Rep.t -> Name.Univ.Set.t -> 'a -> Name.Univ.Set.t
  val register : 'a Type.Name.t -> (Name.Univ.Set.t -> 'a -> Name.Univ.Set.t) -> unit
end
