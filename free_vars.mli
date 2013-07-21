open Std_internal

module Term : sig
  val fv : 'a Type.Rep.t -> 'a -> New_name.Univ.Set.t
  val fv_aux : 'a Type.Rep.t -> New_name.Univ.Set.t -> 'a -> New_name.Univ.Set.t
  val register : 'a Type.Name.t -> (New_name.Univ.Set.t -> 'a -> New_name.Univ.Set.t) -> unit
end

module Pat : sig
  val fv : 'a Type.Rep.t -> 'a -> New_name.Univ.Set.t
  val fv_aux : 'a Type.Rep.t -> New_name.Univ.Set.t -> 'a -> New_name.Univ.Set.t
  val register : 'a Type.Name.t -> (New_name.Univ.Set.t -> 'a -> New_name.Univ.Set.t) -> unit
end
