open Core.Std

val fv : 'a Type.Rep.t -> 'a -> New_name.Univ.Set.t

val register :
  'a Type.Name.t -> (New_name.Univ.Set.t -> 'a -> New_name.Univ.Set.t) -> unit


