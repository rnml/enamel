open Core.Std

type 'a computation = 'a -> Nm.Set.t

val fvs : 'a Type.Rep.t -> 'a computation

val register :
  'a Type.Name.t -> (New_name.Univ.Set.t -> 'a computation) -> unit
