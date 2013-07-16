open Core.Std

type 'a computation = 'a -> New_name.Univ.Set.t

val fvs : 'a Type.Rep.t -> 'a computation

val register :
  'a Type.Name.t -> (New_name.Univ.Set.t -> 'a computation) -> unit

val register_name
  :  'a New_name.t Type.Name.t
  -> ('a New_name.t -> New_name.Univ.t)
  -> unit
