open Core.Std

type 'a computation = New_name.Univ.t * New_name.Univ.t -> 'a -> 'a

val swap : 'a Type.Rep.t -> 'a computation

val register : 'a Type.Name.t -> 'a computation -> unit
