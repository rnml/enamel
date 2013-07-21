open Std_internal

type 'a t = New_name.Univ.t * New_name.Univ.t -> 'a -> 'a

val swap : 'a Type.Rep.t -> 'a t

val register : 'a Type.Name.t -> 'a t -> unit
