open Std_internal

type 'a t = Name.Univ.Perm.t -> 'a -> 'a

val swap : 'a Type.Rep.t -> 'a t

val register : 'a Type.Name.t -> 'a t -> unit
