open Std_internal

val binders : 'a Type.Rep.t -> 'a -> Name.Univ.Set.t
val fold : 'a Type.Rep.t -> Name.Univ.Set.t -> 'a -> Name.Univ.Set.t
