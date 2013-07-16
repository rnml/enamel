open Core.Std

val fvs : 'a Type.Rep.t -> 'a -> New_name.Univ.Set.t

val register
  :  'a Type.Name.t
  -> ('a -> New_name.Univ.Set.t -> New_name.Univ.Set.t)
  -> unit
