open Std_internal

val freshen
  :  'a Type.Rep.t
  -> 'a
  -> 'a * Name.Univ.Perm.t

val fresh_wrt
  :  'a Type.Rep.t
  -> 'a
  -> fvs:Name.Univ.Set.t
  -> 'a * Name.Univ.Perm.t

