open Std_internal

module Bind : sig
  type ('pat, 'term) t
  val create : 'pat -> 'term -> ('pat, 'term) t
  val unbind : ('pat, 'term) t -> 'pat * 'term
  val type_rep : 'pat Type.Rep.t -> 'term Type.Rep.t -> ('pat, 'term) t Type.Rep.t
end

module Rebind : sig
  type ('pat1, 'pat2) t = private 'pat1 * 'pat2
  val create : 'pat1 -> 'pat2 -> ('pat1, 'pat2) t
  val type_rep : 'pat1 Type.Rep.t -> 'pat2 Type.Rep.t -> ('pat1, 'pat2) t Type.Rep.t
end

module Embed : sig
  type 'term t = private 'term
  val create : 'term -> 'term t
  val type_rep : 'term Type.Rep.t -> 'term t Type.Rep.t
end

module Rec : sig
  type 'pat t = private 'pat
  val create : 'pat -> 'pat t
  val type_rep : 'pat Type.Rep.t -> 'pat t Type.Rep.t
end

