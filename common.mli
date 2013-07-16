
module Bind : sig
  type ('pat, 'term) t
  val create : 'pat -> 'term -> ('pat, 'term) t
  val unbind : ('pat, 'term) t -> 'pat * 'term
  val type_name : 'pat Type.Name.t -> 'term Type.Name.t -> ('pat, 'term) t Type.Name.t
end

module Rebind : sig
  type ('pat1, 'pat2) t = private 'pat1 * 'pat2
  val create : 'pat1 -> 'pat2 -> ('pat1, 'pat2) t
  val type_name : 'pat1 Type.Name.t -> 'pat2 Type.Name.t -> ('pat1, 'pat2) t Type.Name.t
end

module Embed : sig
  type 'term t = private 'term
  val create : 'term -> 'term t
  val type_name : 'term Type.Name.t -> 'term t Type.Name.t
end

module Rec : sig
  type 'pat t = private 'pat
  val create : 'pat -> 'pat t
  val type_name : 'pat Type.Name.t -> 'pat t Type.Name.t
end

