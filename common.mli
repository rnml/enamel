
module Bind : sig
  type ('pat, 'term) t
  val create : 'pat -> 'term -> ('pat, 'term) t
  val unbind : ('pat, 'term) t -> 'pat * 'term
end

module Rebind : sig
  type ('pat1, 'pat2) t = private 'pat1 * 'pat2
  val create : 'pat1 -> 'pat2 -> ('pat1, 'pat2) t
end

module Embed : sig
  type 'term t = private 'term
  val create : 'term -> 'term t
end

module Rec : sig
  type 'pat t = private 'pat
  val create : 'pat -> 'pat t
end

