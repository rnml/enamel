open Std_internal

(*
type pat
type exp

module Mode : sig
  type 'a t
  val exp : exp t
  val pat : pat t
end

module Alpha : sig
  type ('a, 'phantom) t
  val now : ('a, 'phantom) t Lazy.t -> ('a, 'phantom) t
end

module Name : sig
  type 'a t
  val tc : 'm Mode.t -> ('a t, 'm) Alpha.t

  val dummy : 'a t
  val raw : string -> 'a t
  val pretty : 'a t -> Pretty.doc
end

module Names : sig
  type 'a t
  val tc : ('a, exp) Alpha.t -> ('a t, _) Alpha.t
end

(*
module T2 : sig
  val tc : ('a, 'm) Alpha.t -> ('b, 'm) Alpha.t -> ('a * 'b, 'm) Alpha.t
end
*)

(*
module T3 : sig
  val tc :
       ('a, 'm) Alpha.t
    -> ('b, 'm) Alpha.t
    -> ('c, 'm) Alpha.t
    -> ('a * 'b * 'c, 'm) Alpha.t
end

module Option : sig
  val tc : ('a, 'm) Alpha.t -> ('a option, 'm) Alpha.t
end

module List : sig
  val tc : ('a, 'm) Alpha.t -> ('a list, 'm) Alpha.t
end

module Tag : sig
  val tc : Constant.t -> ('a, 'm) Alpha.t -> ('a, 'm) Alpha.t
end

module Variant : sig
  module Case : sig
    type ('a, 'm) t
    val tc :
      ('args, 'm) Alpha.t
      -> inj:('args -> 'a)
      -> prj: ('a -> 'args option)
      -> ('a, 'm) t
  end 
  val tc : name:string -> ('a, 'm) Case.t list -> ('a, 'm) Alpha.t
end

module Bind : sig
  type ('p, 'e) t
  val tc :
    ('p, pat) Alpha.t -> ('e, exp) Alpha.t -> (('p, 'e) t, exp) Alpha.t
end

module Embed : sig
  type 'e t
  val tc : ('e, exp) Alpha.t -> ('e t, pat) Alpha.t
end

module Rebind : sig
  type ('p, 'q) t
  val tc :
    ('p, pat) Alpha.t -> ('q, pat) Alpha.t -> (('p, 'q) t, pat) Alpha.t
end

module Rec : sig
  type 'p t
  val tc : ('p, pat) Alpha.t -> ('p t, pat) Alpha.t
end

val bind :
  ('p, pat) Alpha.t -> ('e, _) Alpha.t -> 'p -> 'e -> ('p, 'e) Bind.t

val unbind :
  ('p, pat) Alpha.t -> ('e, exp) Alpha.t -> 'p -> 'e -> ('p, 'e) Bind.t

val fv :
  ('a, exp) Alpha.t -> ('b, _) Alpha.t -> 'b -> 'a Names.t

val subst :
  ('a, exp) Alpha.t -> ('b, _) Alpha.t -> 'a Name.t * 'a -> 'b -> 'b
*)

val equal : ('a, 'm) Alpha.t -> 'a -> 'a -> bool
*)
