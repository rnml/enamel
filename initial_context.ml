open Std_internal

module F = Systemf
module Ctx = Target.Context

let ctx = Ctx.empty

module K = struct
  let star = F.Kind.Star
  let (@->) a b = F.Kind.Arr (a, b)
end

module Ty = struct
  let name x = F.Ty.Name.raw x

  let int  = name "int"
  let ref_ = name "ref"
  let fun_ = name "fun"
  let pair = name "pair"
  let unit = name "unit"

  let a = name "a"
  let b = name "b"
  let c = name "c"
end

let add_ty ctx ty_var kind =
  let ctx = Ctx.add_ty ctx ty_var kind in
  let tm_var = F.Tm.Name.cast ty_var in
  let ctx =
    Ctx.add_tm ctx tm_var
      (Target.Csig.Type (F.Ty.Name ty_var, kind))
  in
  ctx

let ctx = add_ty ctx Ty.fun_  K.(star @-> star @-> star)
let ctx = add_ty ctx Ty.pair  K.(star @-> star @-> star)
let ctx = add_ty ctx Ty.ref_  K.(star @-> star)
let ctx = add_ty ctx Ty.int   K.(star)
let ctx = add_ty ctx Ty.unit  K.(star)

module Tm = struct
  let name x = F.Tm.Name.raw x

  let cons  = name "cons"
  let fst   = name "fst"
  let snd   = name "snd"
  let zero  = name "0"
  let one   = name "1"
  let plus  = name "+"
  let minus = name "-"
  let times = name "*"
  let ref_  = name "ref"
  let get   = name "get"
  let set   = name "set"
  let nil   = name "nil"
end

let add_tm ctx name ty = Ctx.add_tm ctx name (Target.Csig.Val ty)

module T : sig
  type t = Target.Csig.t
  val nm : F.Ty.Name.t -> t
  val forall : t list -> t -> t
  val (@->) : t -> t -> t
  val (+$) : t -> t -> t
  val a : t
  val b : t
  val pair : t -> t -> t
  val ref_ : t -> t
  val int : t
  val unit : t
end = struct
  type t = Target.Csig.t

  let forall xs = function
    | Target.Csig.Fun b ->
      let (ys, a, b) = Target.Csig.un_fun b in
      Target.Csig.mk_fun (xs @ ys) a b
    | _ -> assert false

  let (@->) a b =
    Target.Csig.mk_fun [] a (Target.Asig.mk_exists [] b)

  let (+$) a b =
    match (a, b) with
    | (Target.Csig.Val a, Target.Csig.Val b) ->
      Target.Csig.Val (F.Ty.App (a, b))
    | _ -> assert false

  let nm x = Target.Csig.Val (F.Ty.Name x)

  let a = nm Ty.a
  let b = nm Ty.b

  let forall xs t =
    List.fold_right xs ~init:t ~f:(fun a t ->
      match a with
      | Target.Csig.Val (F.Ty.Name a) ->
        Target.Csig.mk_fun
          [(a, K.star)]
          (Target.Csig.Type (F.Ty.Name a, K.star))
          (Target.Asig.mk_exists [] t)
      | _ -> assert false)

  let pair a b = nm Ty.pair +$ a +$ b
  let ref_ a   = nm Ty.ref_ +$ a
  let int      = nm Ty.int
  let unit     = nm Ty.unit

end

let ctx =
  List.fold
    ~f:(fun ctx (x, ty) -> Ctx.add_tm ctx x ty)
    ~init:ctx [
      (Tm.cons,  T.(forall [a; b] (a @-> b @-> pair a b)));
      (Tm.fst,   T.(forall [a; b] (pair a b @-> a)));
      (Tm.snd,   T.(forall [a; b] (pair a b @-> b)));
      (Tm.zero,  T.(int));
      (Tm.one,   T.(int));
      (Tm.plus,  T.(int @-> int @-> int));
      (Tm.minus, T.(int @-> int @-> int));
      (Tm.times, T.(int @-> int @-> int));
      (Tm.ref_,  T.(forall [a] (a @-> ref_ a)));
      (Tm.get,   T.(forall [a] (ref_ a @-> a)));
      (Tm.set,   T.(forall [a] (ref_ a @-> a @-> unit)));
      (Tm.nil,   T.(unit));
    ]
