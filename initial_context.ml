open Std_internal

module F = Systemf
module Ctx = Target.Context

let ctx = Ctx.empty

module K = struct
  let star = F.Kind.Star
  let (@->) a b = F.Kind.Arr (a, b)
end

module Ty = struct
  let name x = F.Type.Name.freshen (F.Type.Name.raw x)

  let int  = name "int"
  let ref_ = name "ref"
  let fun_ = name "fun"
  let pair = name "pair"

  let a = name "a"
  let b = name "b"
  let c = name "c"
end

let ctx = Ctx.add_ty ctx Ty.fun_  K.(star @-> star @-> star)
let ctx = Ctx.add_ty ctx Ty.pair  K.(star @-> star @-> star)
let ctx = Ctx.add_ty ctx Ty.ref_  K.(star @-> star)
let ctx = Ctx.add_ty ctx Ty.int   K.(star)

module Tm = struct
  let name x = F.Term.Name.freshen (F.Term.Name.raw x)

  let cons  = name "cons"
  let fst   = name "fst"
  let snd   = name "snd"
  let zero  = name "0"
  let one   = name "1"
  let plus  = name "+"
  let minus = name "-"
  let times = name "*"
  let cell  = name "cell"
  let get   = name "get"
  let set   = name "set"
end

let add_tm ctx name ty = Ctx.add_tm ctx name (Target.Csig.Val ty)

module T : sig
  type t = Target.Csig.t
  val nm : F.Type.Name.t -> t
  val forall : t list -> t -> t
  val (@->) : t -> t -> t
  val (+$) : t -> t -> t
  val a : t
  val b : t
  val pair : t -> t -> t
end = struct
  type t = Target.Csig.t

  let forall xs = function
    | Target.Csig.Fun (ys, a, b) ->
      Target.Csig.Fun (xs @ ys, a, b)
    | _ -> assert false

  let (@->) a b =
    Target.Csig.Fun ([], a, Target.Asig.Exists ([], b))

  let (+$) a b =
    match (a, b) with
    | (Target.Csig.Val a, Target.Csig.Val b) ->
      Target.Csig.Val (F.Type.App (a, b))
    | _ -> assert false

  let nm x = Target.Csig.Val (F.Type.Name x)

  let a = nm Ty.a
  let b = nm Ty.b

  let forall xs t =
    forall (List.map xs ~f:(function
      | Target.Csig.Val (F.Type.Name a) -> (a, K.star)
      | _ -> assert false)
    ) t

  let pair a b = nm Ty.pair +$ a +$ b

end

let ctx =
  Ctx.add_tm ctx Tm.cons
    T.(forall [a; b] (a @-> b @-> pair a b))

(* val add_tm  : t -> Term.Name.t -> Csig.t -> t *)
