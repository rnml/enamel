open Std_internal

open Unbound

module Args = struct
  type t = (F.Type.Name.t * F.Kind.t Embed.t) list with sexp
end

module rec Csig : sig
  type t =
    | Val    of F.Type.t
    | Type   of F.Type.t * F.Kind.t
    | Sig    of Asig.t
    | Struct of t F.Label.Map.t
    | Fun    of (Args.t, t * Asig.t) Bind.t
  with sexp
end = struct

  type t =
    | Val    of Type.t
    | Type   of Type.t * Kind.t
    | Sig    of Asig.t
    | Struct of t Label.Map.t
    | Fun    of (Args.t, t * Asig.t) Bind.t
  with sexp
end

and Asig : sig
  type t = Exists of (Args.t, Csig.t) Bind.t with sexp
end = struct
  type t = Exists of (Args.t, Csig.t) Bind.t with sexp
end
