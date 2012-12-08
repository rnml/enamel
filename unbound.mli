open Core.Std

module Compile_time : sig

  module Def : sig
    type 'a t =
    | Synonym of 'a
    | Variant of 'a list Constant.Map.t
    with sexp
    val type_def : ('a -> Text_block.t) -> 'a t -> Text_block.t
  end

  module Regular : sig
    type 'a t =
    | Option of 'a
    | List   of 'a
    | Pair   of 'a * 'a
    | Triple of 'a * 'a * 'a
    | Ref    of string
    | Map    of string * 'a
    with sexp
    val type_def : ('a -> Text_block.t) -> 'a t -> Text_block.t
  end

  module Term : sig
    type ('t, 'p) t =
    | Var of string (* use site *)
    | Bind of 'p * 't
     with sexp
    val type_def : ('a -> Text_block.t) -> ('b -> Text_block.t) -> ('a, 'b) t -> Text_block.t
  end

  module Pattern : sig
    type ('p, 't) t =
    | Var of string (* binding site *)
    | Embed  of 't
    | Rebind of 'p * 'p
    | Rec    of 'p
    with sexp
    val type_def : ('a -> Text_block.t) -> ('b -> Text_block.t) -> ('a, 'b) t -> Text_block.t
  end

  type tm = Tm_regular of tm Regular.t | Tm of (tm, pt) Term.t
   and pt = Pt_regular of pt Regular.t | Pt of (pt, tm) Pattern.t
  with sexp

  module Env : sig
    type t = {
      tms : tm Def.t String.Map.t;
      pts : pt Def.t String.Map.t;
    } with sexp
    val type_defs : t -> Text_block.t
  end

end

module Run_time : sig

  module Bind : sig
    type ('a, 'b) t
  end

  module Embed : sig
    type 'a t
  end

  module Syntax : sig

    module Regular : sig
      type 'a t = [
      | `Option of 'a option
      | `List of 'a list
      | `Pair of 'a * 'a
      | `Con of Constant.t * 'a list
      ] with sexp
    end

    module Term : sig
      type ('t, 'p) t = [
      | `Var of Name.t
      | `Bind of 'p * 't
      ] with sexp
    end

    module Pattern : sig
      type ('p, 't) t = [
      | `Var of Name.t
      | `Embed of 't
      | `Rebind of 'p * 'p
      | `Rec of 'p
      ] with sexp
    end

    type tm = [ tm Regular.t | (tm, pt) Term.t ]
     and pt = [ pt Regular.t | (pt, tm) Pattern.t ]
    with sexp

  end

  module type Term_tc = sig
    type t
    val inject  : t -> Syntax.tm
    val project : Syntax.tm -> t
    val fvs     : t -> Name.Set.t
  end

  module type Pattern_tc = sig
    type t
    val inject  : t -> Syntax.pt
    val project : Syntax.pt -> t
    val fvs     : t -> Name.Set.t
  end

end
