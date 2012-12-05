open Core.Std

module Compile_time = struct

  module Def = struct
    type 'a t = [
    | `Synonym of 'a
    | `Variant of 'a list Constant.Map.t
    ] with sexp
  end

  module Regular = struct
    type 'a t = [
    | `Option of 'a
    | `List   of 'a
    | `Pair   of 'a * 'a
    | `Triple of 'a * 'a * 'a
    | `Ref    of string
    | `Map    of 'a * 'a
    ] with sexp
  end

  module Term = struct
    type ('t, 'p) t = [
    | `Var of string (* use site *)
    | `Bind of 'p * 't
    ] with sexp
  end

  module Pattern = struct
    type ('p, 't) t = [
    | `Var of string (* binding site *)
    | `Embed  of 't
    | `Rebind of 'p * 'p
    | `Rec    of 'p
    ] with sexp
  end

  type tm = [ tm Regular.t | (tm, pt) Term.t ]
  and pt = [ pt Regular.t | (pt, tm) Pattern.t ]
    with sexp

  module Env = struct
    type t = {
      tms : tm Def.t String.Map.t;
      pts : pt Def.t String.Map.t;
    } with sexp
  end

end

module Run_time = struct

  module Syntax = struct

    module Regular = struct
      type 'a t = [
      | `Option of 'a option
      | `List of 'a list
      | `Pair of 'a * 'a
      | `Con of Constant.t * 'a list
      ] with sexp
    end

    module Term = struct
      type ('t, 'p) t = [
      | `Var of Name.t
      | `Bind of 'p * 't
      ] with sexp
    end

    module Pattern = struct
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