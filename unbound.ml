open Core.Std

module type S = sig

  module Compile_time : sig

    module Def : sig
      type 'a t = [
      | `Synonym of 'a
      | `Variant of 'a list Constant.Map.t
      ] with sexp
    end

    module Regular : sig
      type 'a t = [
      | `Option of 'a
      | `List   of 'a
      | `Pair   of 'a * 'a
      | `Ref    of string
      ] with sexp
    end

    module Term : sig
      type ('t, 'p) t = [
      | `Var (* use site *)
      | `Bind of 'p * 't
      ] with sexp
    end

    module Pattern : sig
      type ('p, 't) t = [
      | `Var (* binding site *)
      | `Embed  of 't
      | `Rebind of 'p * 'p
      | `Rec    of 'p
      ] with sexp
    end

    type tm = [ tm Regular.t | (tm, pt) Term.t ]
    and pt = [ pt Regular.t | (pt, tm) Pattern.t ]
      with sexp

    module Env : sig
      type t = {
        tms : tm Def.t String.Map.t;
        pts : pt Def.t String.Map.t;
      } with sexp
    end

  end

  module Run_time : sig

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

end
