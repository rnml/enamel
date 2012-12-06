open Core.Std

let intersperse x = function
  | [] -> []
  | y :: ys ->
    y :: List.fold_right ys ~init:[] ~f:(fun y xys -> x :: y :: xys)

let paren x =
  let open Text_block in
  hcat [text "("; x; text ")"]

let space = Text_block.text " "

let type_apply xs foo =
  let open Text_block in
  match xs with
  | [] -> text foo
  | xs ->
    hcat ~sep:space [
      paren (hcat ~sep:(text ", ") xs);
      text foo
    ]

module Compile_time = struct

  module Def = struct
    type 'a t =
    | Synonym of 'a
    | Variant of 'a list Constant.Map.t
    with sexp

    let type_def a_def = function
      | Synonym a -> a_def a
      | Variant map ->
        Map.to_alist map
        |! List.map ~f:(fun (constr, args) ->
          Text_block.hcat ~sep:space [
            Text_block.text "|";
            Text_block.text (Constant.to_string constr);
            begin
              if List.is_empty args then
                Text_block.nil
              else
                Text_block.hcat ~sep:space
                  (Text_block.text "of" :: intersperse (Text_block.text "*") (List.map args ~f:a_def))
            end;
          ]
        )
        |! Text_block.vcat
  end

  module Regular = struct
    type 'a t =
    | Option of 'a
    | List   of 'a
    | Pair   of 'a * 'a
    | Triple of 'a * 'a * 'a
    | Ref    of string
    | Map    of string * 'a
    with sexp

    let map t ~f =
      match t with
      | Option a         -> Option (f a)
      | List a           -> List (f a)
      | Pair (a, b)      -> Pair (f a, f b)
      | Triple (a, b, c) -> Triple (f a, f b, f c)
      | Ref x            -> Ref x
      | Map (x, a)       -> Map (x, f a)

    let type_def a_def = function
      | Option a -> type_apply [a_def a] "option"
      | List a -> type_apply [a_def a] "list"
      | Map (key, a) -> type_apply [a_def a] (String.capitalize key ^ ".Map.t")
      | Pair (a, b) -> Text_block.(hcat [paren (a_def a); text "*"; paren (a_def b)])
      | Triple (a, b, c) -> Text_block.(hcat [paren (a_def a); text "*"; paren (a_def b); text "*"; paren (a_def c)])
      | Ref "" -> assert false
      | Ref x ->
        let capitalized = let c0 = x.[0] in Char.equal c0 (Char.uppercase c0) in
        if capitalized then Text_block.text (x ^ ".t") else Text_block.text x
  end

  module Term = struct
    type ('t, 'p) t =
    | Var of string (* use site *)
    | Bind of 'p * 't
    with sexp

    let sexp_of_t sexp_of_a sexp_of_b = function
      | Var x -> Sexp.Atom ("$" ^ x)
      | t -> sexp_of_t sexp_of_a sexp_of_b t

    let t_of_sexp a_of_sexp b_of_sexp = function
      | Sexp.Atom x as sexp ->
        begin
          match String.chop_prefix x ~prefix:"$" with
          | None -> t_of_sexp a_of_sexp b_of_sexp sexp
          | Some var -> Var var
        end
      | sexp -> t_of_sexp a_of_sexp b_of_sexp sexp

    let type_def t_def p_def = function
    | Var x       -> type_apply [] (String.capitalize x ^ ".Name.t")
    | Bind (p, t) -> type_apply [p_def p; t_def t] "Bind.t"
  end

  module Pattern = struct
    type ('p, 't) t =
    | Var of string (* binding site *)
    | Embed  of 't
    | Rebind of 'p * 'p
    | Rec    of 'p
    with sexp

    let sexp_of_t sexp_of_a sexp_of_b = function
      | Var x -> Sexp.Atom ("$" ^ x)
      | t -> sexp_of_t sexp_of_a sexp_of_b t

    let t_of_sexp a_of_sexp b_of_sexp = function
      | Sexp.Atom x as sexp ->
        begin
          match String.chop_prefix x ~prefix:"$" with
          | None -> t_of_sexp a_of_sexp b_of_sexp sexp
          | Some var -> Var var
        end
      | sexp -> t_of_sexp a_of_sexp b_of_sexp sexp

    let type_def p_def t_def = function
    | Var x           -> type_apply [] (String.capitalize x ^ ".Name.t")
    | Embed t         -> type_apply [t_def t]            "Embed.t"
    | Rebind (p1, p2) -> type_apply [p_def p1; p_def p2] "Rebind.t"
    | Rec p           -> type_apply [p_def p]            "Rec.t"
  end

  type tm = Tm_regular of tm Regular.t | Tm of (tm, pt) Term.t
   and pt = Pt_regular of pt Regular.t | Pt of (pt, tm) Pattern.t

  let rec sexp_of_tm = function
    | Tm_regular r -> Regular.sexp_of_t sexp_of_tm r
    | Tm t -> Term.sexp_of_t sexp_of_tm sexp_of_pt t

  and sexp_of_pt = function
    | Pt_regular r -> Regular.sexp_of_t sexp_of_pt r
    | Pt t -> Pattern.sexp_of_t sexp_of_pt sexp_of_tm t

  let rec tm_of_sexp sexp =
    match Option.try_with (fun () -> Regular.t_of_sexp Fn.id sexp) with
    | Some r -> Tm_regular (Regular.map r ~f:tm_of_sexp)
    | None -> Tm (Term.t_of_sexp tm_of_sexp pt_of_sexp sexp)

  and pt_of_sexp sexp =
    match Option.try_with (fun () -> Regular.t_of_sexp Fn.id sexp) with
    | Some r -> Pt_regular (Regular.map r ~f:pt_of_sexp)
    | None -> Pt (Pattern.t_of_sexp pt_of_sexp tm_of_sexp sexp)

  let rec tm_def : tm -> Text_block.t = function
    | Tm_regular x -> Regular.type_def tm_def x
    | Tm x -> Term.type_def tm_def pt_def x
  and pt_def : pt -> Text_block.t = function
    | Pt_regular x -> Regular.type_def pt_def x
    | Pt x -> Pattern.type_def pt_def tm_def x

  module Env = struct
    type t = {
      tms : tm Def.t String.Map.t;
      pts : pt Def.t String.Map.t;
    } with sexp

    let type_defs t =
      let open Text_block in
      let gen a_def map =
        Map.to_alist map
        |! List.map ~f:(fun (foo, def) ->
          vcat [
            text ("type " ^ foo ^ " =");
            hpad (Def.type_def a_def def) ~align:`Right 2;
          ]
        )
        |! vcat ~sep:space
      in
      vcat ~sep:space [
        gen tm_def t.tms;
        gen pt_def t.pts;
      ]

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
