open Core.Std

let intersperse x = function
  | [] -> []
  | y :: ys ->
    y :: List.fold_right ys ~init:[] ~f:(fun y xys -> x :: y :: xys)

let paren x =
  let open Text_block in
  hcat [text "("; x; text ")"]

let space = Text_block.text " "

let indent t = Text_block.hpad t ~align:`Right 2

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

  module Ctx : sig
    type t
    val create : ([`Term | `Pattern] * bool) String.Map.t -> t
    val mem     : t -> string -> bool
    val type_of : t -> string -> [`Term | `Pattern] option
    val defined : t -> string -> bool
    val while_visiting : t -> string -> (t -> 'r) -> t * 'r
    val visit_status : t -> string -> [`Not_yet_visited|`Visiting|`Already_visited]
  end = struct

    type t = {
      type_of : ([`Term | `Pattern] * bool) String.Map.t;
      visited : String.Set.t;
      visiting : string option;
    }

    let visit_status t key =
      match t.visiting with
      | Some key' when String.equal key key' -> `Visiting
      | _ -> if Set.mem t.visited key then `Already_visited else `Not_yet_visited

    let create type_of =
      {type_of; visited = String.Set.empty; visiting = None}

    let find {type_of} key = Map.find type_of key

    let type_of t key = Option.map ~f:fst (find t key)

    let mem (t:t) (key:string) = String.Map.mem t.type_of key

    let defined t key =
      match find t key with
      | None -> false
      | Some (_, def'd) -> def'd

    let while_visiting {type_of; visited; visiting = _} key f =
      let ctx = {type_of; visited; visiting = Some key} in
      let r = f ctx in
      let ctx = {type_of; visited = String.Set.add visited key; visiting = None} in
      (ctx, r)

  end

  let type_name ctx x =
    let m = String.capitalize x in
    match Ctx.visit_status ctx x with
    | `Visiting -> "t"
    | `Already_visited -> m ^ ".t"
    | `Not_yet_visited -> "Self." ^ m ^ ".t"

  module Regular = struct
    type 'a t =
    | Option of 'a
    | List   of 'a
    | Tuple  of 'a list
    | Ref    of string
    | Map    of string * 'a
    with sexp

    (* fvs : t:TYPE -> {t -> Name.Univ.Set.t} *)
    let rec fvs f = function
      | Option x ->
        "(function \
        \ | None -> Name.Univ.Set.empty \
        \ | Some x -> (" ^ f x ^ ") x)"
      | List x ->
        "(fun ts -> \
           \ List.fold ts ~init:Name.Univ.Set.empty \
           \   ~f:(fun acc t ->
           \     Name.Univ.Set.union ((" ^ f x ^ ") t)))"
      | Tuple _ | Ref _ | Map _ -> assert false

    let sexp_of_t sexp_of_a = function
      | Ref x -> Sexp.Atom ("$" ^ x)
      | t -> sexp_of_t sexp_of_a t

    let t_of_sexp a_of_sexp = function
      | Sexp.Atom x as sexp ->
        begin
          match String.chop_prefix x ~prefix:"$" with
          | None -> t_of_sexp a_of_sexp sexp
          | Some var -> Ref var
        end
      | sexp -> t_of_sexp a_of_sexp sexp

    let rec refs arefs acc = function
      | Option x | List x | Map (_, x) -> arefs acc x
      | Tuple xs -> List.fold xs ~init:acc ~f:(fun acc x -> arefs acc x)
      | Ref x -> String.Set.add acc x

    let to_list = function
      | Option x -> [x]
      | List x -> [x]
      | Tuple  xs -> xs
      | Ref _ -> []
      | Map (_, x) -> [x]

    let map t ~f =
      match t with
      | Option a   -> Option (f a)
      | List a     -> List (f a)
      | Tuple bs   -> Tuple (List.map ~f bs)
      | Ref x      -> Ref x
      | Map (x, a) -> Map (x, f a)

    let type_def ctx a_def = function
      | Option a -> type_apply [a_def ctx a] "option"
      | List a -> type_apply [a_def ctx a] "list"
      | Map (key, a) -> type_apply [a_def ctx a] (String.capitalize key ^ ".Map.t")
      | Tuple bs ->
        List.map bs ~f:(fun b -> paren (a_def ctx b))
        |! intersperse (Text_block.text " * ")
        |! Text_block.hcat
      | Ref "" -> assert false
      | Ref x ->
        let capitalized = let c0 = x.[0] in Char.equal c0 (Char.uppercase c0) in
        if capitalized then Text_block.text begin
          if Ctx.mem ctx x then type_name ctx x else x ^ ".t"
        end else Text_block.text x
  end

  module Term = struct
    type ('t, 'p) t =
    | Var of string (* use site *)
    | Bind of 'p * 't
    with sexp

    let names tnames pnames acc = function
      | Var x -> String.Set.add acc x
      | Bind (p, t) -> tnames (pnames acc p) t

    let refs tm_refs pt_refs acc = function
      | Var _ -> acc
      | Bind (p, t) -> tm_refs (pt_refs acc p) t

    let type_def ctx t_def p_def = function
      | Bind (p, t) -> type_apply [p_def ctx p; t_def ctx t] "Bind.t"
      | Var x -> type_apply [] (type_name ctx x ^ " New_name.t")
  end

  module Pattern = struct
    type ('p, 't) t =
    | Var of string (* binding site *)
    | Embed  of 't
    | Rebind of 'p * 'p
    | Rec    of 'p
    with sexp

    let names pnames tnames acc = function
      | Var x -> String.Set.add acc x
      | Rebind (p1, p2) -> pnames (pnames acc p1) p2
      | Embed t -> tnames acc t
      | Rec p -> pnames acc p

    let refs pt_refs tm_refs acc = function
      | Var _ -> acc
      | Embed t -> tm_refs acc t
      | Rebind (p, p') -> pt_refs (pt_refs acc p) p'
      | Rec p -> pt_refs acc p

    let type_def ctx p_def t_def = function
      | Var x           -> type_apply [] (type_name ctx x ^ " New_name.t")
      | Embed t         -> type_apply [t_def ctx t]                "Embed.t"
      | Rebind (p1, p2) -> type_apply [p_def ctx p1; p_def ctx p2] "Rebind.t"
      | Rec p           -> type_apply [p_def ctx p]                "Rec.t"
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

  let rec tm_names acc = function
    | Tm_regular rg -> Regular.to_list rg |! List.fold ~init:acc ~f:tm_names
    | Tm tm -> Term.names tm_names pt_names acc tm

  and pt_names acc = function
    | Pt_regular rg -> Regular.to_list rg |! List.fold ~init:acc ~f:pt_names
    | Pt pt -> Pattern.names pt_names tm_names acc pt

  let rec tm_refs acc = function
    | Tm_regular rg -> Regular.refs tm_names acc rg
    | Tm tm -> Term.refs tm_refs pt_refs acc tm

  and pt_refs acc = function
    | Pt_regular rg -> Regular.refs pt_names acc rg
    | Pt pt -> Pattern.refs pt_refs tm_refs acc pt

  let rec tm_def ctx : tm -> Text_block.t = function
    | Tm_regular x -> Regular.type_def ctx tm_def x
    | Tm x -> Term.type_def ctx tm_def pt_def x

  and pt_def ctx : pt -> Text_block.t = function
    | Pt_regular x -> Regular.type_def ctx pt_def x
    | Pt x -> Pattern.type_def ctx pt_def tm_def x

  module Def = struct
    type 'a t =
    | Synonym of 'a
    | Variant of 'a list Constant.Map.t
    with sexp

    let to_list = function
      | Synonym x -> [x]
      | Variant map -> Map.data map |! List.concat

    let type_def ctx a_def = function
      | Synonym a -> a_def ctx a
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
                  (Text_block.text "of"
                   :: intersperse (Text_block.text "*") (List.map args ~f:(a_def ctx)))
            end;
          ]
        )
        |! Text_block.vcat
  end

  module Env = struct
    type t = {
      prelude : string list;
      tms : tm Def.t String.Map.t;
      pts : pt Def.t String.Map.t;
    } with sexp

    let names acc t =
      let aux names acc map =
        List.fold ~init:acc ~f:names begin
          let open List.Monad_infix in
          Map.data map
          >>= fun def ->
          Def.to_list def
        end
      in
      let acc = aux tm_names acc t.tms in
      let acc = aux pt_names acc t.pts in
      acc

    module Ref_sort = Scc.Make (String)

    let sccs t =
      let open List.Monad_infix in
      let defined = String.Set.of_list (Map.keys t.tms @ Map.keys t.pts) in
      let foo map refs =
        Map.to_alist map
        >>= fun (x, def) ->
        Def.to_list def
        >>= fun e ->
        refs e
        >>= fun y ->
        if Set.mem defined y then [(y, x)] else []
      in
      Ref_sort.scc
        (List.concat [
          foo t.tms (fun x -> Set.to_list (tm_refs String.Set.empty x));
          foo t.pts (fun x -> Set.to_list (pt_refs String.Set.empty x));
        ])

    let rec type_defs ?mode t =
      let open Text_block in
      let sccs = sccs t in
      let names = names String.Set.empty t in
      let ctx =
        Ctx.create begin
          Map.merge t.tms t.pts ~f:(fun ~key data ->
            let named = Set.mem names key in
            match data with
            | `Left _  -> Some (`Term,    named)
            | `Right _ -> Some (`Pattern, named)
            | `Both _ -> failwithf "multiple types named %s" key ()
          )
        end
      in
      match mode with
      | None ->
        let prelude = vcat (List.map ~f:text t.prelude) in
        vcat ~sep:space [
          prelude;
          vcat ~sep:space [
            text "module rec Self : sig";
            indent (type_defs ~mode:`Signature t);
            text "end = struct";
            indent (type_defs ~mode:`Structure t);
            text "end";
          ];
        ]
      | Some mode ->
        let gen ctx a_def foo def =
          vcat [
            text ("module " ^ foo ^ begin
              match mode with
              | `Signature -> " : sig"
              | `Structure -> " = struct"
            end);
            indent (vcat (List.filter_opt [
              Some (text ("type t ="));
              Some (indent (Def.type_def ctx a_def def));
              begin
                if not (Ctx.defined ctx foo) then None else
                  Some (Text_block.text begin
                    match mode with
                    | `Signature -> "module Name : New_name.S with type a := t"
                    | `Structure -> "module Name = New_name.Make (struct type a = t end)"
                  end)
              end;
            ]));
            text "end";
          ]
        in
        let w =
          let names = List.concat sccs in
          vcat ~sep:space begin
            List.fold names ~init:(ctx, []) ~f:(fun (ctx, ws) name ->
              let (ctx, w) =
                Ctx.while_visiting ctx name (fun ctx ->
                  match Ctx.type_of ctx name with
                  | Some `Term    -> gen ctx tm_def name (Map.find_exn t.tms name)
                  | Some `Pattern -> gen ctx pt_def name (Map.find_exn t.pts name)
                  | None -> assert false
                )
              in
              (ctx, w :: ws)
            )
            |! snd
            |! List.rev
          end
        in
        w

    let type_defs t = type_defs t

  end

end

module Run_time = struct

  module Bind = struct
    type ('a, 'b) t
  end

  module Embed = struct
    type 'a t
  end

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
