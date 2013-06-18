open Core.Std
open Core_extended.Std

type sexp = Sexp.t = Atom of string | List of sexp list

module rec Term : sig
  type 'v s =
  | Ref of string (* free var *)
  | Var of 'v     (* bound var *)
  | Lam of ('v -> 'v s)
  | App of 'v s * (string option * 'v s) list
  | Match_tuple of 'v s * int * ('v list -> 'v s)
  | Struct of 'v Decl.s list
  | Sig    of 'v Decl.s list
  | Module of 'v Decl.s list * string

  val sexp_of_s : int ref -> sexp s -> sexp

  val lam2 : ('v -> 'v -> 'v s) -> 'v s
  val lam3 : ('v -> 'v -> 'v -> 'v s) -> 'v s
  val lam4 : ('v -> 'v -> 'v -> 'v -> 'v s) -> 'v s

  val app : 'v s * 'v s list -> 'v s

  type t = { closed : 'v. unit -> 'v s } with sexp_of

end = struct

  type 'v s =
  | Ref of string
  | Var of 'v
  | Lam of ('v -> 'v s)
  | App of 'v s * (string option * 'v s) list
  | Match_tuple of 'v s * int * ('v list -> 'v s)
  | Struct of 'v Decl.s list
  | Sig    of 'v Decl.s list
  | Module of 'v Decl.s list * string

  let lam2 f = Lam (fun x1 -> Lam (fun x2 -> f x1 x2))
  let lam3 f = Lam (fun x1 -> lam2 (fun x2 x3 -> f x1 x2 x3))
  let lam4 f = Lam (fun x1 -> lam3 (fun x2 x3 x4 -> f x1 x2 x3 x4))

  let app (v, vs) = App (v, List.map vs ~f:(fun v -> (None, v)))

  let a_char_int = Char.to_int 'a'

  let var_of_int i =
    let q = i / 26 in
    let r = i % 26 in
    let s = String.of_char (Char.of_int_exn (a_char_int + r)) in
    if q = 0 then s else s ^ Int.to_string q

  let rec sexp_of_s n = function
    | Ref x -> Atom x
    | Var sexp -> sexp
    | App (f, xs) ->
      List
        (sexp_of_s n f
         :: List.concat_map xs ~f:(fun (label, arg) ->
           match label with
           | None -> [sexp_of_s n arg]
           | Some label ->
             [Atom ("~" ^ label ^ ":"); List[sexp_of_s n arg]]
         ))
    | Lam _ as lam ->
      let rec loop acc = function
        | Lam f ->
          let x = Atom (var_of_int !n) in
          incr n;
          loop (x :: acc) (f x)
        | body ->
          List (List.concat [
            [Atom "fun"];
            List.rev acc;
            [Atom "->"];
            [sexp_of_s n body]
          ])
      in
      loop [] lam
    | Match_tuple (e, m, cs) ->
      let e = sexp_of_s n e in
      let vs =
        List.init m ~f:(fun _ ->
          let v = Atom (var_of_int !n) in
          incr n;
          v)
      in
      let body = sexp_of_s n (cs vs) in
      List [Atom "let"; List vs; Atom "="; e; Atom "in"; body]
    | Struct ds ->
      List (Atom "struct" :: List.map ds ~f:(Decl.sexp_of_s n))
    | Sig ds ->
      List (Atom "sig" :: List.map ds ~f:(Decl.sexp_of_s n))
    | Module (ds, sig_name) ->
      List (List.concat [
        [Atom "module"];
        List.map ds ~f:(Decl.sexp_of_s n);
        [Atom ":"; Atom sig_name];
      ])

  type t = { closed : 'a. unit -> 'a s }

  let sexp_of_t t = sexp_of_s (ref 0) (t.closed ())

end

and Decl : sig
  type kind = Value | Type | Module | Signature
  type 'v s = kind * string * 'v Term.s option
  val sexp_of_s : int ref -> sexp s -> sexp
  type t = { closed : 'v. unit -> 'v s } with sexp_of
end = struct
  type kind = Value | Type | Module | Signature with sexp_of
  type 'v s = kind * string * 'v Term.s option
  let sexp_of_s n (kind, x, term) =
    List (List.filter_opt [
      Some (sexp_of_kind kind);
      Some (String.sexp_of_t x);
      Option.map term ~f:(Term.sexp_of_s n);
    ])
  type t = { closed : 'v. unit -> 'v s } with sexp_of
  let sexp_of_t t = sexp_of_s (ref 0) (t.closed ())
end


module Type = struct
  type t =
    | Int
    | Char
    | Float
    | String
    | Bool
    | Unit
    | Option of t
    | List of t
    | Array of t
    | Lazy of t
    | Ref of t
    | Pair of t * t
    | Triple of t * t * t
    | Record of (string * t) list
    | Variant of (string * t) list
    | Name of string
  with sexp

  let rec fvs =
    let (+) = String.Set.union in
    function
    | Int | Char | Float | String | Bool | Unit -> String.Set.empty
    | Option t | List t | Array t | Lazy t | Ref t -> fvs t
    | Pair (t1, t2) -> fvs t1 + fvs t2
    | Triple (t1, t2, t3) -> fvs t1 + fvs t2 + fvs t3
    | Record alist
    | Variant alist ->
      List.fold alist ~init:String.Set.empty
        ~f:(fun acc (_, t) -> acc + fvs t)
    | Name x -> String.Set.singleton x

  module Scc = Scc.Make (String)

  module Text = Text_block

  let hwords ws = Text.hcat ~sep:(Text.hstrut 1) ws

  let rec render = function
    | Int    -> Text.text "int"
    | Char   -> Text.text "char"
    | Float  -> Text.text "float"
    | String -> Text.text "string"
    | Bool   -> Text.text "bool"
    | Unit   -> Text.text "unit"
    | Option t -> hwords [render t; Text.text "option"]
    | List t   -> hwords [render t; Text.text "list"]
    | Array t  -> hwords [render t; Text.text "array"]
    | Lazy t   -> hwords [render t; Text.text "Lazy.t"]
    | Ref t    -> hwords [render t; Text.text "rev"]
    | Pair (a, b) ->
      Text.hcat (
        [Text.text "("]
        @ List.intersperse ~sep:(Text.text " * ")
          [render a; render b]
        @ [Text.text ")"]
      )
    | Triple (a, b, c) ->
      Text.hcat (
        [Text.text "("]
        @ List.intersperse ~sep:(Text.text " * ")
          [render a; render b; render c]
        @ [Text.text ")"]
      )
    | Name x -> Text.text x
    | Variant alist ->
      Text.vcat (List.map alist ~f:(fun (c, arg) ->
        hwords [
          Text.text "|";
          Text.text c;
          Text.text "of";
          render arg
        ]))
    | Record alist ->
      Text.vcat [
        Text.text "{";
        Text.hcat [
          Text.hstrut 2;
          Text.vcat (List.map alist ~f:(fun (f, fty) ->
            hwords
              [Text.text f; Text.hcat [render fty; Text.text ";"]]
          ));
        ];
        Text.text "}";
      ]

  let _render alist =
    let map = String.Map.of_alist_exn alist in
    let edges =
      List.concat_map alist ~f:(fun (x, t) ->
        let ys = Set.to_list (fvs t) in
        List.map ys ~f:(fun y -> (x, y)))
    in
    Text.vcat
      (List.map (Scc.scc edges) ~f:(fun group ->
        Text.vcat
          (List.map group ~f:(fun x ->
            let t : t = Map.find_exn map x in
            hwords [
              Text.text "type";
              Text.text x;
              Text.text "=";
              render t;
            ]
           ))
       ))

end
