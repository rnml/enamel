open Std_internal

open Or_error.Monad_infix

module Label = Syntax0.Label

module F = struct

  module Kind = struct
    include Syntax0.F.Kind

    let rec sexp_of_t : t -> Sexp.t = function
      | Type -> Atom "*"
      | Fun (a, b) ->
        let rec loop acc : t -> Sexp.t = function
          | Fun (a, b) -> loop (a :: acc) b
          | b -> List (Atom "fun" :: List.rev_map ~f:sexp_of_t (b :: acc))
        in
        loop [a] b

    let rec t_of_sexp : Sexp.t -> t = function
      | Atom "*" -> Type
      | List (Atom "fun" :: a :: b :: args) ->
        Fun ( t_of_sexp a
            , let rec loop b args =
              match args with
              | [] -> t_of_sexp b
              | c :: args -> Fun (t_of_sexp b, loop c args)
              in loop b args)
      | sexp -> of_sexp_error "Kind.t_of_sexp" sexp
  end

  module Type = struct

    include Syntax0.F.Type

    let rec sexp_of_t (t : t) : Sexp.t =
      match match_ t with
      | Name x -> Name.sexp_of_t x
      | Fun (a, b) ->
        let rec loop acc t : Sexp.t =
          match match_ t with
          | Fun (a, b) -> loop (a :: acc) b
          | _ -> List (Atom "fun" :: List.rev_map ~f:sexp_of_t (t :: acc))
        in
        loop [a] b
      | Record map ->
        List ( Sexp.Atom "record"
               :: (Map.to_alist map
               |> List.map ~f:<:sexp_of< Label.t * t >>))
      | Forall (x, k, b) ->
        let rec loop acc t : Sexp.t =
          match match_ t with
          | Forall (x, k, b) -> loop ((x, k) :: acc) b
          | _ ->
            List (Atom "forall"
                  :: List.rev (sexp_of_t t
                               :: List.map acc
                                    ~f:<:sexp_of< Name.t * Kind.t >>))
        in
        loop [(x, k)] b
      | Exists (x, k, b) ->
        let rec loop acc t : Sexp.t =
          match match_ t with
          | Exists (x, k, b) -> loop ((x, k) :: acc) b
          | _ ->
            List (Atom "exists"
                  :: List.rev (sexp_of_t t
                               :: List.map acc
                                    ~f:<:sexp_of< Name.t * Kind.t >>))
        in
        loop [(x, k)] b
      | Lambda (x, k, b) ->
        let rec loop acc t : Sexp.t =
          match match_ t with
          | Lambda (x, k, b) -> loop ((x, k) :: acc) b
          | _ ->
            List (Atom "lambda"
                  :: List.rev (sexp_of_t t
                               :: List.map acc
                                    ~f:<:sexp_of< Name.t * Kind.t >>))
        in
        loop [(x, k)] b
      | App (a, b) ->
        let rec loop t args : Sexp.t =
          match match_ t with
          | App (a, b) -> loop a (b :: args)
          | _ -> List (List.map ~f:sexp_of_t (t :: args))
        in
        loop a [b]

    let rec t_of_sexp (s : Sexp.t) : t =
      match s with
      | Atom _ -> create (Name (Name.t_of_sexp s))
      | List (Atom "fun" :: a :: b :: cs) ->
        let a = t_of_sexp a in
        let b = t_of_sexp b in
        let rec loop b cs =
          match cs with
          | [] -> b
          | c :: ds ->
            let c = t_of_sexp c in
            create (Fun (b, loop c ds))
        in
        create (Fun (a, loop b cs))
      | List (Atom "record" :: alist) ->
        create (Record
                   (Label.Map.of_alist_exn
                    @@ List.map alist ~f:<:of_sexp< Label.t * t >>))
      | List (Atom "forall" :: param :: rest) as sexp ->
        begin
          let (x, k) = <:of_sexp< Name.t * Kind.t >> param in
          match List.rev rest with
          | [] -> of_sexp_error "F.Type.t_of_sexp" sexp
          | last :: args ->
            create (Forall (x, k, begin
              let last = t_of_sexp last in
              List.fold args ~init:last ~f:(fun acc param ->
                let (x, kind) = <:of_sexp< Name.t * Kind.t >> param in
                create (Forall (x, kind, acc)))
            end))
        end
      | List (Atom "exists" :: param :: rest) as sexp ->
        begin
          let (x, k) = <:of_sexp< Name.t * Kind.t >> param in
          match List.rev rest with
          | [] -> of_sexp_error "F.Type.t_of_sexp" sexp
          | last :: args ->
            create (Exists (x, k, begin
              let last = t_of_sexp last in
              List.fold args ~init:last ~f:(fun acc param ->
                let (x, k) = <:of_sexp< Name.t * Kind.t >> param in
                create (Exists (x, k, acc)))
            end))
        end
      | List (Atom "lambda" :: param :: rest) as sexp ->
        begin
          let (x, k) = <:of_sexp< Name.t * Kind.t >> param in
          match List.rev rest with
          | [] -> of_sexp_error "F.Type.t_of_sexp" sexp
          | last :: args ->
            create (Lambda (x, k, begin
              let last = t_of_sexp last in
              List.fold args ~init:last ~f:(fun acc param ->
                let (x, k) = <:of_sexp< Name.t * Kind.t >> param in
                create (Lambda (x, k, acc)))
            end))
        end
      | List (head :: args) ->
        let head = t_of_sexp head in
        let args = List.map args ~f:t_of_sexp in
        List.fold ~init:head args ~f:(fun acc arg -> create (App (acc, arg)))
      | sexp -> of_sexp_error "F.Type.t_of_sexp" sexp
  end

  module Term = struct

    include Syntax0.F.Term

    let rec sexp_of_t t =
      match match_ t with
      | Name x -> Name.sexp_of_t x
      | Lambda (a, b, c) -> gather_params_term a b c []
      | Tyfun  (a, b, c) -> gather_params_type  a b c []
      | App    (a, b)    -> gather_args_term     a b   []
      | Tyapp  (a, b)    -> gather_args_type     a b   []
      | Record a ->
        let a = Map.to_alist a in
        let a = List.map a ~f:<:sexp_of< Label.t * t >> in
        List (Atom "record" :: a)
      | Dot (a, b) ->
        let a = sexp_of_t a in
        let b = Label.sexp_of_t b in
        List [Atom "dot"; a; b]
      | Pack (a, b, c, d) ->
        let a = Type.sexp_of_t      a in
        let b = sexp_of_t           b in
        let c = Type.Name.sexp_of_t c in
        let d = Type.sexp_of_t      d in
        List [Atom "pack"; a; b; List [Atom "exists"; c; d]]
      | Unpack (a, b, c, d) ->
        let a = Type.Name.sexp_of_t a in
        let b = Name.sexp_of_t      b in
        let c = sexp_of_t           c in
        let d = sexp_of_t           d in
        List [Atom "unpack"; List [a; b; c]; d]
      | Let (a, b, c) ->
        let a = Name.sexp_of_t a in
        let b = sexp_of_t      b in
        let c = sexp_of_t      c in
        List [Atom "let"; List [a; b]; c]

    and gather_params t acc : Sexp.t =
      match match_ t with
      | Lambda (a, b, c) -> gather_params_term a b c acc
      | Tyfun  (a, b, c) -> gather_params_type a b c acc
      | _                -> List [Atom "lambda"; List (List.rev acc); sexp_of_t t]

    and gather_params_term a b c acc =
      gather_params c
        (Sexp.List [Name.sexp_of_t a; Type.sexp_of_t b] :: acc)

    and gather_params_type a b c acc =
      gather_params c
        (Sexp.List [Atom "type"; Type.Name.sexp_of_t a; Kind.sexp_of_t b] :: acc)

    and gather_args t acc : Sexp.t =
      match match_ t with
      | App   (a, b) -> gather_args_term a b acc
      | Tyapp (a, b) -> gather_args_type a b acc
      | _            -> List (sexp_of_t t :: acc)

    and gather_args_term t arg acc =
      gather_args t (sexp_of_t arg :: acc)

    and gather_args_type t arg acc =
      gather_args t (List [Atom "type"; Type.sexp_of_t arg] :: acc)

    let copy_list xs = List.fold_right ~init:[] ~f:(fun x xs -> x :: xs) xs

    let rec t_of_sexp (s : Sexp.t) =
        match s with
        | List (Atom "record" :: a) ->
          let a = List.map a ~f:<:of_sexp< Label.t * t >> in
          let a = Label.Map.of_alist_exn a in
          create @@ Record a
        | List [Atom "dot"; a; b] ->
          let a = t_of_sexp a in
          let b = Label.t_of_sexp b in
          create @@ Dot (a, b)
        | List [Atom "pack"; a; b; List [Atom "exists"; c; d]] ->
          let a = Type.t_of_sexp      a in
          let b = t_of_sexp           b in
          let c = Type.Name.t_of_sexp c in
          let d = Type.t_of_sexp      d in
          create @@ Pack (a, b, c, d)
        | List [Atom "unpack"; List [a; b; c]; d] ->
          let a = Type.Name.t_of_sexp a in
          let b = Name.t_of_sexp      b in
          let c = t_of_sexp           c in
          let d = t_of_sexp           d in
          create @@ Unpack (a, b, c, d)
        | List [Atom "let"; List [a; b]; c] ->
          let a = Name.t_of_sexp a in
          let b = t_of_sexp      b in
          let c = t_of_sexp      c in
          create @@ Let (a, b, c)
        | List [Atom "lambda"; List a; b] ->
          let b = t_of_sexp b in
          List.fold_right ~init:b a ~f:(fun s acc ->
            match s with
            | List [Atom "type"; a; b] ->
              let a = Type.Name.t_of_sexp a in
              let b = Kind.t_of_sexp b in
              create @@ Tyfun (a, b, acc)
            | List [a; b] ->
              let a = Name.t_of_sexp a in
              let b = Type.t_of_sexp b in
              create @@ Lambda (a, b, acc)
            | _ -> of_sexp_error "F.Term.t_of_sexp param" s)
        | List (a :: b :: c) ->
          let a = t_of_sexp a in
          List.fold_left ~init:a (b :: c) ~f:(fun acc s ->
            match s with
            | List [Atom "type"; a] ->
              let a = Type.t_of_sexp a in
              create @@ Tyapp (acc, a)
            | a ->
              let a = t_of_sexp a in
              create @@ App (acc, a))
        | Atom _ -> create @@ Name (Name.t_of_sexp s)
        | List _ -> of_sexp_error "F.Term.t_of_sexp" s
  end

end

module Target = struct

  module T = Syntax0.Target

  let rec sexp_of_asig t : Sexp.t =
    match T.Asig.match_ t with
    | Exists (params, csig) ->
      List [
        Atom "exists";
        <:sexp_of< (F.Type.Name.t * F.Kind.t) list >> params;
        sexp_of_csig csig;
      ]

  and sexp_of_csig t : Sexp.t =
    match T.Csig.match_ t with
    | Val a -> List [Atom "val"; F.Type.sexp_of_t a]
    | Type (a, b) -> List [Atom "type"; F.Type.sexp_of_t a; F.Kind.sexp_of_t b]
    | Sig a -> List [Atom "sig"; sexp_of_asig a]
    | Struct a -> List (Atom "struct" :: List.map (Map.to_alist a) ~f:<:sexp_of< Label.t * csig >>)
    | Forall (params, a, b) ->
      List [
        Atom "forall";
        <:sexp_of< (F.Type.Name.t * F.Kind.t) list >> params;
        sexp_of_csig a;
        sexp_of_asig b;
      ]

  module Csig = struct
    include T.Csig
    let sexp_of_t = sexp_of_csig
  end
  module Asig = struct
    include T.Asig
    let sexp_of_t = sexp_of_asig
  end
end
