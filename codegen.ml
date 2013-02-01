open Core.Std

type sexp = Sexp.t = Atom of string | List of sexp list

type 'v s =
| Ref of string
| Var of 'v
| Lam of ('v -> 'v s)
| App of 'v s * (string * 'v s) list

let lam2 f = Lam (fun x1 -> Lam (fun x2 -> f x1 x2))
let lam3 f = Lam (fun x1 -> lam2 (fun x2 x3 -> f x1 x2 x3))
let lam4 f = Lam (fun x1 -> lam3 (fun x2 x3 x4 -> f x1 x2 x3 x4))

let app (v, vs) = App (v, List.map vs ~f:(fun v -> ("", v)))

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
         if String.equal label "" then
           [sexp_of_s n arg]
         else
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

type t = { closed : 'a. unit -> 'a s }

let sexp_of_t t = sexp_of_s (ref 0) (t.closed ())

