open Std_internal

module type T = sig
  type t with sexp

  include Comparable.S with type t := t

  val map_domain : _ Map.t -> Set.t

  val dummy : t
  val raw : string -> t
  val succ : t -> t
  val preferred : t -> t
  val freshen : t -> t

  val next : t -> not_in:Set.t -> t

  val to_string : t -> string
  val pretty : t -> Pretty.doc
  val swap : t * t -> t -> t
end

module M : sig
  type t = private {
    name : string option;
    stamp : int option;
  }
  include Comparable.S with type t := t
  include Hashable.S with type t := t
  val create : name:string option -> stamp:int option -> t
  val succ : t -> t
  val freshen : t -> t
end = struct
  module T = struct

    type t = {
      name : string option;
      stamp : int option;
    } with compare, sexp

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash = Hashtbl.hash
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  module Weak = Weak.Make (T)

  let global = Weak.create 100

  let create ~name ~stamp =
    let t = {name; stamp} in
    try Weak.find global t
    with Not_found -> Weak.add global t; t

  let succ t =
    let stamp =
      Some (1 + match t.stamp with None -> 0 | Some n -> n)
    in
    create ~name:t.name ~stamp

  let rec freshen t =
    if Weak.mem global t
    then Weak.find global t
    else freshen (succ t)

end

module T = struct
  include M
  let map_domain t =
    Map.fold t ~init:Set.empty
      ~f:(fun ~key ~data:_ set -> Set.add set key)

  let make name = create ~name ~stamp:None
  let dummy = make None
  let raw name = make (Some name)
  let preferred t = make t.name

  let sep = "_"

  let to_string t =
    match t.stamp with
    | None ->
      (match t.name with
      | None -> sep
      | Some x -> x)
    | Some i ->
      let suffix = sep ^ Int.to_string i in
      (match t.name with
      | None -> suffix
      | Some x -> x ^ suffix)

  let of_string x =
    let stamp = None in
    let name = None in
    if String.equal x sep then create ~stamp ~name else
    match String.rsplit2 x ~on:'_' with
    | None -> create ~stamp ~name:(Some x)
    | Some (name, stamp) ->
      let stamp = Some (Int.of_string stamp)  in
      let name =
        if String.is_empty name then None else Some name
      in
      create ~name ~stamp

  let pretty t = Pretty.text (to_string t)

  let next t ~not_in:ts =
    let rec loop t =
      if Set.mem ts t then loop (succ t) else t
    in
    loop t

  let swap (a, b) c =
    if equal c a then b else
    if equal c b then a else c

end
include T

module type S = sig
  include T
  type name
  val to_name : t -> name
  val of_name : name -> t
end
  with type name := t

module Make (X : sig end) : S = struct
  include T
  let to_name x = x
  let of_name x = x
end


