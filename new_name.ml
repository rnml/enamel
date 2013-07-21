open Std_internal

module Uid = Unique_id.Int (struct end)

module type T = sig
  type t with sexp_of
  include Comparable.S with type t := t
  val to_string : t -> string
  val swap : t * t -> t -> t
end

module Basic : sig
  type t with sexp
  include Stringable.S with type t := t
  include Comparable.S with type t := t
  val create : name:string -> stamp:int option -> t
  val preferred : t -> t
  val freshen : t -> t
end = struct

  module U = struct
    module T = struct
      type t = {
        name : string;
        stamp : int option;
      } with compare
      let hash = Hashtbl.hash
      let equal t1 t2 = (compare t1 t2 = 0)
      let to_string t =
        match t.stamp with
        | None -> t.name
        | Some i -> String.concat [t.name; "_"; Int.to_string i]
      let of_string x =
        match String.rsplit2 x ~on:'_' with
        | None -> { name = x; stamp = None }
        | Some (name, stamp) ->
          try
            let stamp = Some (Int.of_string stamp) in
            { name; stamp }
          with _ -> { name = x; stamp = None }
    end
    include T
    include Sexpable.Of_stringable (T)
  end
  include U
  include Comparable.Make (U)
  include Hashable.Make (U)

  module Weak = Caml.Weak.Make (T)

  let global : Weak.t = Weak.create 100

  let create ~name ~stamp =
    Option.iter stamp ~f:(fun s -> assert (Int.(>) s 0));
    let t = {name; stamp} in
    try Weak.find global t
    with Not_found -> Weak.add global t; t

  let succ t =
    let stamp = match t.stamp with None -> 0 | Some n -> n in
    let stamp = Some (1 + stamp) in
    create ~name:t.name ~stamp

  let rec freshen t =
    if Weak.mem global t
    then Weak.find global t
    else freshen (succ t)

  let preferred t = create ~name:t.name ~stamp:None

end

module Univ = struct
  module T = struct
    type t = {kind : Uid.t; basic : Basic.t} with compare, sexp
  end
  include T
  include Comparable.Make (T)
  let to_string t = Basic.to_string t.basic
  let swap (a, b) c =
    if (**) c = a then b
    else if c = b then a
    else c
  let freshen t = { t with basic = Basic.freshen t.basic }
end

type 'a t = Univ.t

module Registry = struct
  module Free_vars = struct
    module Term = Generic.Make (struct
      type 'a t = Univ.Set.t -> 'a -> Univ.Set.t
    end)
    module Pat = Generic.Make (struct
      type 'a t = Univ.Set.t -> 'a -> Univ.Set.t
    end)
  end
  module Swap = Generic.Make (struct
    type 'a t = Univ.t * Univ.t -> 'a -> 'a
  end)
end

module type S = sig
  type 'a name
  type a
  type t = a name with of_sexp
  val type_name : t Type.Name.t
  val type_rep : t Type.Rep.t
  val of_string : string -> t
  include T with type t := t
  val to_univ : t -> Univ.t
  val of_univ : Univ.t -> t option
  val cast    : _ name -> t
  val raw : string -> t
  val preferred : t -> t
end
  with type 'a name := 'a t

module Make (X : sig type a val name : string end) = struct
  let kind = Uid.create ()
  module U = struct
    include Univ
    let of_string x = { kind; basic = Basic.of_string x }
  end
  include U
  include Sexpable.Of_stringable (U)
  let to_univ u = u
  let of_univ u = if Uid.equal u.kind kind then Some u else None
  let cast t = { kind; basic = t.basic }

  let raw name = {kind; basic = Basic.create ~name ~stamp:None}
  let preferred t = {kind; basic = Basic.preferred t.basic}

  let type_name = Type.Name.create ~name:X.name
  let type_rep = Type.Rep.Abstract type_name

  include struct
    let () = Registry.Free_vars.Term.register type_name Univ.Set.add
    let () = Registry.Free_vars.Pat.register  type_name (fun s _ -> s)
    let () = Registry.Swap.register           type_name Univ.swap
  end

end
