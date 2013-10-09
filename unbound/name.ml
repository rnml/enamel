open Std_internal

module Uid = Unique_id.Int (struct end)

module type T = sig
  type t with sexp_of
  include Comparable.S with type t := t
  val to_string : t -> string
  val freshen : t -> t
  module Perm : Perm.S with type elt := t
end

module Stamp = Unique_id.Int (struct end)

module Basic : sig
  type t
  include Identifiable with type t := t
  val create : name:string -> stamp:Stamp.t option -> t
  val preferred : t -> t
  val freshen : t -> t
end = struct

  module U = struct
    module T = struct

      let module_name = "Name"

      type t = {
        name : string;
        stamp : Stamp.t option;
      } with compare, bin_io

      let hash = Hashtbl.hash

      let equal t1 t2 = compare t1 t2 = 0

      let to_string t =
        match t.stamp with
        | None -> t.name
        | Some i -> String.concat [t.name; "_"; Stamp.to_string i]

      let of_string x =
        match String.rsplit2 x ~on:'_' with
        | None -> { name = x; stamp = None }
        | Some (name, stamp) ->
          try {name; stamp = Some (Stamp.of_string stamp)}
          with _ -> {name = x; stamp = None}

    end
    include T
    include Sexpable.Of_stringable (T)
  end
  include U
  include Identifiable.Make (U)

  let create ~name ~stamp = {name; stamp}

  let rec freshen t =
    create ~name:t.name ~stamp:(Some (Stamp.create ()))

  let preferred t =
    create ~name:t.name ~stamp:None

end

module Univ = struct
  module S = struct
    module U = struct
      let module_name = "Name.Univ"
      module T = struct
        type t = {
          kind : Uid.t;
          basic : Basic.t;
        } with compare, of_sexp, bin_io
        let sexp_of_t t = Basic.sexp_of_t t.basic
        let hash (t:t) = Hashtbl.hash t
      end
      include T
      include Sexpable.To_stringable (T)
    end
    include U
    include Identifiable.Make (U)
  end
  include S
  module Perm = Perm.Make (S)
  let to_string t = Basic.to_string t.basic
  let freshen t = { t with basic = Basic.freshen t.basic }
end

type 'a t = Univ.t with sexp

module Registry = struct
  module Free_vars = struct
    module Term = Generic.Make (struct
      type 'a t = Univ.Set.t -> 'a -> Univ.Set.t
    end)
    module Pat = Generic.Make (struct
      type 'a t = Univ.Set.t -> 'a -> Univ.Set.t
    end)
  end
  module Binders = Generic.Make (struct
    type 'a t = Univ.Set.t -> 'a -> Univ.Set.t
  end)
  module Swap = Generic.Make (struct
    type 'a t = Univ.Perm.t -> 'a -> 'a
  end)
end

module Rep =
  Type.Name.Make1 (struct
    let name = "Name"
    type nonrec 'a t = 'a t
  end)

let type_rep ty =
  let type_name = Rep.lookup (Type.Rep.id ty) in
  Registry.Free_vars.Term.register type_name Univ.Set.add;
  Registry.Free_vars.Pat.register type_name (fun s _ -> s);
  Registry.Binders.register type_name Univ.Set.add;
  Registry.Swap.register type_name Univ.Perm.apply;
  Type.Rep.Abstract type_name

module type S = sig
  type 'a name
  type a
  type t = a name with of_sexp
  include T with type t := t
  val of_string : string -> t
  val to_univ : t -> Univ.t
  val of_univ : Univ.t -> t option
  val cast    : _ name -> t
  val raw : string -> t
  val preferred : t -> t
  val create : string -> t
  val type_name : t Type.Name.t
  val type_rep : t Type.Rep.t
end
  with type 'a name := 'a t

module Make (X : sig
  type a
  val name : string
  val type_rep : a Type.Rep.t
end) = struct

  module V = struct
    include Univ
    let kind = Uid.create ()
    let of_string x = { kind; basic = Basic.of_string x }
  end
  include V
  include Sexpable.Of_stringable (V)

  let to_univ u = u
  let of_univ u = if Uid.equal u.kind kind then Some u else None

  let cast t = {kind; basic = t.basic}

  let raw name = {kind; basic = Basic.create ~name ~stamp:None}
  let preferred t = {kind; basic = Basic.preferred t.basic}

  let create name = freshen (raw name)

  let type_name = Type.Name.create ~name:X.name
  let type_rep = Type.Rep.Abstract type_name

  let () = Registry.Free_vars.Term.register type_name Univ.Set.add
  let () = Registry.Free_vars.Pat.register type_name (fun s _ -> s)
  let () = Registry.Binders.register type_name Univ.Set.add
  let () = Registry.Swap.register type_name Univ.Perm.apply

end
