include Core.Std
include Int.Replace_polymorphic_compare

include struct
  open Typerep_kernel.Std
  module Type_generic = Type_generic
  module Variant_and_record_make = Typerep_kernel.Variant_and_record_intf.M
end

module Generic = Type_generic.Make (struct
  module T = struct
    type 'a input = unit
    type 'a output = 'a Type.Rep.t
    type 'a t = unit -> 'a Type.Rep.t
  end
  include T
  include Variant_and_record_make (T)
  module Named = Type_generic.Make_named_for_closure (T)

  let name = "of_typerep"
  let required = []

  let int () = Type.Rep.Int
  let int32 () = assert false
  let int64 () = assert false
  let nativeint () = assert false
  let char () = Type.Rep.Char
  let float () = Type.Rep.Float
  let string () = Type.Rep.String
  let bool () = Type.Rep.Bool
  let unit () = Type.Rep.Unit
  let list a () = Type.Rep.List (a ())
  let array _ = assert false
  let lazy_t a () = Type.Rep.Lazy (a ())
  let ref_ _ () = assert false
  let option a () = Type.Rep.Option (a ())
  let function_ _ _ () = assert false
  let tuple2 a b () = Type.Rep.Pair (a (), b ())
  let tuple3 a b c () = Type.Rep.Triple (a (), b (), c ())
  let tuple4 _ _ _ _ () = assert false
  let tuple4 _ _ _ _ _ () = assert false

  let variant (type a) (v : a Variant.t) () : a Type.Rep.t =
    let module X = struct
      type 'args tag = (a, 'args) Tag.t
      module Label = struct
        type 'a t = 'a tag
        let name_of = Tag.label
        let type_of tag = Tag.traverse tag ()
        type univ = Label : 'a tag -> univ
        let all = Variant.fold v ~init:[] ~f:(fun acc (Variant.Tag t) -> Label t :: acc)
      end
      type rep = Tagged : 'a tag * 'a -> rep
      type t = a
      let name : t Type_equal.Id.t = Type_equal.Id.create ~name:"variant" <:sexp_of<_>>
      let put : type a. a tag -> a -> t =
        fun tag args ->
          match Tag.create tag with
          | Tag.Args f -> f args
          | Tag.Const c -> c
      let inject (Tagged (tag, args)) = put tag args
      let project x =
        match Variant.value v x with
        | Variant.Value (tag, args) -> Tagged (tag, args)
    end in
    Type.Rep.Variant (module X)

  let record (type a) (r : a Record.t) () : a Type.Rep.t =
    let module X = struct
      type 'args field = (a, 'args) Field.t
      type rep = { lookup : 'a. 'a field -> 'a }
      type t = a
      let name : t Type_equal.Id.t = Type_equal.Id.create ~name:"record" <:sexp_of<_>>
      let get = Field.get
      let project t = { lookup = fun field -> get field t }
      let inject {lookup} = Record.create r {Record.get = lookup}
      module Label = struct
        type 'a t = 'a field
        let name_of = Field.label
        let type_of field = Field.traverse field ()
        type univ = Label : 'a t -> univ
        let all = Record.fold r ~init:[] ~f:(fun acc (Record.Field f) -> Label f :: acc)
      end
    end in
    Type.Rep.Record (module X)

  let tuple4 _ = assert false
  let tuple5 _ = assert false
end)

let of_typerep typerep =
  match Generic.of_typerep typerep with
  | `generic f -> f ()
