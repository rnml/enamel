open Core.Std

module type S = sig
  type 'a t
  val register  : 'a Type.Name.t -> 'a t -> unit
  val lookup : 'a Type.Name.t -> 'a t option
end

module Make (Data : T1) = struct

  type entry = Entry : 'a Type.Name.t * 'a Data.t -> entry

  let t = Type.Name.Uid.Table.create ~size:10 ()

  let register name data =
    let key = Type.Name.uid name in
    let data = Entry (name, data) in
    Hashtbl.set t ~key ~data

  module Lift = Type_equal.Lift (Data)

  let lookup (type a) (name : a Type.Name.t) : a Data.t option =
    Option.map (Hashtbl.find t (Type.Name.uid name)) ~f:(function
    | Entry (name', data) ->
      let eq = Type.Name.same_witness_exn name' name in
      Type_equal.conv (Lift.lift eq) data)

end

