
module Bind = struct
  type ('pat, 'term) t = 'pat * 'term with sexp
  let create p t = (p, t)
  let unbind _ = failwith "Bind.unbind unimplemented"
  module Type_name = Type.Name.Make2 (struct type nonrec ('p, 't) t = ('p, 't) t end)
  let type_name = Type_name.lookup
end

module Rebind = struct
  type ('pat1, 'pat2) t = 'pat1 * 'pat2 with sexp
  let create p1 p2 = (p1, p2)
  module Type_name = Type.Name.Make2 (struct type nonrec ('p1, 'p2) t = ('p1, 'p2) t end)
  let type_name = Type_name.lookup
end

module Embed = struct
  type 'term t = 'term with sexp
  let create x = x
  module Type_name = Type.Name.Make1 (struct type nonrec 't t = 't t end)
  let type_name = Type_name.lookup
end

module Rec = struct
  type 'pat t = 'pat with sexp
  let create x = x
  module Type_name = Type.Name.Make1 (struct type nonrec 'p t = 'p t end)
  let type_name = Type_name.lookup
end

