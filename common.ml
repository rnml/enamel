
module Bind = struct
  type ('pat, 'term) t = 'pat * 'term with sexp
  let create p t = (p, t)
  let unbind _ = failwith "Bind.unbind unimplemented"
end

module Rebind = struct
  type ('pat1, 'pat2) t = 'pat1 * 'pat2 with sexp
  let create p1 p2 = (p1, p2)
end

module Embed = struct
  type 'term t = 'term with sexp
  let create x = x
end

module Rec = struct
  type 'pat t = 'pat with sexp
  let create x = x
end

