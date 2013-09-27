open Std_internal

include Identifiable
val pretty : t -> Pretty.doc
val type_rep : t Type.Rep.t
val type_rep_of_map : 'a Type.Rep.t -> 'a Map.t Type.Rep.t

