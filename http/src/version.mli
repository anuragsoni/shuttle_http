type t =
  { major : int
  ; minor : int
  }
[@@deriving sexp]

val to_string : t -> string
val v1_1 : t
