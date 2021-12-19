open Sexplib0.Sexp_conv

type t =
  { major : int
  ; minor : int
  }
[@@deriving sexp]

let to_string t = Printf.sprintf "HTTP/%d.%d" t.major t.minor
let v1_1 = { major = 1; minor = 1 }
