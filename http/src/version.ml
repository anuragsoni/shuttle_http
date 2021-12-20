type t =
  { major : int
  ; minor : int
  }

let sexp_of_t { major; minor } =
  let open Sexplib0 in
  Sexp.(
    List
      [ List [ Atom "major"; Sexp_conv.sexp_of_int major ]
      ; List [ Atom "minor"; Sexp_conv.sexp_of_int minor ]
      ])
;;

let to_string t = Printf.sprintf "HTTP/%d.%d" t.major t.minor
let v1_1 = { major = 1; minor = 1 }
