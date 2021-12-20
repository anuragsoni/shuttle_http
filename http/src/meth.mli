type t =
  [ (* https://datatracker.ietf.org/doc/html/rfc7231#section-4.3 *)
    `GET
  | `HEAD
  | `POST
  | `PUT
  | `DELETE
  | `CONNECT
  | `OPTIONS
  | `TRACE
  | (* https://datatracker.ietf.org/doc/html/rfc5789 *)
    `PATCH
  ]

val sexp_of_t : t -> Sexplib0.Sexp.t
val of_string : string -> t option
val to_string : t -> string
