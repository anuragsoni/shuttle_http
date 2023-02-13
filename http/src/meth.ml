open Core

module T = struct
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
  [@@deriving sexp, compare, hash, enumerate, quickcheck]
end

include T
include Comparable.Make (T)

let of_string = function
  | "GET" -> Ok `GET
  | "HEAD" -> Ok `HEAD
  | "POST" -> Ok `POST
  | "PUT" -> Ok `PUT
  | "DELETE" -> Ok `DELETE
  | "CONNECT" -> Ok `CONNECT
  | "OPTIONS" -> Ok `OPTIONS
  | "TRACE" -> Ok `TRACE
  | "PATCH" -> Ok `PATCH
  | meth -> Or_error.error "Invalid HTTP method" meth sexp_of_string

let to_string = function
  | `GET -> "GET"
  | `HEAD -> "HEAD"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `DELETE -> "DELETE"
  | `CONNECT -> "CONNECT"
  | `OPTIONS -> "OPTIONS"
  | `TRACE -> "TRACE"
  | `PATCH -> "PATCH"

let is_safe = function
  | `GET | `HEAD | `OPTIONS | `TRACE -> true
  | _ -> false

let is_idempotent = function
  | `PUT | `DELETE -> true
  | t -> is_safe t

let is_cacheable = function
  | `GET | `HEAD | `POST -> true
  | _ -> false
