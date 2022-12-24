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
  [@@deriving sexp, compare, hash, enumerate]
end

include T
include Comparable.Make (T)

let of_string = function
  | "GET" -> Some `GET
  | "HEAD" -> Some `HEAD
  | "POST" -> Some `POST
  | "PUT" -> Some `PUT
  | "DELETE" -> Some `DELETE
  | "CONNECT" -> Some `CONNECT
  | "OPTIONS" -> Some `OPTIONS
  | "TRACE" -> Some `TRACE
  | "PATCH" -> Some `PATCH
  | _ -> None
;;

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
;;
