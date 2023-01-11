open Core

(** Request method is used to indicate the purpose of a HTTP request.

    See {:https://datatracker.ietf.org/doc/html/rfc7231#section-4.3} for more details. *)
type t =
  [ `GET
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

val of_string : string -> t option
val to_string : t -> string

(** [is_safe t] returns true if the semantics for a HTTP method are essentially read-only,
    and the client does not expect any state change on the server as a result of the
    request.

    See {:https://datatracker.ietf.org/doc/html/rfc7231#section-4.2.1} for more details. *)
val is_safe : t -> bool

(** [is_idempotent t] returns true if multiple requests with a HTTP method are intended to
    have the same effect on the server as a single such request. This function returns
    true for PUT, DELETE and all safe methods.

    See {:https://datatracker.ietf.org/doc/html/rfc7231#section-4.2.2} for more details. *)
val is_idempotent : t -> bool

(** [is_cacheable t] indicates that responses to requests with an HTTP method are allowed
    to be strored for future reuse. This function returns true for GET, HEAD and POST.

    See {:https://datatracker.ietf.org/doc/html/rfc7231#section-4.2.3} for more details. *)
val is_cacheable : t -> bool

include Comparable.S with type t := t
