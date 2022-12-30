(** [t] Represents a HTTP 1.1 request. *)
type t [@@deriving sexp_of]

val create
  :  ?version:Version.t
  -> ?headers:Headers.t
  -> ?body:Body.t
  -> Meth.t
  -> string
  -> t

(** [meth] returns the HTTP method of this request. *)
val meth : t -> Meth.t

(** [path] returns the path component and query parameters of the request URI *)
val path : t -> string

(** [version] returns the HTTP version number for the request. *)
val version : t -> Version.t

(** [headers] returns HTTP headers of this request. *)
val headers : t -> Headers.t

(** [body] returns the body payload of this request. *)
val body : t -> Body.t

(** [with_body] returns a new request where every value is the same as the input request
    but the body is replaced with the function input. *)
val with_body : t -> Body.t -> t

(** [with_headers] returns a new request where every value is the same as the input
    request but headers are replaced with the function input. *)
val with_headers : t -> Headers.t -> t
