(** [t] Represents a HTTP 1.1 request. *)
type t [@@deriving sexp_of]

val create
  :  ?version:Version.t
  -> ?headers:(string * string) list
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
val headers : t -> (string * string) list

(** [body] returns the body payload of this request. *)
val body : t -> Body.t

(** [with_body] returns a new request where every value is the same as the input request
    but the body is replaced with the function input. *)
val with_body : t -> Body.t -> t

val header : t -> string -> string option
val transfer_encoding : t -> [> `Bad_request | `Chunked | `Fixed of int ]
val keep_alive : t -> bool
val add_transfer_encoding : t -> [ `Chunked | `Fixed of int ] -> t
val iter_headers : t -> f:(key:string -> data:string -> unit) -> unit
val add_header_unless_exists : t -> key:string -> data:string -> t
val add_header : t -> key:string -> data:string -> t
val header_multi : t -> string -> string list
val remove_header : t -> string -> t
val header_exists : t -> string -> bool
