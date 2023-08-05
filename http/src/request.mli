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

(** [transfer_encoding] returns the inferred transfer encoding based on the request's http
    headers. *)
val transfer_encoding : t -> [> `Bad_request | `Chunked | `Fixed of int ]

(** [keep_alive] indicates whether the http connection should be reused.*)
val keep_alive : t -> bool

(** [add_transfer_encoding t encoding] adds transfer-encoding information to the request
    headers.*)
val add_transfer_encoding : t -> [ `Chunked | `Fixed of int ] -> t

(** [iter_headers t ~f] iterates over all request headers and forwards them to the user
    provided callback. *)
val iter_headers : t -> f:(key:string -> data:string -> unit) -> unit

(** [add_header_unless_exists t ~key ~data] returns a request with a new header added to
    it if the header isn't already present in the request. *)
val add_header_unless_exists : t -> key:string -> data:string -> t

(** [add_header t ~key ~data] returns a request with a new header added to it. *)
val add_header : t -> key:string -> data:string -> t

(** [header t key] returns [Some data] if [key] is found in the list of request headers.
    It returns [None] if the requested header isn't found.*)
val header : t -> string -> string option

(** [header_multi t key] returns a list of all values associated with the request header
    name. It returns an empty list if the requested header isn't found.*)
val header_multi : t -> string -> string list

(** [remove_header t key] removes all request headers that match the user provided key.*)
val remove_header : t -> string -> t

(** [header_exists t key] returns if a request header matches the user provided key. *)
val header_exists : t -> string -> bool

(** [replace_header] removes all response headers that match the user provided key and adds
    a new entry for the key with the new user provided data. *)
val replace_header : t -> key:string -> data:string -> t
