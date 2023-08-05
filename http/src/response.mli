(** [t] Represents a HTTP 1.1 response. *)
type t = Response0.t [@@deriving sexp_of]

val create
  :  ?version:Version.t
  -> ?reason_phrase:string
  -> ?headers:(string * string) list
  -> ?body:Body.t
  -> Status.t
  -> t

val upgrade
  :  ?headers:Headers.t
  -> (?unconsumed_data:string -> Async_unix.Fd.t -> unit Async_kernel.Deferred.t)
  -> t

(** [version] returns the HTTP version number for the response. *)
val version : t -> Version.t

(** [status] returns the Status code for this response. *)
val status : t -> Status.t

(** [reason_phrase] returns the status reason phrase for the response. *)
val reason_phrase : t -> string

(** [headers] returns the HTTP headers for this response. *)
val headers : t -> (string * string) list

(** [body] returns the body payload of this response. *)
val body : t -> Body.t

(** [transfer_encoding] returns the inferred transfer encoding based on the response's
    http headers. *)
val transfer_encoding : t -> [> `Bad_response | `Chunked | `Fixed of int ]

(** [keep_alive] indicates whether the http connection should be reused.*)
val keep_alive : t -> bool

(** [add_transfer_encoding t encoding] adds transfer-encoding information to the response
    headers.*)
val add_transfer_encoding : t -> [ `Chunked | `Fixed of int ] -> t

(** [iter_headers t ~f] iterates over all response headers and forwards them to the user
    provided callback. *)
val iter_headers : t -> f:(key:string -> data:string -> unit) -> unit

(** [add_header_unless_exists t ~key ~data] returns a response with a new header added to
    it if the header isn't already present in the response. *)
val add_header_unless_exists : t -> key:string -> data:string -> t

(** [add_header t ~key ~data] returns a response with a new header added to it. *)
val add_header : t -> key:string -> data:string -> t

(** [header t key] returns [Some data] if [key] is found in the list of response headers.
    It returns [None] if the requested header isn't found.*)
val header : t -> string -> string option

(** [header_multi t key] returns a list of all values associated with the response header
    name. It returns an empty list if the requested header isn't found.*)
val header_multi : t -> string -> string list

(** [remove_header t key] removes all response headers that match the user provided key.*)
val remove_header : t -> string -> t

(** [header_exists t key] returns if a response header matches the user provided key. *)
val header_exists : t -> string -> bool
