(** [t] Represents a HTTP 1.1 response. *)
type t [@@deriving sexp_of]

val create
  :  ?version:Version.t
  -> ?reason_phrase:string
  -> ?headers:(string * string) list
  -> ?body:Body.t
  -> Status.t
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

val with_body : t -> Body.t -> t
val transfer_encoding : t -> [> `Bad_response | `Chunked | `Fixed of int ]
val keep_alive : t -> bool
val add_transfer_encoding : t -> [ `Chunked | `Fixed of int ] -> t
val iter_headers : t -> f:(key:string -> data:string -> unit) -> unit
val add_header_unless_exists : t -> key:string -> data:string -> t
val add_header : t -> key:string -> data:string -> t
val header_multi : t -> string -> string list
val remove_header : t -> string -> t
val header_exists : t -> string -> bool
