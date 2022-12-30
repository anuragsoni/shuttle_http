(** [t] Represents a HTTP 1.1 response. *)
type t [@@deriving sexp_of]

val create
  :  ?version:Version.t
  -> ?reason_phrase:string
  -> ?headers:Headers.t
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
val headers : t -> Headers.t

(** [body] returns the body payload of this response. *)
val body : t -> Body.t

val set_headers : t -> Headers.t -> unit
