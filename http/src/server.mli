open! Core
open! Async
open! Shuttle

(** [error_handler] can be used to customize how the server deals with any unhandled
    exceptions. A default implementation is provided that will respond with a status code
    and an empty response body. *)
type error_handler = ?exn:exn -> ?request:Request.t -> Status.t -> Response.t Deferred.t

(** A user provided [service] that is invoked for every request/response cycle for a HTTP
    connection. *)
type service = Request.t -> Response.t Deferred.t

(** [t] represents a server connection handle. The lifecycle for the handle is the same as
    the underlying TCP connection. *)
type t [@@deriving sexp_of]

(** [closed t] returns a deferred that is resolved when the server connection handle is
    closed. *)
val closed : t -> unit Deferred.t

(** [create ?error_handler reader writer] creates a new server handle that can be used to
    drive the HTTP request/response server loop. *)
val create : ?error_handler:error_handler -> Input_channel.t -> Output_channel.t -> t

(** [run t service] accepts a server handle and a user provided service that will be
    invoked for each run of the request/response loop. *)
val run : t -> (Request.t -> Response.t Deferred.t) -> unit Deferred.t

(** [response_string] creates a new HTTP response from a string. *)
val respond_string
  :  t
  -> ?reason_phrase:string
  -> ?headers:Headers.t
  -> ?status:Status.t
  -> string
  -> Response.t

(** [respond_empty] creates a new HTTP response with an empty body. *)
val respond_empty
  :  t
  -> ?reason_phrase:string
  -> ?headers:Headers.t
  -> Status.t
  -> Response.t

(** [respond_stream] creates a new HTTP response from a user provided stream. If the
    stream remains unconsumed when a server handle is closed, the stream's [close]
    function is called so any resources help by the stream can be released. *)
val respond_stream
  :  t
  -> ?reason_phrase:string
  -> ?headers:Headers.t
  -> ?status:Status.t
  -> (module Stream_intf.S)
  -> Response.t
