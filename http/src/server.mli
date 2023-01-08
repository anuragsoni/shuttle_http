open! Core
open! Async
open! Shuttle

(** [t] represents a server connection handle. The lifecycle for the handle is the same as
    the underlying TCP connection. *)
type t [@@deriving sexp_of]

(** [error_handler] can be used to customize how the server deals with any unhandled
    exceptions. A default implementation is provided that will respond with a status code
    and an empty response body. *)
type error_handler = ?exn:exn -> ?request:Request.t -> Status.t -> Response.t Deferred.t

(** A user provided [service] that is invoked for every request/response cycle for a HTTP
    connection. *)
type service = Request.t -> Response.t Deferred.t

(** [create ?error_handler ?read_header_timeout reader writer] creates a new server handle
    that can be used to drive the HTTP request/response server loop.

    - [error_handler] is an optional input that allows customizing how unhandled
      exceptions, and any potential parsing or i/o errors get rendered. The default
      implementation will attempt to send an HTTP response with a status code and an empty
      body.

    - [read_header_timeout] is the maximum time span that the server loop is allowed to
      read a request's headers. The default value is 60 seconds. If read_header_timeout is
      zero then no timeout is used, and the server could potentially wait forever
      attempting to read enough data to parse request headers. *)
val create
  :  ?error_handler:error_handler
  -> ?read_header_timeout:Time_ns.Span.t
  -> Input_channel.t
  -> Output_channel.t
  -> t

(** [closed t] returns a deferred that is resolved when the server connection handle is
    closed. *)
val closed : t -> unit Deferred.t

(** [close] shuts down the http connection. *)
val close : t -> unit

(** [run t service] accepts a server handle and a user provided service that will be
    invoked for each run of the request/response loop. *)
val run : t -> (Request.t -> Response.t Deferred.t) -> unit Deferred.t
