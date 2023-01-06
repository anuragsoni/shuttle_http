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

(** [create ?error_handler ?read_header_timeout ?write_timeout ?time_source ~buf_len fd]
    creates a new server handle that can be used to drive the HTTP request/response server
    loop.

    - [error_handler] is an optional input that allows customizing how unhandled
      exceptions, and any potential parsing or i/o errors get rendered. The default
      implementation will attempt to send an HTTP response with a status code and an empty
      body.

    - [buf_len] is the initial size of the reader and writer buffers created from the user
      provided file descriptor. The buffered reader is used to buffer in-coming data is
      used to parse HTTP payloads.

    - [read_header_timeout] is the maximum time span that the server loop is allowed to
      read a request's headers. The default value is 60 seconds. If read_header_timeout is
      zero then no timeout is used, and the server could potentially wait forever
      attempting to read enough data to parse request headers.

    - [write_timeout] is the maximum time the buffer writer is allowed to wait when
      attempting to write data to the file descriptor.

    - [time_source] is optonal and defaults to wall-clock. *)
val create
  :  ?error_handler:error_handler
  -> ?read_header_timeout:Time_ns.Span.t
  -> ?write_timeout:Time_ns.Span.t
  -> ?time_source:[> read ] Time_source.T1.t
  -> buf_len:int
  -> Fd.t
  -> t

(** [closed t] returns a deferred that is resolved when the server connection handle is
    closed. *)
val closed : t -> unit Deferred.t

(** [close] shuts down the http connection. *)
val close : t -> unit

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
  -> Body.Stream.t
  -> Response.t
