open! Core
open! Async
module Logger : Async_log.Global.S

(** [error_handler] can be used to customize how the server deals with any unhandled
    exceptions. A default implementation is provided that will respond with a status code
    and an empty response body. *)
type error_handler = ?exn:exn -> ?request:Request.t -> Status.t -> Response.t Deferred.t

module Ssl : sig
  type t [@@deriving sexp_of]

  (** ssl options that should be used when creating a https server. *)
  val create
    :  ?version:Async_ssl.Version.t
    -> ?options:Async_ssl.Opt.t list
    -> ?name:string
    -> ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ]
    -> ?ca_file:string
    -> ?ca_path:string
    -> ?verify_modes:Async_ssl.Verify_mode.t list
    -> certificate_file:string
    -> key_file:string
    -> unit
    -> t
end

module Config : sig
  type t [@@deriving sexp_of]

  val default : t

  (** HTTP Server configuration

      - [buf_len] is the buffer size used for the underlying tcp socket channel. The
        default value is 16_000 bytes.

      - [max_connections] is the maximum number of concurrent connections that can be
        active within a server. The default behavior is to have no upper bound on this
        number.

      - [max_accepts_per_batch] is the maximum number of socket connections that a server
        will attempt to accept in a single accept call. The default value is 1.

      - [backlog] is the number of clients that can have a pending connection. Additional
        connections can be rejected, enqueued or ignored based on the underlying operating
        system's behavior.

      - [write_timeout] is the maximum duration that the underlying socket will wait for
        any pending write syscalls to finish.

      - [read_header_timeout] is the maximum time span that the server loop is allowed to
        read a request's headers. The default value is 60 seconds. If read_header_timeout
        is zero then no timeout is used, and the server could potentially wait forever
        attempting to read enough data to parse request headers.

      - [error_handler] allows customizing how unhandled exceptions, and any potential
        parsing or i/o errors get rendered. The default implementation will attempt to
        send an HTTP response with a status code and an empty body. *)
  val create
    :  ?buf_len:int
    -> ?max_buffer_size:int
    -> ?max_connections:int
    -> ?max_accepts_per_batch:int
    -> ?backlog:int
    -> ?write_timeout:Time_ns.Span.t
    -> ?read_header_timeout:Time_ns.Span.t
    -> ?error_handler:error_handler
    -> ?ssl:Ssl.t
    -> unit
    -> t
end

type 'addr t [@@deriving sexp_of]

val peer_addr : 'addr t -> 'addr
val is_ssl : 'addr t -> bool
val ssl_peer_certificate : 'addr t -> Async_ssl.Ssl.Certificate.t Or_error.t option
val ssl_version : 'addr t -> Async_ssl.Ssl.Version.t option

(** A user provided [service] that is invoked for every request/response cycle for a HTTP
    connection. *)
type 'addr service = 'addr t -> Request.t -> Response.t Deferred.t

(** [run_inet ?config addr service] runs a http server where each request will be
    forwarded to the user provided service. *)
val run_inet
  :  ?config:Config.t
  -> Tcp.Where_to_listen.inet
  -> Socket.Address.Inet.t service
  -> Tcp.Server.inet

(** [run ?config addr service] runs a http server where each request will be forwarded to
    the user provided service. *)
val run
  :  ?config:Config.t
  -> ('address, 'listening_on) Tcp.Where_to_listen.t
  -> 'address service
  -> ('address, 'listening_on) Tcp.Server.t Deferred.t
