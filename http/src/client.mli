open! Core
open! Async
open! Shuttle

module Address : sig
  type t [@@deriving sexp_of]

  val of_host_and_port : Host_and_port.t -> t
  val of_unix_domain_socket : Filename.t -> t
end

module Ssl : sig
  type t [@@deriving sexp_of]

  (** ssl options that should be used when using a client over an encrypted connection.
      This can be used either when sending a {{!Shuttle_http.Client.Oneshot.call} one-shot
      request}, or when creating a client that supports keep-alive. If hostname is
      provided it'll be used for validating that the hostname in the peer's ssl
      certificate matches the hostname requested by the client. *)
  val create
    :  ?version:Async_ssl.Version.t
    -> ?options:Async_ssl.Opt.t list
    -> ?name:string
    -> ?hostname:string
    -> ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ]
    -> ?ca_file:string
    -> ?ca_path:string
    -> ?crt_file:string
    -> ?key_file:string
    -> ?verify_modes:Async_ssl.Verify_mode.t list
    -> ?session:Async_ssl.Ssl.Session.t
    -> ?verify_certificate:(Shuttle_ssl.ssl_connection -> unit Or_error.t)
    -> unit
    -> t
end

(** HTTP/1.1 client that supports keep-alives. A client entity can be created once with an
    {{!Shuttle_http.Client.Address.t} address} and re-used for multiple requests. The
    client is closed either when a user explicitly {{!Shuttle_http.Client.close} closes}
    it, or if there is an exception when performing a HTTP request using the client.

    It is the responsiblity of the user to check that a http call raised an exception and
    avoid using a connection once an exception is seen in a call. *)
type t [@@deriving sexp_of]

(** Initiate a TCP connection targeting the user provided Address and perform SSL
    handshake if needed. If an interrup deferred is provided the underlying socket is
    closed when it resolves. If address is a host + port pair the client will
    automatically populate the Host HTTP header for outgoing calls, and ensure that SNI
    and hostname validation is configured if using an SSL connection. *)
val create
  :  ?interrupt:unit Deferred.t
  -> ?connect_timeout:Time.Span.t
  -> ?ssl:Ssl.t
  -> Address.t
  -> t Deferred.Or_error.t

(** [Remote_connection_closed] is raised if attempting if an EOF is reached before the
    full response has been read. *)
exception Remote_connection_closed

(** [Request_aborted] is raised if attempting to enqueue a request within a closed http
    client. *)
exception Request_aborted

(** [call] Attempts to perform a HTTP request using the user provided client. If the
    response contains a "Connection: close" header or if there's an exception when
    performing the call the client will be closed and should not be used for any future
    calls. If performing multiple calls on a client, users should ensure to only wait on a
    response promise if all previous responses have been fully read. *)
val call : t -> Request.t -> Response.t Deferred.t

(** [is_closed] returns if the client has been closed. *)
val is_closed : t -> bool

(** [closed] returns a deferred that's resolved when the http client is closed. *)
val closed : t -> unit Deferred.t

(** [close] initiates shutdown for an http client. Any request that's currently in-flight
    will be attempt to run, and any pending requests will fail with
    {{:Shuttle.Client.Request_aborted} exception}. *)
val close : t -> unit Deferred.t

module Oneshot : sig
  (** [call] Performs a one-shot http client call to the user provided connection target.
      If ssl options are provided the client will attempt to setup a SSL connection. If
      ssl options contain a hostname then the client will perform hostname verification to
      ensure the hostnames on the peer's ssl certificate matches the hostname provided by
      the caller. To disable this check or to customize how the ssl certificate is
      validated users can provide their own implementation of [verify_certificate] when
      creating the {{!Shuttle_http.Client.Ssl.t} ssl} options. *)
  val call
    :  ?interrupt:unit Deferred.t
    -> ?connect_timeout:Time.Span.t
    -> ?ssl:Ssl.t
    -> Address.t
    -> Request.t
    -> Response.t Deferred.t
end
