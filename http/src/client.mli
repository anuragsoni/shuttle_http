open! Core
open! Async
open! Shuttle

module Ssl : sig
  type t [@@deriving sexp_of]

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

(** [call] Performs a one-shot http client call to the user provided connection target. If
    ssl options are provided the client will attempt to setup a SSL connection. If ssl
    options contain a hostname then the client will perform hostname verification to
    ensure the hostnames on the peer's ssl certificate matches the hostname provided by
    the caller. To disable this check or to customize how the ssl certificate is validated
    users can provide their own implementation of [verify_certificate] when creating the
    {{!Shuttle_http.Client.Ssl.t} ssl} options. *)
val call
  :  ?ssl:Ssl.t
  -> 'address Tcp.Where_to_connect.t
  -> Request.t
  -> Response.t Deferred.Or_error.t
