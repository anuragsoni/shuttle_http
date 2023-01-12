open! Core
open! Async
open! Shuttle

module Ssl : sig
  type t [@@deriving sexp_of]

  val create
    :  ?version:Async_ssl.Version.t
    -> ?options:Async_ssl.Opt.t list
    -> ?name:string
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

val call : ?ssl:Ssl.t -> Host_and_port.t -> Request.t -> Response.t Deferred.Or_error.t
