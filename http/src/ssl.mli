open! Core
open Async

(** [upgrade_server_connection] performs TLS negotiation and if it succeeds, applies [f]
    to the new encrypted channels. When the deferred returned by [f] resolves, the TLS
    connection is shutdown, and the channels are closed. *)
val upgrade_server_connection
  :  ?version:Async_ssl.Version.t
  -> ?options:Async_ssl.Opt.t list
  -> ?name:string
  -> ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ]
  -> ?ca_file:string
  -> ?ca_path:string
  -> ?verify_modes:Async_ssl.Verify_mode.t list
  -> crt_file:string
  -> key_file:string
  -> f:
       (Async_ssl.Ssl.Connection.t
        -> Input_channel.t
        -> Output_channel.t
        -> unit Deferred.t)
  -> Input_channel.t
  -> Output_channel.t
  -> unit Deferred.t

val upgrade_client_connection
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
  -> f:
       (Async_ssl.Ssl.Connection.t
        -> Input_channel.t
        -> Output_channel.t
        -> unit Deferred.t)
  -> Input_channel.t
  -> Output_channel.t
  -> unit Deferred.t
