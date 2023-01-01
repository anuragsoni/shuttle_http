open Core
open Async
open Shuttle

type ssl_connection

val peer_certificate : ssl_connection -> Async_ssl.Ssl.Certificate.t Or_error.t option
val ssl_session_resused : ssl_connection -> bool
val pem_peer_certificate_chain : ssl_connection -> string option

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
  -> f:(ssl_connection -> Input_channel.t -> Output_channel.t -> 'a Deferred.t)
  -> Input_channel.t
  -> Output_channel.t
  -> 'a Deferred.t

(** [listen] accepts new TCP connections, performs TLS negotiation and then calls [f] with
    the encrypted channels. Once the deferred returned by [f] resolves, the connection is
    shutdown. *)
val listen
  :  ?version:Async_ssl.Version.t
  -> ?options:Async_ssl.Opt.t list
  -> ?name:string
  -> ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ]
  -> ?ca_file:string
  -> ?ca_path:string
  -> ?verify_modes:Async_ssl.Verify_mode.t list
  -> ?max_connections:int
  -> ?max_accepts_per_batch:int
  -> ?backlog:int
  -> ?socket:([ `Unconnected ], ([< Socket.Address.t ] as 'address)) Socket.t
  -> crt_file:string
  -> key_file:string
  -> on_handler_error:[ `Call of 'address -> exn -> unit | `Ignore | `Raise ]
  -> ('address, 'listening_on) Tcp.Where_to_listen.t
  -> f:
       ('address
        -> ssl_connection
        -> Input_channel.t
        -> Output_channel.t
        -> unit Deferred.t)
  -> ('address, 'listening_on) Tcp.Server.t Deferred.t

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
  -> f:(ssl_connection -> Input_channel.t -> Output_channel.t -> 'a Deferred.t)
  -> Input_channel.t
  -> Output_channel.t
  -> 'a Deferred.t

val with_connection
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
  -> ?interrupt:unit Deferred.t
  -> ?timeout:Time.Span.t
  -> ?input_buffer_size:int
  -> ?output_buffer_size:int
  -> f:(ssl_connection -> Input_channel.t -> Output_channel.t -> 'a Deferred.t)
  -> [< Socket.Address.t ] Tcp.Where_to_connect.t
  -> 'a Deferred.t
