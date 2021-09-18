open Async
open Shuttle

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
  -> f:(Input_channel.t -> Output_channel.t -> 'a Deferred.t)
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
  -> f:('address -> Input_channel.t -> Output_channel.t -> unit Deferred.t)
  -> ('address, 'listening_on) Tcp.Server.t Deferred.t
