open Async
open Shuttle

type ssl_handle =
  { input_channel : Input_channel.t
  ; output_channel : Output_channel.t
  ; shutdown : unit -> unit Deferred.t
  }

(** [server] creates an SSL server from the user provided input and output channels. Once
    the ssl handshake has completed, the user will be provided with a new pair of input
    and output channels, which should be used for all future IO. *)
val server
  :  ?version:Async_ssl.Version.t
  -> ?options:Async_ssl.Opt.t list
  -> ?name:string
  -> ?allowed_ciphers:[ `Only of string list | `Openssl_default | `Secure ]
  -> ?ca_file:string
  -> ?ca_path:string
  -> ?verify_modes:Async_ssl.Verify_mode.t list
  -> crt_file:string
  -> key_file:string
  -> Input_channel.t
  -> Output_channel.t
  -> ssl_handle Deferred.Or_error.t
