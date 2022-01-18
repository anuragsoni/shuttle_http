open! Core
open! Async

val listen
  :  ?max_connections:int
  -> ?max_accepts_per_batch:int
  -> ?backlog:int
  -> ?socket:([ `Unconnected ], ([< Socket.Address.t ] as 'address)) Socket.t
  -> ?input_buffer_size:int
  -> ?output_buffer_size:int
  -> on_handler_error:[ `Call of 'address -> exn -> unit | `Ignore | `Raise ]
  -> f:('address -> Input_channel.t -> Output_channel.t -> unit Deferred.t)
  -> ('address, 'listening_on) Tcp.Where_to_listen.t
  -> ('address, 'listening_on) Tcp.Server.t Deferred.t

val with_connection
  :  ?interrupt:unit Deferred.t
  -> ?timeout:Time.Span.t
  -> ?input_buffer_size:int
  -> ?output_buffer_size:int
  -> f:(Input_channel.t -> Output_channel.t -> 'res Deferred.t)
  -> [< Socket.Address.t ] Tcp.Where_to_connect.t
  -> 'res Async_kernel__Types.Deferred.t
