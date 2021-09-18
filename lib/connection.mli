open! Core
open! Async

val listen
  :  ?max_connections:int
  -> ?max_accepts_per_batch:int
  -> ?backlog:int
  -> ?socket:([ `Unconnected ], ([< Socket.Address.t ] as 'address)) Socket.t
  -> on_handler_error:[ `Call of 'address -> exn -> unit | `Ignore | `Raise ]
  -> ('address, 'listening_on) Tcp.Where_to_listen.t
  -> ('address -> Input_channel.t -> Output_channel.t -> unit Deferred.t)
  -> ('address, 'listening_on) Tcp.Server.t Deferred.t
