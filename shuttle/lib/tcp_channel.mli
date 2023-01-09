open! Core
open! Async

(** [listen] is a wrapper for [Async.Tcp.Server.create_sock]. It uses async to setup a tcp
    server, and creates a new [Input_channel] and [Output_channel] to forward to the user
    provided tcp handler. [listen] will shutdown the server socket either if the handler
    raises an exception, or the Output_channel can no longer write any more bytes (it
    encountered an EPIPE, ECONNRESET). If the server loop is stopped because of a user
    exception, the exception will be re-raised once the socket has been shutdown. *)
val listen
  :  ?max_connections:int
  -> ?max_accepts_per_batch:int
  -> ?backlog:int
  -> ?socket:([ `Unconnected ], ([< Socket.Address.t ] as 'address)) Socket.t
  -> ?buf_len:int
  -> ?write_timeout:Time_ns.Span.t
  -> ?time_source:[> read ] Time_source.T1.t
  -> on_handler_error:[ `Call of 'address -> exn -> unit | `Ignore | `Raise ]
  -> ('address, 'listening_on) Tcp.Where_to_listen.t
  -> ('address -> Input_channel.t -> Output_channel.t -> unit Deferred.t)
  -> ('address, 'listening_on) Tcp.Server.t Deferred.t

val listen_inet
  :  ?max_connections:int
  -> ?max_accepts_per_batch:int
  -> ?backlog:int
  -> ?socket:([ `Unconnected ], Socket.Address.Inet.t) Socket.t
  -> ?buf_len:int
  -> ?write_timeout:Time_ns.Span.t
  -> ?time_source:[> read ] Time_source.T1.t
  -> on_handler_error:[ `Call of Socket.Address.Inet.t -> exn -> unit | `Ignore | `Raise ]
  -> Tcp.Where_to_listen.inet
  -> (Socket.Address.Inet.t -> Input_channel.t -> Output_channel.t -> unit Deferred.t)
  -> (Socket.Address.Inet.t, int) Tcp.Server.t

(** [with_connection] is a wrapper for [Async.Tcp.connect_sock]. It uses async to setup a
    tcp client, and creates a new [Input_channel] and [Output_channel] to forward to the
    user provided handler. *)
val with_connection
  :  ?interrupt:unit Deferred.t
  -> ?connect_timeout:Time.Span.t
  -> ?buf_len:int
  -> ?write_timeout:Time_ns.Span.t
  -> ?time_source:[> read ] Time_source.T1.t
  -> [< Socket.Address.t ] Tcp.Where_to_connect.t
  -> (Input_channel.t -> Output_channel.t -> 'res Deferred.t)
  -> 'res Async_kernel__Types.Deferred.t

val connect
  :  ?interrupt:unit Deferred.t
  -> ?connect_timeout:Time.Span.t
  -> ?buf_len:int
  -> ?write_timeout:Time_ns.Span.t
  -> ?time_source:[> read ] Time_source.T1.t
  -> [< Socket.Address.t ] Tcp.Where_to_connect.t
  -> (Input_channel.t * Output_channel.t) Deferred.t
