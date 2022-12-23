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
  -> ?input_buffer_size:int
  -> ?max_input_buffer_size:int
  -> ?output_buffer_size:int
  -> ?max_output_buffer_size:int
  -> ?write_timeout:Time_ns.Span.t
  -> on_handler_error:[ `Call of 'address -> exn -> unit | `Ignore | `Raise ]
  -> f:('address -> Input_channel.t -> Output_channel.t -> unit Deferred.t)
  -> ('address, 'listening_on) Tcp.Where_to_listen.t
  -> ('address, 'listening_on) Tcp.Server.t Deferred.t

(** [with_connection] is a wrapper for [Async.Tcp.connect_sock]. It uses async to setup a
    tcp client, and creates a new [Input_channel] and [Output_channel] to forward to the
    user provided handler. *)
val with_connection
  :  ?interrupt:unit Deferred.t
  -> ?timeout:Time.Span.t
  -> ?input_buffer_size:int
  -> ?max_input_buffer_size:int
  -> ?output_buffer_size:int
  -> ?max_output_buffer_size:int
  -> f:(Input_channel.t -> Output_channel.t -> 'res Deferred.t)
  -> [< Socket.Address.t ] Tcp.Where_to_connect.t
  -> 'res Async_kernel__Types.Deferred.t
