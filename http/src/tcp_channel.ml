open! Core
open! Async

let close_channels reader writer =
  let%bind () = Output_channel.close writer in
  Input_channel.close reader
;;

let collect_errors writer fn =
  let monitor = Output_channel.monitor writer in
  Monitor.detach monitor;
  choose
    [ choice (Monitor.get_next_error monitor) (fun e -> Error e)
    ; choice (Monitor.try_with ~run:`Now ~rest:`Log fn) Fn.id
    ]
;;

let listen
  ?max_connections
  ?max_accepts_per_batch
  ?backlog
  ?socket
  ?max_buffer_size
  ?buf_len
  ?write_timeout
  ?time_source
  ~on_handler_error
  where_to_listen
  handler
  =
  Tcp.Server.create_sock
    ?max_connections
    ?max_accepts_per_batch
    ?backlog
    ?socket
    ?time_source
    ~on_handler_error
    where_to_listen
    (fun addr socket ->
       let fd = Socket.fd socket in
       let input_channel =
         Input_channel.create ?max_buffer_size ?buf_len ?time_source fd
       in
       let output_channel =
         Output_channel.create ?max_buffer_size ?buf_len ?write_timeout ?time_source fd
       in
       let%bind res =
         Deferred.any
           [ collect_errors output_channel (fun () ->
               handler addr input_channel output_channel)
           ; Output_channel.remote_closed output_channel |> Deferred.ok
           ]
       in
       let%bind () = close_channels input_channel output_channel in
       match res with
       | Ok () -> Deferred.unit
       | Error exn -> raise exn)
;;

let listen_inet
  ?max_connections
  ?max_accepts_per_batch
  ?backlog
  ?socket
  ?max_buffer_size
  ?buf_len
  ?write_timeout
  ?time_source
  ~on_handler_error
  where_to_listen
  handler
  =
  Tcp.Server.create_sock_inet
    ?max_connections
    ?max_accepts_per_batch
    ?backlog
    ?socket
    ?time_source
    ~on_handler_error
    where_to_listen
    (fun addr socket ->
       let fd = Socket.fd socket in
       let input_channel =
         Input_channel.create ?max_buffer_size ?buf_len ?time_source fd
       in
       let output_channel =
         Output_channel.create ?max_buffer_size ?buf_len ?write_timeout ?time_source fd
       in
       let%bind res =
         Deferred.any
           [ collect_errors output_channel (fun () ->
               handler addr input_channel output_channel)
           ; Output_channel.remote_closed output_channel |> Deferred.ok
           ]
       in
       let%bind () = close_channels input_channel output_channel in
       match res with
       | Ok () -> Deferred.unit
       | Error exn -> raise exn)
;;

let with_connection
  ?interrupt
  ?connect_timeout
  ?max_buffer_size
  ?buf_len
  ?write_timeout
  ?time_source
  where_to_connect
  f
  =
  let%bind socket =
    Tcp.connect_sock ?interrupt ?timeout:connect_timeout ?time_source where_to_connect
  in
  let fd = Socket.fd socket in
  let input_channel = Input_channel.create ?max_buffer_size ?buf_len ?time_source fd in
  let output_channel =
    Output_channel.create ?max_buffer_size ?buf_len ?time_source ?write_timeout fd
  in
  let res = collect_errors output_channel (fun () -> f input_channel output_channel) in
  let%bind () =
    Deferred.any_unit
      [ (res >>| fun _ -> ())
      ; Output_channel.close_finished output_channel
      ; Input_channel.closed input_channel
      ]
  in
  let%bind () = close_channels input_channel output_channel in
  match%map res with
  | Ok v -> v
  | Error exn ->
    Exn.reraise exn "Shuttle.Connection: Unhandled exception in TCP client connection"
;;

let connect
  ?interrupt
  ?connect_timeout
  ?max_buffer_size
  ?buf_len
  ?write_timeout
  ?time_source
  where_to_connect
  =
  let%map socket =
    Tcp.connect_sock ?interrupt ?timeout:connect_timeout ?time_source where_to_connect
  in
  let fd = Socket.fd socket in
  let reader = Input_channel.create ?max_buffer_size ?buf_len ?time_source fd in
  let writer =
    Output_channel.create ?max_buffer_size ?buf_len ?time_source ?write_timeout fd
  in
  reader, writer
;;
