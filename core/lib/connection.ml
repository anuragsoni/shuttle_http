open! Core
open! Async

let listen
    ?max_connections
    ?max_accepts_per_batch
    ?backlog
    ?socket
    ?input_buffer_size
    ?output_buffer_size
    ~on_handler_error
    ~f:handler
    where_to_listen
  =
  Tcp.Server.create_sock
    ?max_connections
    ?max_accepts_per_batch
    ?backlog
    ?socket
    ~on_handler_error
    where_to_listen
    (fun addr socket ->
      let fd = Socket.fd socket in
      let input_channel = Input_channel.create ?buf_len:input_buffer_size fd in
      let output_channel = Output_channel.create ?buf_len:output_buffer_size fd in
      Monitor.protect
        ~run:`Now
        ~finally:(fun () ->
          let%bind () = Output_channel.close output_channel in
          Input_channel.close input_channel)
        (fun () -> handler addr input_channel output_channel))
;;

let with_connection
    ?interrupt
    ?timeout
    ?input_buffer_size
    ?output_buffer_size
    ~f
    where_to_connect
  =
  let%bind socket = Tcp.connect_sock ?interrupt ?timeout where_to_connect in
  let fd = Socket.fd socket in
  let input_channel = Input_channel.create ?buf_len:input_buffer_size fd in
  let output_channel = Output_channel.create ?buf_len:output_buffer_size fd in
  Monitor.protect
    ~run:`Now
    ~finally:(fun () ->
      let%bind () = Output_channel.close output_channel in
      Input_channel.close input_channel)
    (fun () -> f input_channel output_channel)
;;
