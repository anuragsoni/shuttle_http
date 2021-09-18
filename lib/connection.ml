open! Core
open! Async

let listen
    ?max_connections
    ?max_accepts_per_batch
    ?backlog
    ?socket
    ~on_handler_error
    where_to_listen
    handler
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
      let input_channel = Input_channel.create fd in
      let output_channel = Output_channel.create fd in
      Monitor.protect
        ~run:`Now
        ~finally:(fun () ->
          let%bind () = Output_channel.close output_channel in
          Input_channel.close input_channel)
        (fun () -> handler addr input_channel output_channel))
;;
