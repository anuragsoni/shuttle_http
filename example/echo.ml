open! Core
open! Async
open Shuttle

let run sock =
  let server =
    Tcp_channel.listen_inet
      ~buf_len:0x1000
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port sock)
      (fun _addr reader writer ->
      Deferred.create (fun ivar ->
        let rec loop () =
          Input_channel.refill reader
          >>> function
          | `Eof -> Ivar.fill ivar ()
          | `Ok ->
            let view = Input_channel.view reader in
            Output_channel.write_bigstring writer view.buf ~pos:view.pos ~len:view.len;
            Input_channel.consume reader view.len;
            Output_channel.flush writer >>> fun () -> loop ()
        in
        loop ()))
  in
  Log.Global.info
    !"Server listening on: %s"
    (Socket.Address.to_string (Tcp.Server.listening_on_address server));
  Deferred.forever () (fun () ->
    let%map.Deferred () = after Time.Span.(of_sec 0.5) in
    Log.Global.printf "Active connections: %d" (Tcp.Server.num_connections server));
  Tcp.Server.close_finished_and_handlers_determined server

let () =
  Command.async
    ~summary:"Start an echo server"
    Command.Let_syntax.(
      let%map_open port =
        flag "-p" ~doc:"int Port number to listen on" (optional_with_default 8080 int)
      in
      fun () -> run port)
  |> Command_unix.run
