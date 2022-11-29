open! Core
open! Async
open Shuttle

let run sock =
  let%bind server =
    Connection.listen
      ~input_buffer_size:0x1000
      ~output_buffer_size:0x1000
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port sock)
      ~f:(fun _addr reader writer ->
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
  Tcp.Server.close_finished_and_handlers_determined server
;;

let () =
  Command.async
    ~summary:"Start an echo server"
    Command.Let_syntax.(
      let%map_open port = anon ("port" %: int) in
      fun () -> run port)
  |> Command_unix.run
;;
