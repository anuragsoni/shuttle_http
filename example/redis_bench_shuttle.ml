open! Core
open! Async
open Shuttle

let unlink f = Deferred.ignore_m (Monitor.try_with (fun () -> Unix.unlink f))

let run sock =
  let%bind () = unlink sock in
  let%bind host_and_port =
    Connection.listen
      ~input_buffer_size:0x1000
      ~output_buffer_size:0x1000
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_file sock)
      ~f:(fun _addr reader writer ->
        let rec loop reader writer =
          Input_channel.refill reader
          >>= function
          | `Eof -> Deferred.unit
          | `Ok ->
            let view = Input_channel.view reader in
            let buf = Input_channel.View.buf view in
            let pos = Input_channel.View.pos view in
            let len = Input_channel.View.length view in
            for i = pos to len - 1 do
              if Char.equal (String.unsafe_get buf i) '\n'
              then Output_channel.write writer "+PONG\r\n"
            done;
            Input_channel.View.consume view len;
            Output_channel.flush writer;
            loop reader writer
        in
        loop reader writer)
  in
  ignore (host_and_port : (Socket.Address.Unix.t, string) Tcp.Server.t);
  Deferred.never ()
;;

let () =
  Command.async
    ~summary:"Start an echo server"
    Command.Let_syntax.(
      let%map_open sock = anon ("socket" %: string) in
      fun () -> run sock)
  |> Command.run
;;
