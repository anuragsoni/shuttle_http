open! Core
open! Async
open Shuttle

let buf' = Bigstring.of_string "+PONG\r\n"

let run ~port =
  let%bind host_and_port =
    Connection.listen
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      ~f:(fun _addr reader writer ->
        Input_channel.read_one_chunk_at_a_time reader ~on_chunk:(fun buf ->
            Bytebuffer.Consume.unsafe_bigstring buf ~f:(fun buf ~pos ~len ->
                for i = pos to len - 1 do
                  if Char.(Bigstring.get buf i = '\n')
                  then Output_channel.write_bigstring writer buf'
                done;
                len);
            Output_channel.flush writer;
            `Continue)
        >>| function
        | `Eof -> ()
        | `Eof_with_unconsumed _ -> assert false
        | `Stopped _ -> ())
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t);
  Deferred.never ()
;;

let () =
  Command.async
    ~summary:"Start an echo server"
    Command.Let_syntax.(
      let%map_open port =
        flag
          "-port"
          (optional_with_default 8888 int)
          ~doc:" Port to listen on (default 8888)"
      in
      fun () -> run ~port)
  |> Command.run
;;
