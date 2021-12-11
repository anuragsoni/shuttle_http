open! Core
open! Async
open Shuttle

let buf' = Bigstring.of_string "+PONG\r\n"
let unlink f = Deferred.ignore_m (Monitor.try_with (fun () -> Unix.unlink f))

let run sock =
  let%bind () = unlink sock in
  let%bind host_and_port =
    Connection.listen
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_file sock)
      ~f:(fun _addr reader writer ->
        Input_channel.read_one_chunk_at_a_time reader ~on_chunk:(fun buf ~pos ~len ->
            for i = pos to len - 1 do
              if Char.(Bigstring.get buf i = '\n')
              then Output_channel.write_bigstring writer buf'
            done;
            Output_channel.flush writer;
            `Continue len)
        >>| function
        | `Eof -> ()
        | `Eof_with_unconsumed _ -> assert false
        | `Stopped _ -> ())
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
