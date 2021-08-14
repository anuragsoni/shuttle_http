open! Core
open! Async

module Codec = struct
  open Shuttle

  type t =
    { writer : Writer.t
    ; reader : Reader.t
    }

  let create fd =
    let reader = Reader.create fd in
    let writer = Writer.create fd in
    { writer; reader }
  ;;

  let buf' = Bigstring.of_string "+PONG\r\n"

  let run_loop t =
    Reader.read_one_chunk_at_a_time t.reader ~on_chunk:(fun buf ~pos ~len ->
        for i = pos to len - 1 do
          if Char.(Bigstring.get buf i = '\n')
          then Shuttle.Writer.schedule_bigstring t.writer buf'
        done;
        Writer.flush t.writer;
        `Continue)
    >>| function
    | Error `Eof -> ()
    | Error `Closed -> raise_s [%message "Attempting to read from a closed fd"]
    | Ok _ -> ()
  ;;
end

let run ~port =
  let host_and_port =
    Tcp.Server.create_sock_inet
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (fun _addr sock ->
        let conn = Codec.create (Socket.fd sock) in
        Codec.run_loop conn)
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
