open Core
open Async

let run ~port =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (fun _addr r w ->
        let lines = Reader.lines r in
        Pipe.iter_without_pushback lines ~f:(fun _ -> Writer.write w "+PONG\r\n"))
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
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
