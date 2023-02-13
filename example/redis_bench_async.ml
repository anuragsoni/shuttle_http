open Core
open Async

let unlink f = Deferred.ignore_m (Monitor.try_with (fun () -> Unix.unlink f))

let run sock =
  let%bind () = unlink sock in
  let%bind server =
    Tcp.Server.create_sock
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_file sock)
      (fun _addr sock ->
      let fd = Socket.fd sock in
      let r = Reader.create ~buf_len:0x1000 fd in
      let w = Writer.create ~buf_len:0x1000 fd in
      let lines = Reader.lines r in
      Pipe.iter_without_pushback lines ~f:(fun _ -> Writer.write w "+PONG\r\n"))
  in
  Deferred.forever () (fun () ->
    let%map.Deferred () = after Time.Span.(of_sec 0.5) in
    Log.Global.printf "Active connections: %d" (Tcp.Server.num_connections server));
  Tcp.Server.close_finished_and_handlers_determined server

let () =
  Command.async
    ~summary:"Start an echo server"
    Command.Let_syntax.(
      let%map_open sock = anon ("socket" %: string) in
      fun () -> run sock)
  |> Command_unix.run
