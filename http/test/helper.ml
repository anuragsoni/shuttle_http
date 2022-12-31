open! Core
open! Async

let rec connect path =
  match%bind
    Monitor.try_with (fun () -> Tcp.connect (Tcp.Where_to_connect.of_file path))
  with
  | Ok (_, r, w) -> return (r, w)
  | Error _ ->
    let%bind () = Clock_ns.after (Time_ns.Span.of_sec 0.01) in
    connect path
;;

let cleanup process =
  Process.send_signal process Signal.kill;
  let%bind (_ : Unix.Exit_or_signal.t) = Process.wait process in
  return ()
;;

let with_client path ~f =
  let%bind r, w = connect path in
  Monitor.protect
    (fun () ->
      Writer.set_raise_when_consumer_leaves w false;
      f r w)
    ~finally:(fun () -> Writer.close w >>= fun () -> Reader.close r)
;;

let with_server path ~f =
  let%bind process = Process.create_exn ~prog:"./bin/http_server.exe" ~args:[ path ] () in
  Monitor.protect ~finally:(fun () -> cleanup process) (fun () -> f ())
;;

let with_server_custom_error_handler path ~f =
  let%bind process =
    Process.create_exn
      ~prog:"./bin/http_server_custom_error_handler.exe"
      ~args:[ path ]
      ()
  in
  Monitor.protect ~finally:(fun () -> cleanup process) (fun () -> f ())
;;

let with_server_timeout path ~f =
  let%bind process =
    Process.create_exn ~prog:"./bin/http_server_timeout.exe" ~args:[ path ] ()
  in
  Monitor.protect ~finally:(fun () -> cleanup process) (fun () -> f ())
;;

let send_request_and_log_response r w req =
  Writer.write w req;
  let reader = Reader.pipe r in
  let buf = Buffer.create 128 in
  let%map () = Pipe.iter_without_pushback reader ~f:(Buffer.add_string buf) in
  printf "%S" (Buffer.contents buf)
;;
