open! Core
open! Async
open Shuttle_http

let unlink f = Deferred.ignore_m (Monitor.try_with (fun () -> Unix.unlink f))

let handler ctx request =
  match Request.path request with
  | "/error" -> failwith "ERROR"
  | "/echo" -> return (Server.respond_stream ctx (Body.to_stream (Request.body request)))
  | _ -> return (Server.respond_string ctx "Hello World")
;;

let run path =
  let%bind () = unlink path in
  let%bind server =
    Shuttle.Connection.listen
      ~input_buffer_size:0x4000
      ~output_buffer_size:0x4000
      ~max_accepts_per_batch:64
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_file path)
      ~f:(fun _addr reader writer ->
      let server = Shuttle_http.Server.create reader writer in
      Server.run server (handler server))
  in
  Tcp.Server.close_finished_and_handlers_determined server
;;

let () =
  Command.async
    ~summary:"Start an http server for use within integration tests"
    Command.Let_syntax.(
      let%map_open sock = anon ("socket" %: string) in
      fun () -> run sock)
  |> Command_unix.run
;;
