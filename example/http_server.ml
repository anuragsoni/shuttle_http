open! Core
open! Async
open Shuttle_http

let text = String.init 2053 ~f:(fun _ -> 'a')
let handler _address _request = return (Response.create ~body:(Body.string text) `Ok)

let run port =
  let server = Server.run_inet (Tcp.Where_to_listen.of_port port) handler in
  Log.Global.info
    !"Server listening on: %s"
    (Socket.Address.to_string (Tcp.Server.listening_on_address server));
  Deferred.forever () (fun () ->
    let%map.Deferred () = after Time.Span.(of_sec 0.5) in
    Log.Global.printf "Active connections: %d" (Tcp.Server.num_connections server));
  Tcp.Server.close_finished_and_handlers_determined server
;;

let () =
  Command.async
    ~summary:"Start an echo server"
    Command.Let_syntax.(
      let%map_open port =
        flag "-p" ~doc:"int Port number to listen on" (optional_with_default 8080 int)
      in
      fun () -> run port)
  |> Command_unix.run
;;
