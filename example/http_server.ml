open! Core
open! Async
open Shuttle_http

let websocket_handler =
  Shuttle_websocket.create (fun ws ->
    let rd, wr = Websocket.pipes ws in
    Pipe.transfer_id rd wr)
;;

let service context request =
  Log.Global.info "Peer address: %s" (Socket.Address.to_string (Server.peer_addr context));
  match Request.path request, Request.meth request with
  | "/echo", `POST -> return (Response.create ~body:(Request.body request) `Ok)
  | "/websocket", `GET -> websocket_handler request
  | "/", `GET -> return (Response.create ~body:(Body.string "Hello World") `Ok)
  | ("/echo" | "/"), _ -> return (Response.create `Method_not_allowed)
  | _ -> return (Response.create `Not_found)
;;

let run port =
  let server = Server.run_inet (Tcp.Where_to_listen.of_port port) service in
  Log.Global.info
    !"Server listening on: %s"
    (Socket.Address.to_string (Tcp.Server.listening_on_address server));
  Deferred.forever () (fun () ->
    let%map.Deferred () = after Time_float.Span.(of_sec 0.5) in
    Log.Global.printf "Active connections: %d" (Tcp.Server.num_connections server));
  Tcp.Server.close_finished_and_handlers_determined server
;;

let command =
  Command.async
    ~summary:"Http-server demo"
    (let%map_open.Command port =
       flag "-p" ~doc:"int Port number to listen on" (optional_with_default 8080 int)
     in
     fun () -> run port)
;;

let () = Command_unix.run command
