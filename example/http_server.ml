open! Core
open! Async
open Shuttle_http

let websocket_handler request =
  Log.Global.info !"Request: %{sexp: Request.t}" request;
  match Request.header request "Sec-WebSocket-Key" with
  | None -> return (Response.create `Bad_request)
  | Some v ->
    let accept_key = Websocket.sec_websocket_accept_header_value ~sec_websocket_key:v in
    let handler ?unconsumed_data fd =
      Log.Global.info !"Unconsumed data: %{sexp: string option}" unconsumed_data;
      let reader = Reader.create fd in
      let writer = Writer.create fd in
      let ws = Websocket.create ~role:Websocket.Websocket_role.Server reader writer in
      let rd, wr = Websocket.pipes ws in
      Pipe.transfer rd wr ~f:(fun x ->
        Log.Global.info "received: %S" x;
        x)
    in
    return
      (Response.upgrade
         ~headers:
           [ "Upgrade", "websocket"
           ; "Connection", "Upgrade"
           ; "Sec-WebSocket-Accept", accept_key
           ]
         handler)
;;

let service context request =
  Log.Global.info "Peer address: %s" (Socket.Address.to_string (Server.peer_addr context));
  match Request.path request, Request.meth request with
  | "/echo", `GET -> websocket_handler request
  | "/", `GET -> return (Response.create ~body:(Body.string "Hello World") `Ok)
  | ("/echo" | "/"), _ -> return (Response.create `Method_not_allowed)
  | _ -> return (Response.create `Not_found)
;;

let run port =
  let server = Server.run_inet (Tcp.Where_to_listen.of_port port) service in
  Log.Global.info
    !"Server listening on: %s"
    (Socket.Address.to_string (Tcp.Server.listening_on_address server));
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
