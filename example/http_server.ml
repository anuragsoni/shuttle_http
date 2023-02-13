open! Core
open! Async
open Shuttle_http

let service request =
  match Request.path request, Request.meth request with
  | "/echo", `POST -> return (Response.create ~body:(Request.body request) `Ok)
  | "/", `GET -> return (Response.create ~body:(Body.string "Hello World") `Ok)
  | ("/echo" | "/"), _ -> return (Response.create `Method_not_allowed)
  | _ -> return (Response.create `Not_found)

let run port =
  let server =
    Server.run_inet (Tcp.Where_to_listen.of_port port) (fun _addr -> service)
  in
  Log.Global.info
    !"Server listening on: %s"
    (Socket.Address.to_string (Tcp.Server.listening_on_address server));
  Tcp.Server.close_finished_and_handlers_determined server

let () =
  Command.async
    ~summary:"Start an echo server"
    (Command.Param.return (fun () -> run 8080))
  |> Command_unix.run
