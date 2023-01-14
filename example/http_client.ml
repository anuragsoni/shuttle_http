open! Core
open! Async
open Shuttle_http

let run () =
  let stdout = Lazy.force Writer.stdout in
  let%bind response =
    Deferred.Or_error.ok_exn
      (Client.call
         ~ssl:(Client.Ssl.create ())
         (Tcp.Where_to_connect.of_host_and_port
            (Host_and_port.create ~host:"httpbin.org" ~port:443))
         (Request.create
            ~headers:(Headers.of_rev_list [ "Host", "httpbin.org" ])
            `GET
            "/stream/20"))
  in
  Log.Global.info !"Headers: %{sexp: Headers.t}" (Response.headers response);
  Body.Stream.iter
    (Body.to_stream (Response.body response))
    ~f:(fun chunk ->
      Writer.write stdout chunk;
      Writer.flushed stdout)
;;

let () =
  Command.async ~summary:"Http client example" (Command.Param.return (fun () -> run ()))
  |> Command_unix.run
;;
