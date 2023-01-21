open! Core
open! Async
open Shuttle_http

let run () =
  let stdout = Lazy.force Writer.stdout in
  let%bind httpbin =
    Deferred.Or_error.ok_exn
      (Client.create
         ~ssl:(Client.Ssl.create ())
         (Client.Address.of_host_and_port
            (Host_and_port.create ~host:"httpbin.org" ~port:443)))
  in
  Monitor.protect
    ~finally:(fun () -> Client.close httpbin)
    (fun () ->
      let%bind response =
        let%map res, duration =
          Clock_ns.duration_of (fun () ->
            Client.call httpbin (Request.create `GET "/stream/20"))
        in
        Log.Global.info !"Duration of request A: %{sexp: Time_ns.Span.t}" duration;
        res
      in
      Log.Global.info !"Headers: %{sexp: Headers.t}" (Response.headers response);
      let%bind () =
        Body.Stream.iter
          (Body.to_stream (Response.body response))
          ~f:(fun chunk ->
            Writer.write stdout chunk;
            Writer.flushed stdout)
      in
      let%bind response =
        let%map res, duration =
          Clock_ns.duration_of (fun () ->
            Client.call httpbin (Request.create `GET "/get"))
        in
        Log.Global.info !"Duration of request B: %{sexp: Time_ns.Span.t}" duration;
        res
      in
      Log.Global.info !"Headers: %{sexp: Headers.t}" (Response.headers response);
      Body.Stream.iter
        (Body.to_stream (Response.body response))
        ~f:(fun chunk ->
          Writer.write stdout chunk;
          Writer.flushed stdout))
;;

let () =
  Command.async ~summary:"Http client example" (Command.Param.return (fun () -> run ()))
  |> Command_unix.run
;;
