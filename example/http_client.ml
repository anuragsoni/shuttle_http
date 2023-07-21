open! Core
open! Async
open Shuttle_http

let httpbin_address =
  Client.Address.of_host_and_port (Host_and_port.create ~host:"httpbin.org" ~port:443)
;;

let one_shot_client () =
  let%bind response =
    Client.Oneshot.call
      ~ssl:(Client.Ssl.create ())
      httpbin_address
      (Request.create `GET "/get")
  in
  printf "Response status: %d\n" (Response.status response |> Status.to_int);
  let%map body = Body.to_string (Response.body response) in
  print_endline body
;;

let persistent_client () =
  let%bind httpbin =
    Deferred.Or_error.ok_exn (Client.create ~ssl:(Client.Ssl.create ()) httpbin_address)
  in
  Monitor.protect
    ~finally:(fun () -> Client.close httpbin)
    (fun () ->
      let%bind response = Client.call httpbin (Request.create `GET "/stream/20") in
      printf !"Headers: %{sexp: (string * string) list}" (Response.headers response);
      let%bind () =
        Body.Stream.iter
          (Body.to_stream (Response.body response))
          ~f:(fun chunk ->
            printf "%s" chunk;
            Deferred.unit)
      in
      let%bind response = Client.call httpbin (Request.create `GET "/get") in
      printf !"Headers: %{sexp: (string * string) list}" (Response.headers response);
      Body.Stream.iter
        (Body.to_stream (Response.body response))
        ~f:(fun chunk ->
          printf "%s" chunk;
          Deferred.unit))
;;

let run () =
  Deferred.List.iter ~how:`Sequential [ one_shot_client; persistent_client ] ~f:(fun f ->
    f ())
;;

let () =
  Command.async ~summary:"Http client example" (Command.Param.return (fun () -> run ()))
  |> Command_unix.run
;;
