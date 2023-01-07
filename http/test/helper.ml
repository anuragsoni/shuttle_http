open! Core
open! Async

let rec connect port =
  match%bind
    Monitor.try_with (fun () ->
      Tcp.connect
        (Tcp.Where_to_connect.of_host_and_port
           (Host_and_port.create ~host:"localhost" ~port)))
  with
  | Ok (_, r, w) -> return (r, w)
  | Error _ ->
    let%bind () = Clock_ns.after (Time_ns.Span.of_sec 0.01) in
    connect port
;;

let with_client port ~f =
  let%bind r, w = connect port in
  Monitor.protect
    (fun () ->
      Writer.set_raise_when_consumer_leaves w false;
      f r w)
    ~finally:(fun () -> Writer.close w >>= fun () -> Reader.close r)
;;

let with_server ?error_handler ?read_header_timeout handler ~f =
  let open Shuttle_http in
  let%bind server =
    Shuttle.Connection.listen
      ~input_buffer_size:0x4000
      ~output_buffer_size:0x4000
      ~max_accepts_per_batch:64
      ~on_handler_error:`Raise
      Tcp.Where_to_listen.of_port_chosen_by_os
      (fun _addr reader writer ->
      let server =
        Shuttle_http.Server.create ?read_header_timeout ?error_handler reader writer
      in
      Server.run server (handler server))
  in
  Monitor.protect
    ~finally:(fun () -> Tcp.Server.close server)
    (fun () -> f (Tcp.Server.listening_on server))
;;

let send_request_and_log_response r w req =
  Writer.write w req;
  let reader = Reader.pipe r in
  let buf = Buffer.create 128 in
  let%map () = Pipe.iter_without_pushback reader ~f:(Buffer.add_string buf) in
  printf "%S" (Buffer.contents buf)
;;
