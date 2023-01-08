open! Core
open! Async
open! Shuttle

let rec connect port =
  match%bind
    Monitor.try_with (fun () ->
      Tcp_channel.connect
        (Tcp.Where_to_connect.of_host_and_port
           (Host_and_port.create ~host:"localhost" ~port)))
  with
  | Ok (r, w) -> return (r, w)
  | Error _ ->
    let%bind () = Clock_ns.after (Time_ns.Span.of_sec 0.01) in
    connect port
;;

let with_client port ~f =
  let%bind r, w = connect port in
  Monitor.protect
    (fun () -> f r w)
    ~finally:(fun () -> Output_channel.close w >>= fun () -> Input_channel.close r)
;;

let with_server ?error_handler ?read_header_timeout handler ~f =
  let open Shuttle_http in
  let%bind server =
    Tcp_channel.listen
      ~input_buffer_size:0x4000
      ~output_buffer_size:0x4000
      ~max_accepts_per_batch:64
      ~on_handler_error:`Raise
      Tcp.Where_to_listen.of_port_chosen_by_os
      (fun _addr reader writer ->
      let server =
        Shuttle_http.Server.create ?read_header_timeout ?error_handler reader writer
      in
      Server.run server handler)
  in
  Monitor.protect
    ~finally:(fun () -> Tcp.Server.close server)
    (fun () -> f (Tcp.Server.listening_on server))
;;

let send_request_and_log_response r w req =
  Output_channel.write w req;
  let%bind () = Output_channel.flush w in
  let reader = Input_channel.pipe r in
  let buf = Buffer.create 128 in
  let%map () = Pipe.iter_without_pushback reader ~f:(Buffer.add_string buf) in
  printf "%S" (Buffer.contents buf)
;;
