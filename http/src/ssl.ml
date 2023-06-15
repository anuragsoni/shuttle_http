open! Core
open! Async
open! Async_ssl

let close_channels input_channel output_channel =
  let%bind () = Output_channel.close output_channel in
  Input_channel.close input_channel
;;

let pipe_of_channels input_channel output_channel =
  let net_to_ssl, net_to_ssl_writer = Pipe.create () in
  upon (Input_channel.transfer input_channel net_to_ssl_writer) (fun () ->
    close_channels input_channel output_channel >>> fun () -> Pipe.close net_to_ssl_writer);
  let ssl_to_net = Output_channel.pipe output_channel in
  upon (Pipe.closed ssl_to_net) (fun () ->
    choose
      [ choice (after (Time_float.Span.of_sec 30.)) (fun () -> ())
      ; choice (Pipe.downstream_flushed ssl_to_net) (fun (_ : Pipe.Flushed_result.t) ->
          ())
      ]
    >>> fun () -> don't_wait_for (close_channels input_channel output_channel));
  net_to_ssl, ssl_to_net
;;

let upgrade_client_connection
  ?version
  ?options
  ?name
  ?hostname
  ?allowed_ciphers
  ?ca_file
  ?ca_path
  ?crt_file
  ?key_file
  ?verify_modes
  ?session
  ~f
  input_channel
  output_channel'
  =
  let net_to_ssl, ssl_to_net = pipe_of_channels input_channel output_channel' in
  let app_to_ssl, app_writer = Pipe.create () in
  let app_reader, ssl_to_app = Pipe.create () in
  match%bind
    Ssl.client
      ?version
      ?options
      ?name
      ?hostname
      ?allowed_ciphers
      ?ca_file
      ?ca_path
      ?crt_file
      ?key_file
      ?verify_modes
      ?session
      ~app_to_ssl
      ~ssl_to_app
      ~net_to_ssl
      ~ssl_to_net
      ()
  with
  | Error e ->
    let%map () = close_channels input_channel output_channel' in
    Error.raise e
  | Ok conn ->
    let%bind input_channel =
      Input_channel.of_pipe
        ~max_buffer_size:(Input_channel.max_buffer_size input_channel)
        ~buf_len:(Input_channel.buffer_size input_channel)
        (Info.of_string "shuttle_ssl.ssl_reader")
        app_reader
    in
    upon (Input_channel.closed input_channel) (fun () -> Pipe.close_read app_reader);
    let%bind output_channel, flushed =
      Output_channel.of_pipe
        ~max_buffer_size:(Input_channel.max_buffer_size input_channel)
        ~buf_len:(Input_channel.buffer_size input_channel)
        (Info.of_string "shuttle_ssl.ssl_writer")
        app_writer
    in
    let shutdown () =
      let%bind () = Output_channel.close output_channel in
      let%bind () = flushed in
      Ssl.Connection.close conn;
      let%bind () =
        match%map Ssl.Connection.closed conn with
        | Ok _ -> ()
        | Error e ->
          Log.Global.error !"Error while shutting down ssl connection %{sexp: Error.t}" e
      in
      Deferred.all_unit
        [ Input_channel.close input_channel
        ; Output_channel.close_finished output_channel'
        ]
    in
    Monitor.protect ~run:`Now ~finally:shutdown (fun () ->
      f conn input_channel output_channel)
;;

let upgrade_server_connection
  ?version
  ?options
  ?name
  ?allowed_ciphers
  ?ca_file
  ?ca_path
  ?verify_modes
  ~crt_file
  ~key_file
  ~f
  input_channel
  output_channel'
  =
  let net_to_ssl, ssl_to_net = pipe_of_channels input_channel output_channel' in
  let app_to_ssl, app_writer = Pipe.create () in
  let app_reader, ssl_to_app = Pipe.create () in
  match%bind
    Ssl.server
      ?version
      ?options
      ?name
      ?allowed_ciphers
      ?ca_file
      ?ca_path
      ?verify_modes
      ~crt_file
      ~key_file
      ~net_to_ssl
      ~ssl_to_net
      ~ssl_to_app
      ~app_to_ssl
      ()
  with
  | Error e ->
    let%map () = close_channels input_channel output_channel' in
    Error.raise e
  | Ok conn ->
    let%bind input_channel =
      Input_channel.of_pipe
        ~max_buffer_size:(Input_channel.max_buffer_size input_channel)
        ~buf_len:(Input_channel.buffer_size input_channel)
        (Info.of_string "shuttle_ssl.ssl_reader")
        app_reader
    in
    upon (Input_channel.closed input_channel) (fun () -> Pipe.close_read app_reader);
    let%bind output_channel, flushed =
      Output_channel.of_pipe
        ~max_buffer_size:(Input_channel.max_buffer_size input_channel)
        ~buf_len:(Input_channel.buffer_size input_channel)
        (Info.of_string "shuttle_ssl.ssl_writer")
        app_writer
    in
    let shutdown () =
      let%bind () = Output_channel.close output_channel in
      let%bind () = flushed in
      Ssl.Connection.close conn;
      let%bind () =
        match%map Ssl.Connection.closed conn with
        | Ok _ -> ()
        | Error e ->
          Log.Global.error !"Error while shutting down ssl connection %{sexp: Error.t}" e
      in
      Deferred.all_unit
        [ Input_channel.close input_channel
        ; Output_channel.close_finished output_channel'
        ]
    in
    Monitor.protect ~run:`Now ~finally:shutdown (fun () ->
      f conn input_channel output_channel)
;;
