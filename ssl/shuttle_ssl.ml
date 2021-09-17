open! Core
open! Async
open! Async_ssl
open! Shuttle

type ssl_handle =
  { input_channel : Input_channel.t
  ; output_channel : Output_channel.t
  ; shutdown : unit -> unit Deferred.t
  }

let server
    ?version
    ?options
    ?name
    ?allowed_ciphers
    ?ca_file
    ?ca_path
    ?verify_modes
    ~crt_file
    ~key_file
    input_channel
    output_channel
  =
  let net_to_ssl = Input_channel.pipe input_channel in
  let ssl_to_net = Output_channel.pipe output_channel in
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
  | Error _ as e -> return e
  | Ok conn ->
    let%bind input_channel =
      Input_channel.of_pipe (Info.of_string "shuttle_ssl.ssl_reader") app_reader
    in
    let%bind output_channel, flushed =
      Output_channel.of_pipe (Info.of_string "shuttle_ssl.ssl_writer") app_writer
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
      Input_channel.close input_channel
    in
    return (Ok { input_channel; output_channel; shutdown })
;;
