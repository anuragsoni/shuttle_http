open Core
open Async
open Shuttle
open Io_util

module Address = struct
  type t =
    | Host_and_port of Host_and_port.t
    | Unix_domain of Filename.t
  [@@deriving sexp_of]

  let of_host_and_port host_and_port = Host_and_port host_and_port
  let of_unix_domain_socket file = Unix_domain file
end

let write_request writer request =
  Output_channel.write writer (Meth.to_string (Request.meth request));
  Output_channel.write_char writer ' ';
  Output_channel.write writer (Request.path request);
  Output_channel.write_char writer ' ';
  Output_channel.write writer (Version.to_string (Request.version request));
  Output_channel.write writer "\r\n";
  let headers = Request.headers request in
  let headers, is_chunked =
    match Request.body request with
    | Body.Empty ->
      Headers.add_unless_exists headers ~key:"Content-Length" ~data:"0", false
    | Body.Fixed x ->
      ( Headers.add_unless_exists
          headers
          ~key:"Content-Length"
          ~data:(Int.to_string (String.length x))
      , false )
    | Body.Stream stream ->
      (match Body.Stream.encoding stream with
       | `Chunked ->
         Headers.add_unless_exists headers ~key:"Transfer-Encoding" ~data:"chunked", true
       | `Fixed len ->
         ( Headers.add_unless_exists
             headers
             ~key:"Content-Length"
             ~data:(Int.to_string len)
         , false ))
  in
  Headers.iter
    ~f:(fun ~key ~data ->
      Output_channel.write writer key;
      Output_channel.write writer ": ";
      Output_channel.write writer data;
      Output_channel.write writer "\r\n")
    headers;
  Output_channel.write writer "\r\n";
  match Request.body request with
  | Body.Empty -> Output_channel.flush writer
  | Body.Fixed x ->
    Output_channel.write writer x;
    Output_channel.flush writer
  | Body.Stream stream ->
    let%bind () =
      Body.Stream.iter stream ~f:(fun v ->
        if String.is_empty v
        then Deferred.unit
        else if is_chunked
        then (
          Output_channel.writef writer "%x\r\n" (String.length v);
          Output_channel.write writer v;
          Output_channel.write writer "\r\n";
          Output_channel.flush writer)
        else (
          Output_channel.write writer v;
          Output_channel.flush writer))
    in
    if is_chunked
    then (
      Output_channel.write writer "0\r\n\r\n";
      Output_channel.flush writer)
    else Output_channel.flush writer
;;

module Ssl = struct
  type t =
    { version : Async_ssl.Version.t option
    ; options : Async_ssl.Opt.t list option
    ; name : string option
    ; hostname : string option
    ; allowed_ciphers : [ `Only of string list | `Openssl_default | `Secure ] option
    ; ca_file : string option
    ; ca_path : string option
    ; crt_file : string option
    ; key_file : string option
    ; verify_modes : Async_ssl.Verify_mode.t list option
    ; session : (Async_ssl.Ssl.Session.t[@sexp.opaque]) option
    ; verify_certificate : (Shuttle_ssl.ssl_connection -> unit Or_error.t) option
    }
  [@@deriving sexp_of]

  let create
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
    ?verify_certificate
    ()
    =
    { version
    ; options
    ; name
    ; allowed_ciphers
    ; ca_file
    ; ca_path
    ; hostname
    ; crt_file
    ; key_file
    ; verify_modes
    ; session
    ; verify_certificate
    }
  ;;
end

let host_matches ssl_hostname hostname =
  let ssl_hostname_parts = String.split ~on:'.' ssl_hostname in
  match ssl_hostname_parts with
  | [] -> false
  | x :: xs ->
    let wildcard_count = String.count x ~f:(fun ch -> Char.equal ch '*') in
    if wildcard_count > 1
    then
      raise_s
        [%message
          "More than one wildcard characters in hostname part" ~hostname:ssl_hostname]
    else if wildcard_count = 0
    then if String.Caseless.equal ssl_hostname hostname then true else false
    else (
      let regex_parts_head =
        if String.equal x "*"
        then "[^.]+"
        else Re2.replace_exn ~f:(fun _ -> "[^.]+") (Re2.create_exn (Re2.escape "*")) x
      in
      let regex_parts = "\\A" :: regex_parts_head :: List.map xs ~f:Re2.escape in
      let pattern =
        regex_parts
        |> String.concat ~sep:(Re2.escape ".")
        |> Re2.create_exn ~options:{ Re2.Options.default with case_sensitive = true }
      in
      Re2.matches pattern hostname)
;;

let default_ssl_verify_certificate ssl_conn hostname =
  match Shuttle_ssl.peer_certificate ssl_conn with
  | None -> Or_error.errorf "Missing ssl peer certificate"
  | Some (Error e) -> Error e
  | Some (Ok cert) ->
    (match Async_ssl.Ssl.Certificate.subject_alt_names cert with
     | [] ->
       let name =
         cert
         |> Async_ssl.Ssl.Certificate.subject
         |> List.find_map ~f:(function
              | "CN", name -> Some name
              | _ -> None)
       in
       (match name with
        | None -> Or_error.errorf "Could not find Common Name in ssl certificate"
        | Some name ->
          if host_matches name hostname
          then Ok ()
          else
            Or_error.error_s
              [%message "SSL Certificate validation failed." ~common_name:name ~hostname])
     | names ->
       if List.exists names ~f:(fun glob -> host_matches glob hostname)
       then Ok ()
       else
         Or_error.error_s
           [%message
             "SSL Certificate validation failed."
               ~hostname_requested:hostname
               ~certificate_hostnames:(names : string list)])
;;

module Connection = struct
  type t =
    { reader : Input_channel.t
    ; writer : Output_channel.t
    ; mutable is_closed : bool
    }
  [@@deriving sexp_of]

  let is_closed t = t.is_closed

  let close t =
    if not (is_closed t)
    then
      Output_channel.close t.writer
      >>> fun () -> Input_channel.close t.reader >>> fun () -> t.is_closed <- true
  ;;

  let closed t =
    Deferred.all_unit
      [ Input_channel.closed t.reader; Output_channel.close_finished t.writer ]
  ;;

  let create ?ssl ?connect_timeout ?interrupt where_to_connect =
    let%bind reader, writer =
      Tcp_channel.connect ?connect_timeout ?interrupt where_to_connect
    in
    match ssl with
    | None -> Deferred.Or_error.return { reader; writer; is_closed = false }
    | Some ssl ->
      Deferred.Or_error.try_with_join ~run:`Now (fun () ->
        let ivar = Ivar.create () in
        don't_wait_for
          (Shuttle_ssl.upgrade_client_connection
             ?version:ssl.Ssl.version
             ?options:ssl.options
             ?name:ssl.name
             ?hostname:ssl.hostname
             ?allowed_ciphers:ssl.allowed_ciphers
             ?ca_file:ssl.ca_file
             ?ca_path:ssl.ca_path
             ?crt_file:ssl.crt_file
             ?key_file:ssl.key_file
             ?verify_modes:ssl.verify_modes
             ?session:ssl.session
             reader
             writer
             ~f:(fun conn reader writer ->
             let verification_result =
               match ssl.verify_certificate with
               | None ->
                 (match ssl.hostname with
                  | None -> Ok ()
                  | Some hostname -> default_ssl_verify_certificate conn hostname)
               | Some v -> v conn
             in
             match verification_result with
             | Error err ->
               Ivar.fill ivar (Error err);
               Deferred.unit
             | Ok () ->
               let conn = { reader; writer; is_closed = false } in
               Ivar.fill ivar (Ok conn);
               closed conn));
        Ivar.read ivar)
  ;;

  let call t request =
    Deferred.Or_error.try_with_join ~run:`Now (fun () ->
      let%bind () = write_request t.writer request in
      Deferred.repeat_until_finished () (fun () ->
        let view = Input_channel.view t.reader in
        match Parser.parse_response view.buf ~pos:view.pos ~len:view.len with
        | Error Partial ->
          (match%map Input_channel.refill t.reader with
           | `Eof -> `Finished (Or_error.errorf "Unexpected EOF while reading")
           | `Ok -> `Repeat ())
        | Error (Fail error) -> return (`Finished (Error error))
        | Ok (response, consumed) ->
          Input_channel.consume t.reader consumed;
          (match parse_body t.reader (Response.headers response) with
           | Error _ as error -> return (`Finished error)
           | Ok body ->
             let response = Response.with_body response body in
             return (`Finished (Ok response)))))
  ;;
end

module Oneshot = struct
  let call ?interrupt ?connect_timeout ?ssl address request =
    let request, ssl =
      match address with
      | Address.Host_and_port host_and_port ->
        let request =
          request
          |> Request.headers
          |> Headers.add_unless_exists
               ~key:"Host"
               ~data:(Host_and_port.host host_and_port)
          |> Headers.add_unless_exists ~key:"Connection" ~data:"close"
          |> Request.with_headers request
        in
        let ssl =
          Option.map ssl ~f:(fun ssl ->
            match ssl.Ssl.hostname with
            | None -> { ssl with hostname = Some (Host_and_port.host host_and_port) }
            | Some _ -> ssl)
        in
        request, ssl
      | Unix_domain _ -> request, ssl
    in
    let%bind.Deferred.Or_error conn =
      match address with
      | Address.Host_and_port host_and_port ->
        Connection.create
          ?connect_timeout
          ?ssl
          ?interrupt
          (Tcp.Where_to_connect.of_host_and_port host_and_port)
      | Unix_domain file ->
        Connection.create
          ?connect_timeout
          ?ssl
          ?interrupt
          (Tcp.Where_to_connect.of_file file)
    in
    match%bind Connection.call conn request with
    | Error error ->
      Connection.close conn;
      Deferred.Or_error.fail error
    | Ok response ->
      (match Response.body response with
       | Body.Fixed _ | Body.Empty ->
         Connection.close conn;
         Deferred.Or_error.return response
       | Body.Stream stream ->
         upon (Body.Stream.closed stream) (fun () -> Connection.close conn);
         Deferred.Or_error.return response)
  ;;
end
