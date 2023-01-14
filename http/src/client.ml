open! Core
open! Async
open! Shuttle
open Io_util

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

type t =
  { host_and_port : Host_and_port.t
  ; ssl : Ssl.t option
  }
[@@deriving sexp_of]

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

let call ?ssl where_to_connect request =
  let ivar = Ivar.create () in
  let run reader writer =
    let%bind () = write_request writer request in
    let rec loop () =
      let view = Input_channel.view reader in
      match Parser.parse_response view.buf ~pos:view.pos ~len:view.len with
      | Ok (response, consumed) ->
        Input_channel.consume reader consumed;
        (match parse_body reader (Response.headers response) with
         | Error error ->
           Ivar.fill ivar (Error error);
           Deferred.unit
         | Ok body ->
           let response = Response.with_body response body in
           Ivar.fill ivar (Ok response);
           (match body with
            | Body.Empty | Body.Fixed _ -> Deferred.unit
            | Body.Stream stream -> Body.Stream.closed stream))
      | Error (Fail error) ->
        Ivar.fill ivar (Error error);
        Deferred.unit
      | Error Partial ->
        (match%bind Input_channel.refill reader with
         | `Ok -> loop ()
         | `Eof ->
           Ivar.fill ivar (Or_error.errorf "Unexpected EOF while reading response");
           Deferred.unit)
    in
    loop ()
  in
  let run () =
    Tcp_channel.with_connection where_to_connect (fun reader writer ->
      match ssl with
      | None -> run reader writer
      | Some ssl ->
        Shuttle_ssl.upgrade_client_connection
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
          ~f:(fun conn reader writer ->
            match
              match ssl.verify_certificate with
              | None ->
                (match ssl.hostname with
                 | None -> Ok ()
                 | Some hostname -> default_ssl_verify_certificate conn hostname)
              | Some v -> v conn
            with
            | Ok () -> run reader writer
            | Error error ->
              Ivar.fill ivar (Error error);
              Deferred.unit)
          reader
          writer)
  in
  (Monitor.try_with_or_error ~name:"Shuttle_http.Client.call" ~here:[%here] (fun () ->
     run ())
  >>> function
  | Ok () -> ()
  | Error error -> Ivar.fill_if_empty ivar (Error error));
  Ivar.read ivar
;;
