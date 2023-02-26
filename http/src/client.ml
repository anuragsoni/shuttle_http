open Core
open Async
open Shuttle
open Io_util

module Address = struct
  module T = struct
    type t =
      | Host_and_port of Host_and_port.t
      | Unix_domain of Filename.t
    [@@deriving sexp, equal, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let of_host_and_port host_and_port = Host_and_port host_and_port
  let of_unix_domain_socket file = Unix_domain file

  let hostname = function
    | Host_and_port host_and_port -> Some (Host_and_port.host host_and_port)
    | _ -> None
  ;;
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

exception Remote_connection_closed
exception Request_aborted

module Connection = struct
  type conn =
    { reader : Input_channel.t
    ; writer : Output_channel.t
    ; address : Address.t
    }
  [@@deriving sexp_of]

  type t = conn Sequencer.t [@@deriving sexp_of]

  let close t = Throttle.kill t
  let is_closed t = Throttle.is_dead t
  let closed t = Throttle.cleaned t

  let create ?ssl ?connect_timeout ?interrupt address =
    let%bind reader, writer =
      match address with
      | Address.Host_and_port host_and_port ->
        Tcp_channel.connect
          ?connect_timeout
          ?interrupt
          (Tcp.Where_to_connect.of_host_and_port host_and_port)
      | Address.Unix_domain file ->
        Tcp_channel.connect
          ?connect_timeout
          ?interrupt
          (Tcp.Where_to_connect.of_file file)
    in
    match ssl with
    | None ->
      let conn = { reader; writer; address } in
      let t = Sequencer.create conn in
      Throttle.at_kill t (fun conn ->
        let%bind () = Output_channel.close conn.writer in
        Input_channel.close conn.reader);
      Deferred.Or_error.return t
    | Some ssl ->
      let ssl =
        match ssl.Ssl.hostname with
        | Some _ -> ssl
        | None -> { ssl with hostname = Address.hostname address }
      in
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
               let conn = { reader; writer; address } in
               let t = Sequencer.create conn in
               Throttle.at_kill t (fun conn ->
                 let%bind () = Output_channel.close conn.writer in
                 Input_channel.close conn.reader);
               Ivar.fill ivar (Ok t);
               closed t));
        Ivar.read ivar)
  ;;

  let call t request =
    let ivar = Ivar.create () in
    don't_wait_for
      (Throttle.enqueue' t (fun conn ->
         let request =
           match conn.address with
           | Address.Host_and_port host_and_port ->
             request
             |> Request.headers
             |> Headers.add_unless_exists
                  ~key:"Host"
                  ~data:(Host_and_port.host host_and_port)
             |> Request.with_headers request
           | Unix_domain _ -> request
         in
         let%bind () = write_request conn.writer request in
         Deferred.repeat_until_finished () (fun () ->
           let view = Input_channel.view conn.reader in
           match Parser.parse_response view.buf ~pos:view.pos ~len:view.len with
           | Error Partial ->
             (match%map Input_channel.refill conn.reader with
              | `Eof -> raise Remote_connection_closed
              | `Ok -> `Repeat ())
           | Error (Fail error) -> Error.raise error
           | Ok (response, consumed) ->
             Input_channel.consume conn.reader consumed;
             (match parse_body conn.reader (Response.headers response) with
              | Error error -> Error.raise error
              | Ok body ->
                let response = Response.with_body response body in
                if not
                     (keep_alive (Response.headers response)
                     && keep_alive (Request.headers request))
                then close t;
                Ivar.fill ivar response;
                (match Response.body response with
                 | Body.Fixed _ | Body.Empty -> return (`Finished ())
                 | Body.Stream stream ->
                   let%map () = Body.Stream.closed stream in
                   `Finished ()))))
      >>| function
      | `Ok () -> ()
      | `Raised exn ->
        Throttle.kill t;
        raise exn
      | `Aborted ->
        Throttle.kill t;
        raise Request_aborted);
    Ivar.read ivar
  ;;
end

module T = struct
  type t = Connection.t [@@deriving sexp_of]

  let create ?interrupt ?connect_timeout ?ssl address =
    Connection.create ?interrupt ?connect_timeout ?ssl address
  ;;

  let close t =
    Connection.close t;
    Connection.closed t
  ;;

  let closed t = Connection.closed t
  let close_finished t = closed t
  let is_closed t = Connection.is_closed t
end

include T

let call t request = Connection.call t request

module Persistent_connection = Persistent_connection_kernel.Make (T)

module Persistent = struct
  type t = Persistent_connection.t [@@deriving sexp_of]

  let create ?random_state ?retry_delay ?time_source ?ssl ~server_name address =
    Persistent_connection.create
      ~server_name
      ~address:(module Address)
      ?retry_delay
      ?time_source
      ?random_state
      ~connect:(fun address -> Connection.create ?ssl address)
      address
  ;;

  let closed t = Persistent_connection.close_finished t
  let is_closed t = Persistent_connection.is_closed t
  let close t = Persistent_connection.close t

  let call t request =
    match%bind Persistent_connection.connected_or_failed_to_connect t with
    | Ok conn -> Connection.call conn request
    | Error err -> Error.raise err
  ;;
end

module Oneshot = struct
  let call ?interrupt ?connect_timeout ?ssl address request =
    let%bind conn =
      Deferred.Or_error.ok_exn
        (Connection.create ?ssl ?connect_timeout ?interrupt address)
    in
    Connection.call conn request
  ;;
end
