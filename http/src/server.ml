open! Core
open! Async
open! Shuttle.Std

type error_handler = ?exn:Exn.t -> ?request:Request.t -> Status.t -> Response.t Deferred.t
[@@deriving sexp_of]

let default_error_handler ?exn:_ ?request:_ status =
  return
    (Response.create
       ~headers:(Headers.of_rev_list [ "Connection", "close"; "Content-Length", "0" ])
       ~body:Body.empty
       status)
;;

type handler = Request.t -> Response.t Deferred.t [@@deriving sexp_of]

type t =
  { closed : unit Ivar.t
  ; monitor : Monitor.t
  ; reader : Input_channel.t
  ; writer : Output_channel.t
  ; error_handler : error_handler
  ; handler : handler
  }
[@@deriving sexp_of]

let write_response t res =
  Output_channel.write t.writer (Version.to_string (Response.version res));
  Output_channel.write_char t.writer ' ';
  Output_channel.write t.writer (Status.to_string (Response.status res));
  Output_channel.write_char t.writer ' ';
  Output_channel.write t.writer "\r\n";
  let headers = Response.headers res in
  let headers, is_chunked =
    match Response.body res with
    | Body.Empty ->
      Headers.add_unless_exists headers ~key:"Content-Length" ~data:"0", false
    | Body.Fixed x ->
      ( Headers.add_unless_exists
          headers
          ~key:"Content-Length"
          ~data:(Int.to_string (String.length x))
      , false )
    | Body.Stream stream ->
      let module M = (val stream : Stream_intf.S) in
      (match M.encoding () with
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
      Output_channel.write t.writer key;
      Output_channel.write t.writer ": ";
      Output_channel.write t.writer data;
      Output_channel.write t.writer "\r\n")
    headers;
  Output_channel.write t.writer "\r\n";
  match Response.body res with
  | Body.Empty -> Output_channel.flush t.writer
  | Body.Fixed x ->
    Output_channel.write t.writer x;
    Output_channel.flush t.writer
  | Body.Stream stream ->
    let module M = (val stream : Stream_intf.S) in
    Deferred.repeat_until_finished () (fun () ->
      M.read ()
      >>= function
      | `Eof -> return (`Finished ())
      | `Ok v ->
        if String.is_empty v
        then return (`Repeat ())
        else if is_chunked
        then (
          Output_channel.writef t.writer "%x\r\n" (String.length v);
          Output_channel.write t.writer v;
          Output_channel.write t.writer "\r\n";
          let%map () = Output_channel.flush t.writer in
          `Repeat ())
        else (
          Output_channel.write t.writer v;
          let%map () = Output_channel.flush t.writer in
          `Repeat ()))
;;

let create ?(error_handler = default_error_handler) reader writer handler =
  { closed = Ivar.create ()
  ; monitor = Monitor.create ()
  ; reader
  ; writer
  ; error_handler
  ; handler
  }
;;

let handle_error t =
  Monitor.detach_and_get_next_error t.monitor
  >>> fun exn ->
  t.error_handler ~exn `Internal_server_error
  >>> fun response ->
  if Ivar.is_empty t.closed
  then write_response t response >>> fun () -> Ivar.fill t.closed ()
;;

let keep_alive headers =
  match Headers.find headers "connection" with
  | Some x when String.Caseless.equal x "close" -> false
  | _ -> true
;;

let run t =
  let rec loop t =
    let view = Input_channel.view t.reader in
    match Parser.parse_request view.buf ~pos:view.pos ~len:view.len with
    | Error Partial ->
      Input_channel.refill t.reader
      >>> (function
      | `Eof -> Ivar.fill t.closed ()
      | `Ok -> loop t)
    | Error (Fail error) ->
      t.error_handler ~exn:(Error.to_exn error) `Bad_request
      >>> fun response -> write_response t response >>> fun () -> Ivar.fill t.closed ()
    | Ok (req, consumed) ->
      Input_channel.consume t.reader consumed;
      t.handler req
      >>> fun response ->
      let is_keep_alive =
        keep_alive (Request.headers req) && keep_alive (Response.headers response)
      in
      write_response t response
      >>> fun () -> if is_keep_alive then loop t else Ivar.fill t.closed ()
  in
  loop t;
  handle_error t;
  Ivar.read t.closed
;;
