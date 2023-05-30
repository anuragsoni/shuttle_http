open Core
open Eio.Std

let keep_alive headers =
  match Headers.find headers "connection" with
  | Some x when String.Caseless.equal x "close" -> false
  | _ -> true
;;

type error_handler = ?exn:Exn.t -> ?request:Request.t -> Status.t -> Response.t

let default_error_handler ?exn:_ ?request:_ status =
  Response.create
    ~headers:(Headers.of_rev_list [ "Connection", "close"; "Content-Length", "0" ])
    ~body:Body.empty
    status
;;

module Config = struct
  type t =
    { buf_len : int
    ; max_buffer_size : int option
    ; max_connections : int option
    ; backlog : int
    ; error_handler : error_handler
    }

  let create
    ?(buf_len = 0x4000)
    ?max_buffer_size
    ?max_connections
    ?(error_handler = default_error_handler)
    ?(backlog = 128)
    ()
    =
    { buf_len; max_buffer_size; max_connections; backlog; error_handler }
  ;;

  let default = create ~backlog:128 ()
end

type service = Request.t -> Response.t

type t =
  { closed : unit Promise.t * unit Promise.u
  ; reader : Eio.Buf_read.t
  ; writer : Eio.Buf_write.t
  ; error_handler : error_handler
  ; sw : Eio.Switch.t
  }

let closed t = fst t.closed

let close t =
  if Fn.non Promise.is_resolved (fst t.closed) then Promise.resolve (snd t.closed) ()
;;

let write_response t res =
  let open Eio in
  Buf_write.pause t.writer;
  Buf_write.string t.writer (Version.to_string (Response.version res));
  Buf_write.char t.writer ' ';
  Buf_write.string t.writer (Status.to_string (Response.status res));
  Buf_write.char t.writer ' ';
  Buf_write.string t.writer "\r\n";
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
  in
  Headers.iter
    ~f:(fun ~key ~data ->
      Buf_write.string t.writer key;
      Buf_write.string t.writer ": ";
      Buf_write.string t.writer data;
      Buf_write.string t.writer "\r\n")
    headers;
  Buf_write.string t.writer "\r\n";
  match Response.body res with
  | Body.Empty -> Buf_write.flush t.writer
  | Body.Fixed x ->
    Buf_write.string t.writer x;
    Buf_write.flush t.writer
;;

let create ?(error_handler = default_error_handler) ~sw reader writer =
  { closed = Promise.create (); reader; writer; error_handler; sw }
;;

let run_server_loop t handler =
  let rec parse_request t =
    let view = Eio.Buf_read.peek t.reader in
    match Parser.parse_request view.buffer ~pos:view.off ~len:view.len with
    | Error Partial ->
      Eio.Buf_read.ensure t.reader (view.len + 1);
      parse_request t
    | Error (Fail error) ->
      let response = t.error_handler ~exn:(Error.to_exn error) `Bad_request in
      write_response t response;
      close t
    | Ok (req, consumed) ->
      Eio.Buf_read.consume t.reader consumed;
      create_request_body_reader t req
  and create_request_body_reader t req =
    let response = handler req in
    write_response_and_continue t req response
  and write_response_and_continue t req response =
    let is_keep_alive =
      keep_alive (Request.headers req) && keep_alive (Response.headers response)
    in
    write_response t response;
    if is_keep_alive then parse_request t else close t
  in
  parse_request t
;;

let run ~(config : Config.t) ~sw ~net addr handler =
  let sock = Eio.Net.listen net ~sw ~backlog:config.backlog addr in
  Eio.Net.run_server
    ?max_connections:config.max_connections
    ~on_error:raise
    sock
    (fun flow _addr ->
    let max_size =
      match config.max_buffer_size with
      | Some s -> s
      | None -> Int.max_value
    in
    let reader = Eio.Buf_read.of_flow flow ~initial_size:config.buf_len ~max_size in
    Eio.Buf_write.with_flow ~initial_size:config.buf_len flow (fun writer ->
      let t = create ~error_handler:config.error_handler ~sw reader writer in
      Switch.on_release sw (fun () -> close t);
      try run_server_loop t handler with
      | End_of_file -> ()))
;;
