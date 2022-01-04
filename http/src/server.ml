module Make (IO : Io_intf.S) = struct
  open IO

  module Deferred = struct
    include Deferred

    let unit = return ()

    module Infix = struct
      let return x = return x
      let ( >>= ) = ( >>= )
    end

    module Option = struct
      let none = return None
      let some x = return (Some x)
    end
  end

  open Deferred.Infix

  module Pull = struct
    type 'a t = { mutable next : unit -> 'a option Deferred.t }

    let create next =
      let t = { next } in
      let next () =
        next ()
        >>= function
        | None ->
          t.next <- (fun () -> return None);
          Deferred.Option.none
        | r -> return r
      in
      t.next <- next;
      t
    ;;

    let read t = t.next ()
    let empty () = { next = (fun () -> Deferred.Option.none) }

    let iter t ~f =
      let rec loop t ~f =
        t.next ()
        >>= function
        | None -> Deferred.unit
        | Some v -> f v >>= fun () -> loop t ~f
      in
      loop t ~f
    ;;

    let drain t =
      let rec loop t =
        t.next ()
        >>= function
        | None -> Deferred.unit
        | Some _ -> loop t
      in
      loop t
    ;;
  end

  module Body = struct
    module Fixed = struct
      type t = { mutable to_read : int }

      let rec read_upto reader len =
        let view = Reader.view reader in
        let to_read = min len (Reader.View.length view) in
        if to_read = 0
        then
          Reader.refill reader
          >>= function
          | `Eof -> return `Eof
          | `Ok -> read_upto reader len
        else (
          let chunk = String.sub (Reader.View.buf view) (Reader.View.pos view) to_read in
          Reader.View.consume view to_read;
          return (`Ok chunk))
      ;;

      let create reader len =
        let t = { to_read = len } in
        let next () =
          if t.to_read = 0
          then Deferred.Option.none
          else
            read_upto reader len
            >>= function
            | `Eof -> Deferred.Option.none
            | `Ok chunk ->
              t.to_read <- t.to_read - String.length chunk;
              Deferred.Option.some chunk
        in
        Pull.create next
      ;;
    end

    module Chunked = struct
      type t = { mutable state : Parser.chunk_kind }

      let create reader =
        let t = { state = Parser.Start_chunk } in
        let rec next () =
          let view = Reader.view reader in
          let buf = Reader.View.buf view in
          let pos = Reader.View.pos view in
          let len = Reader.View.length view in
          match Parser.parse_chunk ~pos ~len buf t.state with
          | Ok (parse_result, consumed) ->
            Reader.View.consume view consumed;
            (match parse_result with
            | Parser.Chunk_complete chunk ->
              t.state <- Parser.Start_chunk;
              Deferred.Option.some (String.sub chunk.buf chunk.pos chunk.len)
            | Parser.Done -> Deferred.Option.none
            | Parser.Partial_chunk (chunk, to_consume) ->
              t.state <- Parser.Continue_chunk to_consume;
              Deferred.Option.some (String.sub chunk.buf chunk.pos chunk.len))
          | Error (Msg msg) -> failwith msg
          | Error Partial ->
            Reader.refill reader
            >>= (function
            | `Eof -> Deferred.Option.none
            | `Ok -> next ())
        in
        Pull.create next
      ;;
    end

    module Reader = struct
      type t = string Pull.t

      let create = Pull.create
      let iter t ~f = Pull.iter t ~f
      let drain t = Pull.drain t
      let read t = Pull.read t
    end

    let write_chunk writer chunk =
      let len = String.length chunk in
      Printf.ksprintf (fun chunk_len -> Writer.write writer chunk_len) "%x\r\n" len;
      Writer.write writer chunk;
      Writer.write writer "\r\n"
    ;;

    let final_chunk = "0\r\n\r\n"
  end

  let write_response writer response =
    let open Cohttp in
    Writer.write writer (Code.string_of_version (Response.version response));
    Writer.write_char writer ' ';
    Writer.write writer (Code.string_of_status (Response.status response));
    Writer.write_char writer ' ';
    Writer.write
      writer
      (Code.reason_phrase_of_code (Code.code_of_status (Response.status response)));
    Writer.write writer "\r\n";
    Header.iter
      (fun key data ->
        Writer.write writer key;
        Writer.write writer ": ";
        Writer.write writer data;
        Writer.write writer "\r\n")
      (Response.headers response);
    Writer.write writer "\r\n"
  ;;

  module Connection = struct
    type response =
      | Keep_alive
      | Close

    type sink = string -> unit Deferred.t

    type t =
      { reader : Reader.t
      ; writer : Writer.t
      ; request : Cohttp.Request.t
      ; request_body : string Pull.t
      ; handler : t -> response Deferred.t
      ; error_handler :
          ?request:Cohttp.Request.t
          -> Cohttp.Code.status_code
          -> (Cohttp.Response.t * string) Deferred.t
      }

    let request t = t.request
    let request_body t = t.request_body

    let respond_with_string t response body =
      write_response t.writer response;
      Writer.write t.writer body;
      Writer.flush t.writer
      >>= fun () ->
      if not
           Cohttp.(
             Header.get_connection_close (Request.headers t.request)
             || Header.get_connection_close (Response.headers response))
      then return Keep_alive
      else return Close
    ;;

    let respond_with_stream t response stream =
      write_response t.writer response;
      Body.Reader.iter stream ~f:(fun buf ->
          Body.write_chunk t.writer buf;
          Writer.flush t.writer)
      >>= fun () ->
      Writer.write t.writer Body.final_chunk;
      Writer.flush t.writer
      >>= fun () ->
      if not
           Cohttp.(
             Header.get_connection_close (Request.headers t.request)
             || Header.get_connection_close (Response.headers response))
      then return Keep_alive
      else return Close
    ;;
  end

  let requests
      reader
      writer
      (error_handler :
        ?request:Cohttp.Request.t
        -> Cohttp.Code.status_code
        -> (Cohttp.Response.t * string) Deferred.t)
    =
    let rec loop () =
      let view = Reader.view reader in
      match
        Parser.parse_request
          ~pos:(Reader.View.pos view)
          ~len:(Reader.View.length view)
          (Reader.View.buf view)
      with
      | Ok (req, consumed) ->
        Reader.View.consume view consumed;
        Deferred.Option.some req
      | Error Parser.Partial ->
        Reader.refill reader
        >>= (function
        | `Eof -> Deferred.Option.none
        | `Ok -> loop ())
      | Error (Msg _msg) ->
        error_handler `Bad_request
        >>= fun (response, body) ->
        let headers = Cohttp.Response.headers response in
        let response =
          { response with
            headers = Cohttp.Header.add_unless_exists headers "Connection" "close"
          }
        in
        write_response writer response;
        Writer.write writer body;
        Writer.flush writer >>= fun () -> Deferred.Option.none
    in
    Pull.create loop
  ;;

  let create_body_reader reader req =
    match Cohttp.Request.encoding req with
    | Cohttp.Transfer.Unknown -> Pull.empty ()
    | Cohttp.Transfer.Fixed 0L -> Pull.empty ()
    | Cohttp.Transfer.Fixed len -> Body.Fixed.create reader (Int64.to_int len)
    | Cohttp.Transfer.Chunked -> Body.Chunked.create reader
  ;;

  let run reader writer handler error_handler =
    let requests = requests reader writer error_handler in
    let rec aux () =
      Pull.read requests
      >>= function
      | None -> Deferred.unit
      | Some req ->
        let request_body = create_body_reader reader req in
        let conn =
          { Connection.reader
          ; writer
          ; handler
          ; error_handler
          ; request = req
          ; request_body
          }
        in
        conn.handler conn
        >>= (function
        | Connection.Keep_alive -> Pull.drain conn.request_body >>= fun () -> aux ()
        | Connection.Close -> Pull.drain conn.request_body)
    in
    aux ()
  ;;
end
