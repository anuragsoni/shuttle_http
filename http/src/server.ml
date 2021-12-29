open Import

let[@inline] transfer_encoding headers =
  match List.rev @@ Headers.find_multi headers "Transfer-Encoding" with
  | x :: _ when caseless_equal x "chunked" -> `Chunked
  | _x :: _ -> `Bad_request
  | [] ->
    (match
       List.sort_uniq String.compare (Headers.find_multi headers "Content-Length")
     with
    | [] -> `Fixed 0
    | [ x ] -> `Fixed (int_of_string x)
    | _ -> `Bad_request)
;;

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
  end

  module Body = struct
    module Fixed = struct
      let rec read_upto reader len sink =
        let view = Reader.view reader in
        let to_read = min len (Reader.View.length view) in
        if to_read = 0
        then
          Reader.refill reader
          >>= function
          | `Eof -> return `Eof
          | `Ok -> read_upto reader len sink
        else
          sink (Reader.View.buf view) ~pos:(Reader.View.pos view) ~len:to_read
          >>= fun () ->
          Reader.View.consume view to_read;
          return (`Ok to_read)
      ;;

      let rec consume_fixed reader to_read sink =
        if to_read = 0
        then sink "" ~pos:0 ~len:0 >>= fun () -> return `Ok
        else
          read_upto reader to_read sink
          >>= function
          | `Eof -> sink "" ~pos:0 ~len:0 >>= fun () -> return `Bad_request
          | `Ok read -> consume_fixed reader (to_read - read) sink
      ;;
    end

    module Chunked = struct
      let rec consume_chunk reader state sink =
        let view = Reader.view reader in
        let buf = Reader.View.buf view in
        let pos = Reader.View.pos view in
        let len = Reader.View.length view in
        match Parser.parse_chunk ~pos ~len buf state with
        | Error Partial ->
          Reader.refill reader
          >>= (function
          | `Eof -> sink "" ~pos:0 ~len:0 >>= fun () -> return `Bad_request
          | `Ok -> consume_chunk reader state sink)
        | Error (Msg msg) -> failwith msg
        | Ok (parse_result, consumed) ->
          Reader.View.consume view consumed;
          (match parse_result with
          | Parser.Chunk_complete chunk ->
            sink chunk.buf ~pos:chunk.pos ~len:chunk.len
            >>= fun () -> consume_chunk reader Parser.Start_chunk sink
          | Parser.Done -> sink "" ~pos:0 ~len:0 >>= fun () -> return `Ok
          | Parser.Partial_chunk (chunk, to_consume) ->
            sink chunk.buf ~pos:chunk.pos ~len:chunk.len
            >>= fun () -> consume_chunk reader (Parser.Continue_chunk to_consume) sink)
      ;;
    end

    let write_chunk writer chunk ~pos ~len =
      Printf.ksprintf (fun chunk_len -> Writer.write writer chunk_len) "%x\r\n" len;
      Writer.write writer ~pos ~len chunk;
      Writer.write writer "\r\n"
    ;;

    let final_chunk = "0\r\n\r\n"
  end

  type response = [ `Response of Response.t ] Deferred.t
  type sink = string -> pos:int -> len:int -> unit Deferred.t

  let write_response writer response =
    Writer.write writer (Version.to_string (Response.version response));
    Writer.write_char writer ' ';
    Writer.write writer (Status.to_string (Response.status response));
    Writer.write_char writer ' ';
    Writer.write writer (Status.to_reason_phrase (Response.status response));
    Writer.write writer "\r\n";
    Headers.iter (Response.headers response) ~f:(fun ~key ~data ->
        Writer.write writer key;
        Writer.write writer ": ";
        Writer.write writer data;
        Writer.write writer "\r\n");
    Writer.write writer "\r\n"
  ;;

  module Context = struct
    type t =
      { writer : Writer.t
      ; request : Request.t
      }

    let create writer request = { writer; request }
    let request t = t.request

    let respond_with_string t response body =
      write_response t.writer response;
      Writer.write t.writer body;
      Writer.flush t.writer >>= fun () -> return (`Response response)
    ;;

    let respond_with_stream t response pull =
      write_response t.writer response;
      let rec loop () =
        pull ()
        >>= function
        | None ->
          Writer.write t.writer Body.final_chunk;
          Writer.flush t.writer >>= fun () -> Deferred.return (`Response response)
        | Some chunk ->
          Body.write_chunk t.writer chunk ~pos:0 ~len:(String.length chunk);
          Writer.flush t.writer >>= fun () -> loop ()
      in
      loop ()
    ;;
  end

  type 'a t =
    { reader : Reader.t
    ; writer : Writer.t
    ; on_request : Request.t -> 'a * sink
    ; handler : 'a -> Context.t -> response
    ; error_handler : ?request:Request.t -> Status.t -> (Response.t * string) Deferred.t
    }

  let requests conn =
    let rec loop () =
      let view = Reader.view conn.reader in
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
        Reader.refill conn.reader
        >>= (function
        | `Eof -> Deferred.Option.none
        | `Ok -> loop ())
      | Error (Msg _msg) ->
        conn.error_handler `Bad_request
        >>= fun (response, body) ->
        let response =
          Response.add_header_if_missing response ~key:"Connection" ~data:"close"
        in
        write_response conn.writer response;
        Writer.write conn.writer body;
        Writer.flush conn.writer >>= fun () -> Deferred.Option.none
    in
    Pull.create loop
  ;;

  let keep_alive resp =
    match Headers.find (Response.headers resp) "connection" with
    | Some x when caseless_equal x "close" -> false
    | _ -> true
  ;;

  let consume_body reader req sink =
    match transfer_encoding (Request.headers req) with
    | `Bad_request -> return `Bad_request
    | `Fixed 0 -> sink "" ~pos:0 ~len:0 >>= fun () -> return `Ok
    | `Fixed len -> Body.Fixed.consume_fixed reader len sink
    | `Chunked -> Body.Chunked.consume_chunk reader Parser.Start_chunk sink
  ;;

  let run reader writer on_request handler error_handler =
    let conn = { reader; writer; on_request; handler; error_handler } in
    let requests = requests conn in
    let rec aux () =
      Pull.read requests
      >>= function
      | None -> Deferred.unit
      | Some req ->
        let ctx, sink = on_request req in
        let context = Context.create writer req in
        Deferred.both (consume_body reader req sink) (conn.handler ctx context)
        >>= (function
        | `Bad_request, _ ->
          conn.error_handler ~request:req `Bad_request
          >>= fun (response, body) ->
          write_response conn.writer response;
          Writer.write conn.writer body;
          Writer.flush conn.writer
        | _, `Response response -> if keep_alive response then aux () else Deferred.unit)
    in
    aux ()
  ;;
end
