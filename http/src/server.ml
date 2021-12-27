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
    let empty () = { next = (fun () -> Deferred.Option.none) }

    let of_list xs =
      let xs = ref xs in
      let next () =
        match !xs with
        | [] -> Deferred.Option.none
        | x :: xs' ->
          xs := xs';
          Deferred.Option.some x
      in
      { next }
    ;;

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

      let rec consume_fixed reader to_read sink =
        if to_read = 0
        then return `Ok
        else
          read_upto reader to_read
          >>= function
          | `Eof -> sink None >>= fun () -> return `Bad_request
          | `Ok chunk ->
            sink (Some chunk)
            >>= fun () -> consume_fixed reader (to_read - String.length chunk) sink
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
          | `Eof -> sink None >>= fun () -> return `Bad_request
          | `Ok -> consume_chunk reader state sink)
        | Error (Msg msg) -> failwith msg
        | Ok (parse_result, consumed) ->
          Reader.View.consume view consumed;
          (match parse_result with
          | Parser.Chunk_complete chunk ->
            sink (Some chunk) >>= fun () -> consume_chunk reader Parser.Start_chunk sink
          | Parser.Done -> sink None >>= fun () -> return `Ok
          | Parser.Partial_chunk (chunk, to_consume) ->
            sink (Some chunk)
            >>= fun () -> consume_chunk reader (Parser.Continue_chunk to_consume) sink)
      ;;
    end

    type t =
      | Fixed of string
      | Stream of string Pull.t

    let string s = Fixed s
    let stream s = Stream s

    let write_chunk writer chunk =
      let len = String.length chunk in
      Printf.ksprintf (fun chunk_len -> Writer.write writer chunk_len) "%x\r\n" len;
      Writer.write writer chunk;
      Writer.write writer "\r\n"
    ;;

    let final_chunk = "0\r\n\r\n"

    let write_body t writer =
      match t with
      | Fixed s ->
        Writer.write writer s;
        Writer.flush writer
      | Stream stream ->
        Pull.iter stream ~f:(fun chunk ->
            write_chunk writer chunk;
            Writer.flush writer)
        >>= fun () ->
        Writer.write writer final_chunk;
        Writer.flush writer
    ;;
  end

  type sink = string option -> unit Deferred.t

  type 'a t =
    { reader : Reader.t
    ; writer : Writer.t
    ; on_request : Request.t -> 'a * sink
    ; handler : 'a -> Request.t -> (Response.t * Body.t) Deferred.t
    ; error_handler : ?request:Request.t -> Status.t -> (Response.t * string) Deferred.t
    }

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

  let[@inline] is_chunked_response resp =
    match transfer_encoding (Response.headers resp) with
    | `Chunked -> true
    | `Bad_request | `Fixed _ -> false
  ;;

  let keep_alive resp =
    match Headers.find (Response.headers resp) "connection" with
    | Some x when caseless_equal x "close" -> false
    | _ -> true
  ;;

  let consume_body reader req sink =
    match transfer_encoding (Request.headers req) with
    | `Bad_request -> return `Bad_request
    | `Fixed 0 -> sink None >>= fun () -> return `Ok
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
        consume_body reader req sink
        >>= (function
        | `Bad_request ->
          conn.error_handler ~request:req `Bad_request
          >>= fun (response, body) ->
          write_response conn.writer response;
          Writer.write conn.writer body;
          Writer.flush conn.writer
        | `Ok ->
          conn.handler ctx req
          >>= fun (response, body) ->
          write_response conn.writer response;
          Body.write_body body conn.writer
          >>= fun () -> if keep_alive response then aux () else Deferred.unit)
    in
    aux ()
  ;;
end
