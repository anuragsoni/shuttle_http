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
              Deferred.Option.some chunk
            | Parser.Done -> Deferred.Option.none
            | Parser.Partial_chunk (chunk, to_consume) ->
              t.state <- Parser.Continue_chunk to_consume;
              Deferred.Option.some chunk)
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

  type t =
    { reader : Reader.t
    ; writer : Writer.t
    ; handler : Request.t -> string Pull.t -> (Response.t * Body.t) Deferred.t
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

  let create_body_stream reader req =
    match transfer_encoding (Request.headers req) with
    | `Bad_request -> `Bad_request
    | `Fixed 0 -> `Ok (Pull.empty ())
    | `Fixed len -> `Ok (Body.Fixed.create reader len)
    | `Chunked -> `Ok (Body.Chunked.create reader)
  ;;

  let run reader writer handler error_handler =
    let conn = { reader; writer; handler; error_handler } in
    let requests = requests conn in
    let rec aux () =
      Pull.read requests
      >>= function
      | None -> Deferred.unit
      | Some req ->
        (match create_body_stream reader req with
        | `Ok body ->
          conn.handler req body
          >>= fun (response, body) ->
          write_response conn.writer response;
          Body.write_body body conn.writer
          >>= fun () -> if keep_alive response then aux () else Deferred.unit
        | `Bad_request ->
          conn.error_handler ~request:req `Bad_request
          >>= fun (response, body) ->
          write_response conn.writer response;
          Writer.write conn.writer body;
          Writer.flush conn.writer)
    in
    aux ()
  ;;
end
