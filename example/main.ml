open! Core
open! Async
open Httpaf
open Shuttle

let write_iovecs writer iovecs =
  match Writer.is_closed writer with
  | true -> `Closed
  | false ->
    let rec aux acc = function
      | [] -> `Ok acc
      | { Faraday.buffer; off; len } :: xs ->
        Writer.schedule_bigstring writer buffer ~pos:off ~len;
        aux (acc + len) xs
    in
    aux 0 iovecs
;;

module Server = struct
  let create_connection_handler
      ?(config = Config.default)
      ~error_handler
      ~request_handler
      client_addr
      reader
      writer
    =
    let request_handler = request_handler client_addr in
    let error_handler = error_handler client_addr in
    let conn = Server_connection.create ~config ~error_handler request_handler in
    let read_complete = Ivar.create () in
    let rec reader_thread () =
      match Server_connection.next_read_operation conn with
      | `Close -> Ivar.fill read_complete ()
      | `Yield -> Server_connection.yield_reader conn reader_thread
      | `Read ->
        Reader.read_one_chunk_at_a_time reader ~on_chunk:(fun buf ~pos ~len ->
            let consumed = Server_connection.read conn buf ~off:pos ~len in
            Reader.Read_chunk_result.Consumed consumed)
        >>> (function
        | Ok _ -> reader_thread ()
        | Error `Closed -> raise_s [%message "Attempting to read from a closed fd"]
        | Error `Eof ->
          ignore (Server_connection.read_eof conn Bigstringaf.empty ~off:0 ~len:0 : int);
          reader_thread ())
    in
    let write_complete = Ivar.create () in
    let rec writer_thread () =
      match Server_connection.next_write_operation conn with
      | `Write iovecs ->
        let result = write_iovecs writer iovecs in
        Writer.flush writer;
        Writer.flushed writer (fun () ->
            Server_connection.report_write_result conn result;
            writer_thread ())
      | `Close _ -> Ivar.fill write_complete ()
      | `Yield -> Server_connection.yield_writer conn writer_thread
    in
    let monitor = Monitor.create ~here:[%here] ~name:"AsyncHttpServer" () in
    Monitor.detach_and_iter_errors monitor ~f:(fun e ->
        Ivar.fill_if_empty read_complete ();
        Server_connection.report_exn conn e);
    Scheduler.within ~monitor reader_thread;
    Scheduler.within ~monitor writer_thread;
    Deferred.all_unit [ Ivar.read write_complete; Ivar.read write_complete ]
  ;;
end

let text =
  "CHAPTER I. Down the Rabbit-Hole  Alice was beginning to get very tired of sitting by \
   her sister on the bank, and of having nothing to do: once or twice she had peeped \
   into the book her sister was reading, but it had no pictures or conversations in it, \
   <and what is the use of a book,> thought Alice <without pictures or conversations?> \
   So she was considering in her own mind (as well as she could, for the hot day made \
   her feel very sleepy and stupid), whether the pleasure of making a daisy-chain would \
   be worth the trouble of getting up and picking the daisies, when suddenly a White \
   Rabbit with pink eyes ran close by her. There was nothing so very remarkable in that; \
   nor did Alice think it so very much out of the way to hear the Rabbit say to itself, \
   <Oh dear! Oh dear! I shall be late!> (when she thought it over afterwards, it \
   occurred to her that she ought to have wondered at this, but at the time it all \
   seemed quite natural); but when the Rabbit actually took a watch out of its \
   waistcoat-pocket, and looked at it, and then hurried on, Alice started to her feet, \
   for it flashed across her mind that she had never before seen a rabbit with either a \
   waistcoat-pocket, or a watch to take out of it, and burning with curiosity, she ran \
   across the field after it, and fortunately was just in time to see it pop down a \
   large rabbit-hole under the hedge. In another moment down went Alice after it, never \
   once considering how in the world she was to get out again. The rabbit-hole went \
   straight on like a tunnel for some way, and then dipped suddenly down, so suddenly \
   that Alice had not a moment to think about stopping herself before she found herself \
   falling down a very deep well. Either the well was very deep, or she fell very \
   slowly, for she had plenty of time as she went down to look about her and to wonder \
   what was going to happen next. First, she tried to look down and make out what she \
   was coming to, but it was too dark to see anything; then she looked at the sides of \
   the well, and noticed that they were filled with cupboards......"
;;

let text = Bigstring.of_string text

let benchmark =
  let headers =
    Headers.of_list [ "content-length", Int.to_string (Bigstringaf.length text) ]
  in
  let handler reqd =
    let { Request.target; _ } = Reqd.request reqd in
    let request_body = Reqd.request_body reqd in
    Body.close_reader request_body;
    match target with
    | "/" -> Reqd.respond_with_bigstring reqd (Response.create ~headers `OK) text
    | "/yield" ->
      Scheduler.yield ()
      >>> fun () -> Reqd.respond_with_bigstring reqd (Response.create ~headers `OK) text
    | "/delay" ->
      after Time.Span.millisecond
      >>> fun () -> Reqd.respond_with_bigstring reqd (Response.create ~headers `OK) text
    | _ -> Reqd.respond_with_string reqd (Response.create `Not_found) "Route not found"
  in
  handler
;;

let error_handler ?request:_ error start_response =
  let response_body = start_response Headers.empty in
  (match error with
  | `Exn exn ->
    Body.write_string response_body (Exn.to_string exn);
    Body.write_string response_body "\n"
  | #Status.standard as error ->
    Body.write_string response_body (Status.default_reason_phrase error));
  Body.close_writer response_body
;;

let main port max_accepts_per_batch () =
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  let request_handler _ = benchmark in
  let error_handler _ = error_handler in
  let server =
    Tcp.(
      Server.create_sock_inet
        ~on_handler_error:`Ignore
        ~backlog:11_000
        ~max_connections:10_000
        ~max_accepts_per_batch
        where_to_listen)
      (fun addr sock ->
        let fd = Socket.fd sock in
        let reader = Reader.create fd in
        let writer = Writer.create fd in
        Server.create_connection_handler
          addr
          ~request_handler
          ~error_handler
          reader
          writer
        >>= fun () -> Writer.close writer >>= fun () -> Reader.close reader)
  in
  Deferred.forever () (fun () ->
      Clock.after Time.Span.(of_sec 0.5)
      >>| fun () -> Log.Global.printf "conns: %d" (Tcp.Server.num_connections server));
  Deferred.never ()
;;

let () =
  Command.async
    ~summary:"Start a hello world Async server"
    Command.Param.(
      map
        (both
           (flag
              "-p"
              (optional_with_default 8080 int)
              ~doc:"int Source port to listen on")
           (flag "-a" (optional_with_default 1 int) ~doc:"int Maximum accepts per batch"))
        ~f:(fun (port, accepts) () -> main port accepts ()))
  |> Command.run
;;
