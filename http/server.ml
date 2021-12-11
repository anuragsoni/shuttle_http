open Core
open Async
open Shuttle
open Httpaf
open! Import

let create_connection_handler
    ?(config = Config.default)
    ~error_handler
    ~request_handler
    reader
    writer
  =
  let conn = Server_connection.create ~config ~error_handler request_handler in
  let read_complete = Ivar.create () in
  let rec reader_thread () =
    match Server_connection.next_read_operation conn with
    | `Close -> Ivar.fill read_complete ()
    | `Yield -> Server_connection.yield_reader conn reader_thread
    | `Read ->
      Input_channel.read_one_chunk_at_a_time reader ~on_chunk:(fun buf ~pos ~len ->
          let count = Server_connection.read conn buf ~off:pos ~len in
          `Continue count)
      >>> (function
      | `Stopped _ -> reader_thread ()
      | `Eof_with_unconsumed buf ->
        ignore
          (Server_connection.read_eof
             conn
             (Bigstring.of_string buf)
             ~off:0
             ~len:(String.length buf)
            : int);
        reader_thread ()
      | `Eof ->
        ignore (Server_connection.read_eof conn Bigstringaf.empty ~off:0 ~len:0 : int);
        reader_thread ())
  in
  let write_complete = Ivar.create () in
  let rec writer_thread () =
    match Server_connection.next_write_operation conn with
    | `Write iovecs ->
      let result = enqueue_iovecs writer iovecs in
      Output_channel.flush writer;
      Server_connection.report_write_result conn result;
      writer_thread ()
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
