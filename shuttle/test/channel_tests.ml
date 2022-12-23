open! Core
open! Async
open! Shuttle

let stdout = Lazy.force Writer.stdout

let write_chunks_to_chan wr chunks =
  Deferred.List.iter ~how:`Sequential chunks ~f:(fun chunk ->
    Output_channel.write wr chunk;
    Output_channel.flush wr)
;;

let%expect_test "create a pipe from an input_channel" =
  Unix.pipe (Info.of_string "test input_channel")
  >>= fun (`Reader reader, `Writer writer) ->
  let rd = Input_channel.create reader in
  let wr = Output_channel.create writer in
  let pipe = Input_channel.pipe rd in
  don't_wait_for
    (write_chunks_to_chan
       wr
       [ "Hello, World!"; " This is a line\nWith some more data as part of this chunk\n" ]
    >>= fun () -> Output_channel.close wr);
  let%map () =
    Pipe.iter_without_pushback pipe ~f:(fun payload -> Writer.write stdout payload)
  in
  [%expect
    {|
    Hello, World! This is a line
    With some more data as part of this chunk |}];
  Writer.writef stdout "Input_channel closed? %b" (Input_channel.is_closed rd);
  [%expect {| Input_channel closed? true |}]
;;

let%expect_test "create a pipe from an output channel" =
  Unix.pipe (Info.of_string "test input_channel")
  >>= fun (`Reader reader, `Writer writer) ->
  let rd = Input_channel.create reader in
  let wr = Output_channel.create writer in
  let pipe_w = Output_channel.pipe wr in
  let pipe_r = Input_channel.pipe rd in
  don't_wait_for
    (Deferred.List.iter
       ~how:`Sequential
       [ "Hello!!"; " This is another chunk.\n"; "Pushing to writer\n" ]
       ~f:(fun msg -> Pipe.write pipe_w msg)
    >>= fun () -> Output_channel.close wr);
  let%map () =
    Pipe.iter_without_pushback pipe_r ~f:(fun msg -> Writer.write stdout msg)
  in
  [%expect {|
    Hello!! This is another chunk.
    Pushing to writer |}]
;;

let%expect_test "create input_channel from pipe" =
  let p = Pipe.of_list [ "hello!"; " this is a chunk.\n"; "Another line!\n" ] in
  Input_channel.of_pipe (Info.of_string "testing") p
  >>= fun rd ->
  let pipe_r = Input_channel.pipe rd in
  let%map () =
    Pipe.iter_without_pushback pipe_r ~f:(fun msg -> Writer.write stdout msg)
  in
  [%expect {|
    hello! this is a chunk.
    Another line! |}]
;;

let%expect_test "create output_channel from pipe" =
  let rd, wr = Pipe.create () in
  let%bind t, _flushed = Output_channel.of_pipe (Info.of_string "testing") wr in
  Output_channel.write t "Hello";
  Output_channel.schedule_flush t;
  Output_channel.write t " World!\n";
  Output_channel.schedule_flush t;
  Output_channel.writef t "Another line using writef %d %b\n" 12 false;
  Output_channel.schedule_flush t;
  don't_wait_for (Output_channel.flushed t >>= fun () -> Output_channel.close t);
  let%map () = Pipe.iter_without_pushback rd ~f:(fun msg -> Writer.write stdout msg) in
  [%expect {|
    Hello World!
    Another line using writef 12 false |}]
;;

let%expect_test "Internal buffer automatically increases in size" =
  Unix.pipe (Info.of_string "test input_channel")
  >>= fun (`Reader reader, `Writer writer) ->
  let rd = Input_channel.create ~buf_len:16 reader in
  let wr = Output_channel.create writer in
  don't_wait_for
    (Output_channel.write wr "hello World ";
     Output_channel.write wr "this is another block of text";
     Output_channel.write wr (String.init 54 ~f:(fun _ -> 'a'));
     Output_channel.flush wr >>= fun () -> Output_channel.close wr);
  let%map () =
    Deferred.create (fun ivar ->
      let rec loop () =
        Input_channel.refill rd
        >>> function
        | `Eof -> Ivar.fill ivar ()
        | `Ok -> loop ()
      in
      loop ())
  in
  let view = Input_channel.view rd in
  let content = Bigstring.to_string view.buf ~pos:view.pos ~len:view.len in
  Writer.writef
    stdout
    "Content: %s\n Internal buffer length: %d\n"
    content
    (Bigstring.length view.buf);
  [%expect
    {|
      Content: hello World this is another block of textaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
       Internal buffer length: 128 |}]
;;

let%expect_test "Flush operation reports when the remote peer is closed" =
  let reader_fd, writer_fd = Unix.socketpair () in
  let _reader = Input_channel.create reader_fd in
  let writer = Output_channel.create writer_fd in
  (* Suppress exceptions as we want to test the flush behavior in this test. Without this,
     there is a race condition between the exception that'll be raised because of
     attempting to write to a closed FD, and the deferred value being resolved. *)
  Monitor.detach_and_iter_errors (Output_channel.monitor writer) ~f:(fun _error -> ());
  Output_channel.write writer "Hello World";
  (* Flush operation can respond if previous writes were successful *)
  let%bind flush_result = Output_channel.flush_or_fail writer in
  Writer.writef stdout !"%{sexp: Output_channel.Flush_result.t}" flush_result;
  [%expect {| Flushed |}];
  (* Close the reader end so the next flush operation results in an error *)
  let%bind () = Fd.close reader_fd in
  Output_channel.write writer "foo bar";
  let%map flush_result = Output_channel.flush_or_fail writer in
  Writer.writef stdout !"%{sexp: Output_channel.Flush_result.t}" flush_result;
  [%expect {| Remote_closed |}]
;;

let%expect_test "Flush operation reports when attempting to write on a closed FD" =
  let _reader_fd, writer_fd = Unix.socketpair () in
  let writer = Output_channel.create writer_fd in
  (* Suppress exceptions as we want to test the flush behavior in this test. Without this,
     there is a race condition between the exception that'll be raised because of
     attempting to write to a closed FD, and the deferred value being resolved. *)
  Monitor.detach_and_iter_errors (Output_channel.monitor writer) ~f:(fun _error -> ());
  Output_channel.write writer "Hello World";
  (* Flush operation can respond if previous writes were successful *)
  let%bind flush_result = Output_channel.flush_or_fail writer in
  Writer.writef stdout !"%{sexp: Output_channel.Flush_result.t}" flush_result;
  [%expect {| Flushed |}];
  (* Close the writer end so the next flush operation results in an error *)
  let%bind () = Fd.close writer_fd in
  Output_channel.write writer "foo bar";
  let%map flush_result = Output_channel.flush_or_fail writer in
  Writer.writef stdout !"%{sexp: Output_channel.Flush_result.t}" flush_result;
  [%expect {| Error |}]
;;
