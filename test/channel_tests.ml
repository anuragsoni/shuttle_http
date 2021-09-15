open! Core
open! Async
open! Shuttle
open! Expect_test_config_with_unit_expect

let stdout = Lazy.force Writer.stdout

let write_chunks_to_chan wr chunks =
  Deferred.List.iter ~how:`Sequential chunks ~f:(fun chunk ->
      Output_channel.write_string wr chunk;
      Output_channel.flush wr;
      Output_channel.flushed wr)
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
  let%bind () =
    Pipe.iter_without_pushback pipe ~f:(fun payload -> Writer.write stdout payload)
  in
  let%bind () =
    [%expect
      {|
    Hello, World! This is a line
    With some more data as part of this chunk |}]
  in
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
  let%bind () =
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
  let%bind () =
    Pipe.iter_without_pushback pipe_r ~f:(fun msg -> Writer.write stdout msg)
  in
  [%expect {|
    hello! this is a chunk.
    Another line! |}]
;;

let%expect_test "create output_channel from pipe" =
  let rd, wr = Pipe.create () in
  let%bind t, _flushed = Output_channel.of_pipe (Info.of_string "testing") wr in
  Output_channel.write_string t "Hello";
  Output_channel.flush t;
  Output_channel.write_string t " World!\n";
  Output_channel.flush t;
  Output_channel.write_string t "Another line\n";
  Output_channel.flush t;
  don't_wait_for (Output_channel.flushed t >>= fun () -> Output_channel.close t);
  let%bind () = Pipe.iter_without_pushback rd ~f:(fun msg -> Writer.write stdout msg) in
  [%expect {|
    Hello World!
    Another line |}]
;;
