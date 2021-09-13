open! Core
open! Async
open! Shuttle
open! Expect_test_config

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
