open! Core
open! Async
include Output_channel0
module Input_channel = Input_channel0

let of_pipe info pipe_writer =
  Async.Unix.pipe info
  >>| fun (`Reader rd, `Writer wr) ->
  let input_channel = Input_channel.create rd in
  let output_channel = create wr in
  let flushed =
    let%bind () = Input_channel.write_to_pipe input_channel pipe_writer in
    let%map () = Input_channel.close input_channel
    and () = close output_channel in
    Pipe.close pipe_writer
  in
  output_channel, flushed
;;
