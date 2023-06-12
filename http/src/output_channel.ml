open! Core
open! Async
include Output_channel0
module Input_channel = Input_channel0

let write_timeout t = t.write_timeout
let time_source t = t.time_source
let buffer_size t = Bytebuffer.capacity t.buf
let max_buffer_size t = Bytebuffer.max_buffer_size t.buf

let of_pipe ?max_buffer_size ?buf_len info pipe_writer =
  Async.Unix.pipe info
  >>| fun (`Reader rd, `Writer wr) ->
  let input_channel = Input_channel.create ?max_buffer_size ?buf_len rd in
  let output_channel = create ?max_buffer_size ?buf_len wr in
  let flushed =
    let%bind () = Input_channel.transfer input_channel pipe_writer in
    let%map () = Input_channel.close input_channel
    and () = close output_channel in
    Pipe.close pipe_writer
  in
  output_channel, flushed
;;
