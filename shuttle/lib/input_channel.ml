open! Core
open! Async
include Input_channel0
module Output_channel = Output_channel0

let buffer_size t = Bytebuffer.capacity t.buf
let time_source t = t.time_source

let of_pipe ?buf_len info reader =
  Unix.pipe info
  >>| fun (`Reader rd, `Writer wr) ->
  let input_channel = create ?buf_len rd in
  let output_channel = Output_channel.create ?buf_len wr in
  don't_wait_for
    (let%bind () = Output_channel.write_from_pipe output_channel reader in
     Output_channel.close output_channel);
  input_channel
