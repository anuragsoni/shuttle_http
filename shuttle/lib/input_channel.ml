open! Core
open! Async
include Input_channel0
module Output_channel = Output_channel0

let buffer_size t = Bytebuffer.capacity t.buf
let time_source t = t.time_source

type slice = Bytebuffer.Slice.t = private
  { buf : Bigstring.t
  ; pos : int
  ; len : int
  }

let of_pipe info reader =
  Unix.pipe info
  >>| fun (`Reader rd, `Writer wr) ->
  let input_channel = create rd in
  let output_channel = Output_channel.create wr in
  don't_wait_for
    (let%bind () = Output_channel.write_from_pipe output_channel reader in
     Output_channel.close output_channel);
  input_channel
;;
