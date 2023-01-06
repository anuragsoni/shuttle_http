open! Core
open! Async
open! Shuttle

let close reader writer =
  let%bind () = Output_channel.close writer in
  Input_channel.close reader
;;
