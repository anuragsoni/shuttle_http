open! Core
open! Async
open Shuttle

let run filename =
  Input_channel.with_file ~buf_len:0x4000 filename ~f:(fun chan ->
      let pipe_r = Input_channel.pipe chan in
      let wr = Output_channel.create ~buf_len:0x4000 (Fd.stdout ()) in
      let pipe_w = Output_channel.pipe wr in
      let%bind () = Pipe.transfer_id pipe_r pipe_w in
      Output_channel.flushed wr)
;;

let command =
  Command.async
    ~summary:"cat"
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: Filename.arg_type) in
      fun () -> run filename)
;;

let () = Command.run command
