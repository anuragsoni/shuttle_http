open Core
open Shuttle

let enqueue_iovecs channel iovecs =
  if Output_channel.is_closed channel
  then `Closed
  else
    `Ok
      (List.fold iovecs ~init:0 ~f:(fun acc { Faraday.buffer; off; len } ->
           Output_channel.write_bigstring channel buffer ~pos:off ~len;
           acc + len))
;;
