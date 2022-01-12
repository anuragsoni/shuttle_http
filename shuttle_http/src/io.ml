open! Core
open! Async
open! Shuttle

module IO = struct
  type ic = Input_channel.t
  type oc = Output_channel.t
  type 'a t = 'a Deferred.t

  let ( >>= ) = Deferred.( >>= )
  let return x = return x

  type conn = unit

  let read_line ic =
    Input_channel.read_line ic
    >>| function
    | `Eof -> None
    | `Ok v -> Some v
  ;;

  let write oc buf =
    Output_channel.write oc buf;
    Deferred.unit
  ;;

  let refill ic = Input_channel.refill ic
  let flush oc = Output_channel.flush oc

  let with_input_buffer ic ~f =
    let view = Input_channel.view ic in
    let res, consumed =
      f
        (Input_channel.View.buf view)
        ~pos:(Input_channel.View.pos view)
        ~len:(Input_channel.View.length view)
    in
    Input_channel.View.consume view consumed;
    res
  ;;

  let read ic len =
    Input_channel.read ic len
    >>| function
    | `Eof -> ""
    | `Ok v -> v
  ;;
end

include IO
module Request = Cohttp.Request.Private.Make (IO)
module Response = Cohttp.Response.Private.Make (IO)
