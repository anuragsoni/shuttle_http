(** Alternative to
    {{:https://github.com/janestreet/async_unix/blob/cdd9aba67eec2f30bb3a7a22f92c056742073726/src/reader.mli}
    Async_unix.Reader}, based on the low latency transport in async_rpc. *)

open! Core
open! Async_kernel
open Async_unix

type 'a handle_chunk_result =
  [ `Stop of 'a
    (** [Stop a] indicates that the read loop's handler consumed 0 bytes and that the read
        loop should stop with the user provided value [a]. *)
  | `Stop_consumed of 'a * int
    (** [Stop_consumed (a, n)] indicates that the read loop's handler consumed [n] bytes,
        and that the read loop should stop with the user provided value [a]. *)
  | `Continue of int
    (** [Continue] indicates that the read loop's handler consumed [n] bytes, and would
        like to keep reading. *)
  ]
[@@deriving sexp_of]

type t [@@deriving sexp_of]

val create : ?buf_len:int -> Fd.t -> t
val is_closed : t -> bool
val closed : t -> unit Deferred.t
val close : t -> unit Deferred.t

(** [read_one_chunk_at_a_time ~on_chunk] reads bytes into the reader's internal buffer,
    and calls [on_chunk] whenever there is data available. *)
val read_one_chunk_at_a_time
  :  t
  -> on_chunk:(Bigstring.t -> pos:int -> len:int -> 'a handle_chunk_result)
  -> [ `Stopped of 'a | `Eof | `Eof_with_unconsumed of string ] Deferred.t

(** [drain t] reads chunks of data from the reader and discards them. *)
val drain : t -> unit Deferred.t

(** [pipe] returns a reader pipe that contains the results of reading chunks from an
    input_channel. *)
val pipe : t -> string Pipe.Reader.t

(** [transfer] will read chunks from an input channel and write them to the provided
    writer end of an async pipe. The deferred returned by the function will be determined
    on EOF or if the writer is closed. Use [transfer] in scenarios where [pipe] is needed,
    but if there is a need to not close the channel automatically once the transfer is
    finished. *)
val transfer : t -> string Pipe.Writer.t -> unit Deferred.t

val of_pipe : Info.t -> string Pipe.Reader.t -> t Deferred.t
