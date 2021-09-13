(** Alternative to
    {{:https://github.com/janestreet/async_unix/blob/cdd9aba67eec2f30bb3a7a22f92c056742073726/src/reader.mli}
    Async_unix.Reader}, based on the low latency transport in async_rpc. *)

open! Core
open! Async_kernel
open Async_unix

type 'a handle_chunk_result =
  [ `Stop of 'a
    (** [Stop a] indicates that the read loop's handler consumed 0 bytes and that the read
        loop should stop. *)
  | `Continue
    (** [Continue] indicates that the read loop's handler consumed some bytes, and would
        like to keep reading. *)
  | `Wait of unit Deferred.t
    (** [Wait promise] should be used when the user's handler needs to process the
        reader's buffer asynchronously. This will ensure that future calls to the handler
        will only be made after the deferred returned by the user is resolved. *)
  ]
[@@deriving sexp_of]

type t [@@deriving sexp_of]

val create : ?buf_len:int -> Fd.t -> t
val is_closed : t -> bool
val close : t -> unit Deferred.t

(** [read_one_chunk_at_a_time ~on_chunk] reads bytes into the reader's internal buffer,
    and calls [on_chunk] whenever there is data available. *)
val read_one_chunk_at_a_time
  :  t
  -> on_chunk:(Bytebuffer.t -> 'a handle_chunk_result)
  -> [ `Stopped of 'a | `Eof | `Eof_with_unconsumed of Bigstring.t ] Deferred.t

(** [drain t] reads chunks of data from the reader and discards them. *)
val drain : t -> unit Deferred.t

(** [pipe] returns a reader pipe that contains the results of reading chunks from an
    input_channel. *)
val pipe : t -> string Pipe.Reader.t