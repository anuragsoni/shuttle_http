open! Core
open! Async_kernel
open Async_unix

type t [@@deriving sexp_of]

type slice = private
  { buf : Bigstring.t
  ; pos : int
  ; len : int
  }

val create : ?buf_len:int -> ?time_source:[> read ] Time_source.T1.t -> Fd.t -> t
val time_source : t -> Time_source.t
val buffer_size : t -> int
val is_closed : t -> bool
val closed : t -> unit Deferred.t
val close : t -> unit Deferred.t
val refill : t -> [> `Ok | `Eof ] Deferred.t

exception Timeout

val refill_with_timeout : t -> Time_ns.Span.t -> [> `Ok | `Eof ] Deferred.t
val view : t -> slice
val consume : t -> int -> unit

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

val of_pipe : ?buf_len:int -> Info.t -> string Pipe.Reader.t -> t Deferred.t
