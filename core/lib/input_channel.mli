(** Alternative to
    {{:https://github.com/janestreet/async_unix/blob/cdd9aba67eec2f30bb3a7a22f92c056742073726/src/reader.mli}
    Async_unix.Reader}, based on the low latency transport in async_rpc. *)

open! Core
open! Async_kernel
open Async_unix

type t [@@deriving sexp_of]

val create : ?max_buffer_size:int -> ?buf_len:int -> Fd.t -> t
val is_closed : t -> bool
val closed : t -> unit Deferred.t
val close : t -> unit Deferred.t
val refill : t -> [ `Ok | `Eof ] Deferred.t
val view : t -> Bigstring.t Core_unix.IOVec.t
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

val of_pipe : Info.t -> string Pipe.Reader.t -> t Deferred.t
val read_line : t -> [ `Ok of string | `Eof ] Deferred.t
val lines : t -> string Pipe.Reader.t
val read : t -> int -> [ `Ok of string | `Eof ] Deferred.t

(** [open_file ?buf_len filename] opens [filename] and returns an [Input_channel] that can
    be used to read from the file. [buf_len] is an optional input and can be used to
    control the channel's buffer size. *)
val open_file : ?buf_len:int -> Filename.t -> t Deferred.t

(** [with_file ?buf_len filename ~f] opens [filename], creates a new channel and passes it
    to [f]. Once [f] returns, the channel and the underlying file is closed. *)
val with_file : ?buf_len:int -> Filename.t -> f:(t -> 'a Deferred.t) -> 'a Deferred.t
