(** Alternative to
    {{:https://github.com/janestreet/async_unix/blob/cdd9aba67eec2f30bb3a7a22f92c056742073726/src/writer.mli}
    Async_unix.Writer}, based on the low latency transport in async_rpc. *)

open! Core
open! Async_kernel
open Async_unix

type t [@@deriving sexp_of]

(** [create ?initial_buffer_size ?max_buffer_size ?write_timeout fd] creates a new writer.

    The writer doesn't flush automatically and the user is responsible for calling
    [flush], which triggers a write system call if needed. *)
val create : ?initial_buffer_size:int -> ?write_timeout:Time_ns.Span.t -> Fd.t -> t

val is_closed : t -> bool
val is_open : t -> bool
val close_started : t -> unit Deferred.t
val close_finished : t -> unit Deferred.t

(** [write_bigstring] copies the bigstring into the channel's internal buffer. It is safe
    to modify the bigstring once [write_bigstring] returns. *)
val write_bigstring : t -> ?pos:int -> ?len:int -> Bigstring.t -> unit

(** [write_string] copies the string into the channel's internal buffer. The string will
    surface the next time the writer schedules a write. *)
val write_string : t -> ?pos:int -> ?len:int -> string -> unit

val write_char : t -> char -> unit

(** [close] will close the underlying file descriptor after waiting for the writer to be
    flushed. *)
val close : t -> unit Deferred.t

(** [flush] will schedule a write system call if one is needed. *)
val flush : t -> unit

(** [flushed t f] deferred that will get resolved when all prior writes have finished. *)
val flushed : t -> unit Deferred.t

val pipe : t -> string Pipe.Writer.t
