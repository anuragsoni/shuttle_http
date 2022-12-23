open! Core
open! Async_kernel
open Async_unix
module Logger : Log.Global_intf

type t [@@deriving sexp_of]

(** [create ?max_buffer_size ?buf_len ?write_timeout fd] creates a new writer.

    The writer doesn't flush automatically and the user is responsible for calling
    [flush], which triggers a write system call if needed. *)
val create
  :  ?max_buffer_size:int
  -> ?buf_len:int
  -> ?write_timeout:Time_ns.Span.t
  -> ?time_source:[> read ] Time_source.T1.t
  -> Fd.t
  -> t

(** [monitor] returns the async monitor used by [Output_channel] for performing all write
    operations.*)
val monitor : t -> Monitor.t

(** [remote_closed] is a deferred that's resolved when the consumer that's reading the
    bytes written to the Output_channel is closed, i.e. the channel has received an EPIPE
    or ECONNRESET when it attempts to perform a write. *)
val remote_closed : t -> unit Deferred.t

val is_closed : t -> bool
val is_open : t -> bool
val close_started : t -> unit Deferred.t
val close_finished : t -> unit Deferred.t

(** [write_bigstring] copies the bigstring into the channel's internal buffer. It is safe
    to modify the bigstring once [write_bigstring] returns. *)
val write_bigstring : t -> ?pos:int -> ?len:int -> Bigstring.t -> unit

val schedule_bigstring : t -> ?pos:int -> ?len:int -> Bigstring.t -> unit
  [@@deprecated
    "schedule_bigstring will be removed in a future release. Use [write_bigstring] \
     instead."]

(** [write] copies the string into the channel's internal buffer. The string will surface
    the next time the writer schedules a write. *)
val write : t -> ?pos:int -> ?len:int -> string -> unit

val write_string : t -> ?pos:int -> ?len:int -> string -> unit
  [@@deprecated "write_string will be removed in a future release. Use [write] instead. "]

val write_char : t -> char -> unit
val writef : t -> ('a, unit, string, unit) format4 -> 'a

(** [close] will close the underlying file descriptor after waiting for the writer to be
    flushed. *)
val close : t -> unit Deferred.t

(** [schedule_flush] will schedule a write system call if one is needed. *)
val schedule_flush : t -> unit

module Flush_result : sig
  (** [t] Result of a flush operation.

      - [Flushed] indicates all prior writes at the time [flush] was call have finished
        without any errors.

      - [Remote_closed] indicates that the consumer that's reading the bytes written to
        the Output_channel is closed, i.e. the channel has received an EPIPE or ECONNRESET
        when it attempts to perform a write.

      - [Error] indicates that the write operation was interrupted by an unhandled
        exception, or a timeout. *)
  type t =
    | Flushed
    | Remote_closed
    | Error
  [@@deriving sexp_of]
end

(** [flushed_or_fail t] returns a Deferred that is resolved when all previous writes
    complete, or if any of the write operations fail. *)
val flushed_or_fail : t -> Flush_result.t Deferred.t

(** [flushed t] returns a deferred that will get resolved when all previous writes have
    finished. Unlike [flushed_or_fail] if a write call fails then the deferred will never
    be resolved. *)
val flushed : t -> unit Deferred.t

(** [flush] schedules a write system call if one is needed and returns a deferred that is
    resolved when all prior writes have finished. If a write call fails then the deferred
    will never be resolved. *)
val flush : t -> unit Deferred.t

(** [flush_or_fail] schedules a write system call if one is needed and returns a deferred
    that is resolved when all previous writes complete, or if any of the write operations
    fail. *)
val flush_or_fail : t -> Flush_result.t Deferred.t

val pipe : t -> string Pipe.Writer.t
val of_pipe : Info.t -> string Pipe.Writer.t -> (t * unit Deferred.t) Deferred.t
