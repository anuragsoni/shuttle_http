open! Core
open! Async

(** [S] represents streaming HTTP bodies. This module signature is intended to pair with
    [Body.t] to allow users to create streaming request and response bodies while giving
    them full control over the internal representation of managing the stream object.
    [Body.t] provides a convenience function that allows one to create a stream matching
    this signature from an [Async_kernel.Pipe.Reader.t]. *)
module type S = sig
  (** [encoding] informs whether the body needs to be chunk encoded or not. For servers
      this function is used to automatically populate the transfer-encoding or
      content-length headers. *)
  val encoding : unit -> [ `Fixed of int | `Chunked ]

  (** [close] allows for closing/tearing-down any resources that are used to produce the
      content for a stream. For servers, this function will be called if the underlying
      client socket connection is closed, or when the body is fully drained. *)
  val close : unit -> unit

  (** [read] surfaces new data to the reader of the stream. *)
  val read : unit -> [ `Ok of string | `Eof ] Deferred.t

  (** [drain] should consume items one at a time from the stream and discard them. *)
  val drain : unit -> unit Deferred.t

  (** [closed] is a deferred that should be resolved when the stream is closed/drained. *)
  val closed : unit -> unit Deferred.t

  (** [read_started] indicated whether a user started to consume a stream or not. Creators
      of the stream should set this value to true when a consumer starts reading the
      stream. Servers will use [read_started] to verify if they should drain before
      starting the next cycle of the server loop, or if they should wait for the body to
      be closed by the user. *)
  val read_started : unit -> bool

  (** [start_read] should be used by consumers of the stream that the body is going to be
      consumed. This will inform that server not to attempt draining the body. *)
  val start_read : unit -> unit
end
