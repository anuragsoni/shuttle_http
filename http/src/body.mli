open! Core
open! Async

module Stream : sig
  type t [@@deriving sexp_of]

  (** [of_pipe] is a convenience function that creates a stream from a user provided
      [Async_kernel.Pipe.Reader.t]. The pipe will be closed whenever the streaming body is
      closed, or EOF is reached. *)
  val of_pipe : [ `Chunked | `Fixed of int ] -> string Pipe.Reader.t -> t

  val close : t -> unit
  val encoding : t -> [ `Chunked | `Fixed of int ]
  val iter : t -> f:(string -> unit Deferred.t) -> unit Deferred.t
  val drain : t -> unit Deferred.t
  val closed : t -> unit Deferred.t
  val read_started : t -> bool
end

type t = private
  | Empty
  | Fixed of string
  | Stream of Stream.t
[@@deriving sexp_of]

(** [string str] creates a fixed length encoded body from a user provided string. *)
val string : string -> t

(** [empty] is a zero length body. *)
val empty : t

(** [of_pipe] is a convenience function that creates a streaming body from a user provided
    [Async_kernel.Pipe.Reader.t]. The pipe will be closed whenever the streaming body is
    closed, or EOF is reached. *)
val of_pipe : [ `Chunked | `Fixed of int ] -> string Pipe.Reader.t -> t

(** [stream] creates a streaming body from a user provided streaming module. *)
val stream : Stream.t -> t

(** [to_stream] converts a HTTP body to a stream. *)
val to_stream : t -> Stream.t
