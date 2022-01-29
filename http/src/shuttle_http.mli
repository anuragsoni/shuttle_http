open! Core
open! Async
open! Shuttle

(** Bodies for HTTP requests and responses, with support for streaming. *)
module Body : sig
  (** Streaming body reader. *)
  module Reader : sig
    type t [@@deriving sexp_of]

    (** [encoding] returns whether the body is a chunk encoded payload, or a fixed length
        payload. *)
    val encoding : t -> Http.Transfer.encoding

    (** [pipe] creates an async pipe and returns its reader end. This can be used for
        consuming a request body using the full API set provided by [Async_kernel.Pipe]. *)
    val pipe : t -> string Pipe.Reader.t

    (** [drain] will read chunks of the HTTP body and discard them. *)
    val drain : t -> unit Deferred.t
  end

  (** Body writer with support for streaming. *)
  module Writer : sig
    type t [@@deriving sexp_of]

    (** [encoding] returns whether the body is a chunk encoded payload, or a fixed length
        payload. *)
    val encoding : t -> Http.Transfer.encoding

    (** [empty] represents a fixed length encoded body of length 0. *)
    val empty : t

    (** [string] creates a fixed length body from the input string. *)
    val string : string -> t

    (** [stream] creates a streaming body writer from the given pipe. Default value of
        [?encoding] is "chunked". The body writer ensures that the payloads will be chunk
        encoded when using an encoding value of chunked. *)
    val stream : ?encoding:Http.Transfer.encoding -> string Pipe.Reader.t -> t
  end
end

(** Low level HTTP 1.1 server implementation for OCaml. *)
module Server : sig
  type response = Http.Response.t * Body.Writer.t

  (** [run_server_loop] accepts a HTTP handler, and returns a callback that can be used to
      drive the server loop created via [Shuttle.Connection.listen]. This allows the user
      to customize the [Input_channel] and [Output_channel] and have control over the
      various Server configuration options like [accept_n], [backlog] and more. *)
  val run_server_loop
    :  (Http.Request.t -> Body.Reader.t -> response Deferred.t)
    -> Input_channel.t
    -> Output_channel.t
    -> unit Deferred.t
end

module Private : sig
  module Parser = Parser
end
