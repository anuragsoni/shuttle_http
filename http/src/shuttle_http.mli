open! Core
open! Async
module Headers = Headers
module Meth = Meth
module Request = Request
module Response = Response
module Server = Server
module Status = Status
module Version = Version
module Parser = Parser
module Stream_intf = Stream_intf

module Body : sig
  type t [@@deriving sexp_of]

  (** [string str] creates a fixed length encoded body from a user provided string. *)
  val string : string -> t

  (** [empty] is a zero length body. *)
  val empty : t

  (** [stream] creates a streaming body from a user provided streaming module. *)
  val stream : (module Stream_intf.S) -> t

  (** [of_pipe] is a convenience function that creates a streaming body from a user
      provided [Async_kernel.Pipe.Reader.t]. The pipe will be closed whenever the
      streaming body is closed, or EOF is reached.. *)
  val of_pipe : [ `Chunked | `Fixed of int ] -> string Pipe.Reader.t -> t

  (** [to_stream] converts a HTTP body to a stream. *)
  val to_stream : t -> (module Stream_intf.S)
end
