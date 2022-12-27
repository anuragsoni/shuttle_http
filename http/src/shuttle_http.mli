open! Core
open! Async
module Headers = Headers
module Meth = Meth

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
      streaming body is closed, or EOF is reached. *)
  val of_pipe : [ `Chunked | `Fixed of int ] -> string Pipe.Reader.t -> t

  (** [to_stream] converts a HTTP body to a stream. *)
  val to_stream : t -> (module Stream_intf.S)

  (** [stream_of_pipe] is a convenience function that creates a stream from a user
      provided [Async_kernel.Pipe.Reader.t]. The pipe will be closed whenever the
      streaming body is closed, or EOF is reached. *)
  val stream_of_pipe
    :  [ `Chunked | `Fixed of int ]
    -> string Pipe.Reader.t
    -> (module Stream_intf.S)
end

module Request : sig
  (** [t] Represents a HTTP 1.1 request. *)
  type t [@@deriving sexp_of]

  val create
    :  ?version:Version.t
    -> ?headers:Headers.t
    -> ?body:Body.t
    -> Meth.t
    -> string
    -> t

  (** [meth] returns the HTTP method of this request. *)
  val meth : t -> Meth.t

  (** [path] returns the path component and query parameters of the request URI *)
  val path : t -> string

  (** [version] returns the HTTP version number for the request. *)
  val version : t -> Version.t

  (** [headers] returns HTTP headers of this request. *)
  val headers : t -> Headers.t

  (** [body] returns the body payload of this request. *)
  val body : t -> Body.t

  (** [set_body] stores a body payload within the request. *)
  val set_body : t -> Body.t -> unit
end

module Response : sig
  (** [t] Represents a HTTP 1.1 response. *)
  type t [@@deriving sexp_of]

  val create
    :  ?version:Version.t
    -> ?reason_phrase:string
    -> ?headers:Headers.t
    -> ?body:Body.t
    -> Status.t
    -> t

  (** [version] returns the HTTP version number for the response. *)
  val version : t -> Version.t

  (** [status] returns the Status code for this response. *)
  val status : t -> Status.t

  (** [reason_phrase] returns the status reason phrase for the response. *)
  val reason_phrase : t -> string

  (** [headers] returns the HTTP headers for this response. *)
  val headers : t -> Headers.t

  (** [body] returns the body payload of this response. *)
  val body : t -> Body.t
end

module Server = Server
module Status = Status
module Version = Version
module Parser = Parser
module Stream_intf = Stream_intf
