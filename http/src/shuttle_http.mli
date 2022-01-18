open! Core
open! Async
open! Shuttle

module Body : sig
  module Reader : sig
    type t [@@deriving sexp_of]

    val encoding : t -> Http.Transfer.encoding
    val pipe : t -> string Pipe.Reader.t
    val drain : t -> unit Deferred.t
  end

  module Writer : sig
    type t [@@deriving sexp_of]

    val encoding : t -> Http.Transfer.encoding
    val empty : t
    val string : string -> t
    val stream : string Pipe.Reader.t -> t
  end
end

module Server : sig
  type response = Http.Response.t * Body.Writer.t

  val run_server_loop
    :  (body:Body.Reader.t -> Http.Request.t -> response Deferred.t)
    -> Input_channel.t
    -> Output_channel.t
    -> unit Deferred.t

  val respond
    :  ?headers:Http.Header.t
    -> ?body:Body.Writer.t
    -> Http.Status.t
    -> response Deferred.t

  val respond_string
    :  ?headers:Http.Header.t
    -> ?status:Http.Status.t
    -> string
    -> response Deferred.t

  val respond_stream
    :  ?headers:Http.Header.t
    -> ?status:Http.Status.t
    -> string Pipe.Reader.t
    -> response Deferred.t
end
