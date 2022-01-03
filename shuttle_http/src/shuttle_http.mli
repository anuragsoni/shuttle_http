open Async
open Shuttle

module Connection : sig
  type 'a t
  type response

  val user_context : 'a t -> 'a
  val request : 'a t -> Cohttp.Request.t
  val request_body : 'a t -> string Pipe.Reader.t
  val respond_with_string : 'a t -> Cohttp.Response.t -> string -> response Deferred.t

  val respond_with_stream
    :  'a t
    -> Cohttp.Response.t
    -> string Pipe.Reader.t
    -> response Deferred.t
end

val run
  :  'a
  -> Input_channel.t
  -> Output_channel.t
  -> ('a Connection.t -> Connection.response Deferred.t)
  -> (?request:Cohttp.Request.t
      -> Cohttp.Code.status_code
      -> (Cohttp.Response.t * string) Deferred.t)
  -> unit Deferred.t
