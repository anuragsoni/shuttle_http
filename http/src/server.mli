open! Core
open! Async
open! Shuttle

type body
type response = Http.Response.t * body

val run_server_loop
  :  (body:string Pipe.Reader.t -> Http.Request.t -> response Deferred.t)
  -> Input_channel.t
  -> Output_channel.t
  -> unit Deferred.t

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
