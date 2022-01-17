open! Core
open! Async
open! Shuttle

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
