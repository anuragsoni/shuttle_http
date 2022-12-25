open! Core
open! Async
open! Shuttle

type error_handler = ?exn:exn -> ?request:Request.t -> Status.t -> Response.t Deferred.t
type handler = Request.t -> Response.t Deferred.t
type t [@@deriving sexp_of]

val closed : t -> unit Deferred.t
val create : ?error_handler:error_handler -> Input_channel.t -> Output_channel.t -> t
val run : t -> (Request.t -> Response.t Deferred.t) -> unit Deferred.t

val respond_string
  :  t
  -> ?reason_phrase:string
  -> ?headers:Headers.t
  -> ?status:Status.t
  -> string
  -> Response.t

val respond_empty
  :  t
  -> ?reason_phrase:string
  -> ?headers:Headers.t
  -> Status.t
  -> Response.t

val respond_stream
  :  t
  -> ?reason_phrase:string
  -> ?headers:Headers.t
  -> ?status:Status.t
  -> (module Stream_intf.S)
  -> Response.t
