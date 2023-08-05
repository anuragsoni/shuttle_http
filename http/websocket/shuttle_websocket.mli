open! Core
open! Async

module Config : sig
  type t

  val create
    :  ?buffer_size:int
    -> ?error_handler:Shuttle_http.Server.error_handler
    -> ?response_headers:(string * string) list
    -> ?opcode:[ `Binary | `Text ]
    -> ?subprotocol:(string list -> string option)
    -> unit
    -> t

  val default : t
end

val create
  :  ?config:Config.t
  -> (Websocket.t -> unit Deferred.t)
  -> Shuttle_http.Request.t
  -> Shuttle_http.Response.t Deferred.t
