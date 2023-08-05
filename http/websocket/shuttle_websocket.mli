open! Core
open! Async

module Config : sig
  (* Configuration for how to negotiate a websocket connection.

     - [buffer_size] controls the initial buffer size for the underlying reader/writer
     pair that are handed off to async_websocket. The default is 16_000 bytes.

     - [error_handler] is a user provided handler that will be called if the websocket
     function encounters an invalid upgrade request.

     - [response_headers] any headers that should be sent with the successful
     Switching-Protocol response sent to the client.

     - [opcode] specifies whether to use Text of Binary frames on the websocket
     connection.

     - [subprotocol] If a client sends a list of subprotocols this function is called to
     check which subprotocol is selected by the server. Reply with None to indicate a null
     subprotocol. *)
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

(** [create] accepts a user provided websocket-handler and performs an HTTP/1.1
    protocol upgrade. [create] returns a {{!Shuttle_http.Response.t} http response}
    that either represents an error indicating a bad upgrade request,
    or contains a `Switching-Protocol` response with an HTTP protocol upgrade handler.
    If the protocol negotiation succeeds the user provided websocket handler
    will be called with a websocket descriptor. Once the deferred returned by the websocket-handler is resolved,
    or if there are any unhandled exceptions in the handler the underlying TCP connection is closed. *)
val create
  :  ?config:Config.t
  -> (Websocket.t -> unit Deferred.t)
  -> Shuttle_http.Request.t
  -> Shuttle_http.Response.t Deferred.t
