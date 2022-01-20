(** Low level HTTP 1.1 server implementation for OCaml. *)

open! Core
open! Async
open! Shuttle

(** [body] represents the response body sent to the client. [respond_*] functions can be
    used to construct a body from OCaml strings/streams. *)
type body

type response = Http.Response.t * body

(** [run_server_loop] accepts a HTTP handler, and returns a callback that can be used to
    drive the server loop created via [Shuttle.Connection.listen]. This allows the user to
    customize the [Input_channel] and [Output_channel] and have control over the various
    Server configuration options like [accept_n], [backlog] and more. *)
val run_server_loop
  :  (body:string Pipe.Reader.t -> Http.Request.t -> response Deferred.t)
  -> Input_channel.t
  -> Output_channel.t
  -> unit Deferred.t

(** [respond_string] creates a new HTTP response with a fixed length body created from the
    string input. Users should ensure that the header collection contains the
    "content-length" header with the length of the string that should be sent as the
    response. *)
val respond_string
  :  ?headers:Http.Header.t
  -> ?status:Http.Status.t
  -> string
  -> response Deferred.t

(** [respond_stream] creates a new streaming HTTP response from an async pipe. Users
    should ensure that the header collection contains the "transfer-encoding" header with
    the value "chunked". *)
val respond_stream
  :  ?headers:Http.Header.t
  -> ?status:Http.Status.t
  -> string Pipe.Reader.t
  -> response Deferred.t
