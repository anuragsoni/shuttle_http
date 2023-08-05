open! Core
open! Async

type t =
  { ssl : Async_ssl.Ssl.Connection.t option
  ; unconsumed_data : string option
  ; fd : Fd.t
  ; parent_connection_closed : unit Deferred.t
  }
[@@deriving sexp_of]

val fd : t -> Fd.t
val unconsumed_data : t -> string option
val ssl : t -> Async_ssl.Ssl.Connection.t option
val parent_connection_closed : t -> unit Deferred.t
