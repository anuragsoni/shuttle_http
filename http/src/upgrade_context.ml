open! Core
open! Async

type t =
  { ssl : Async_ssl.Ssl.Connection.t option
  ; unconsumed_data : string option
  ; fd : Fd.t
  ; parent_connection_closed : unit Deferred.t
  }
[@@deriving sexp_of]

let fd t = t.fd
let unconsumed_data t = t.unconsumed_data
let ssl t = t.ssl
let parent_connection_closed t = t.parent_connection_closed
