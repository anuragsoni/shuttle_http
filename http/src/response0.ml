open Core
open Async

type body =
  | Response of Body.t
  | Upgrade of (Upgrade_context.t -> unit Deferred.t)
[@@deriving sexp_of]

type t =
  { version : Version.t
  ; status : Status.t
  ; reason_phrase : string
  ; headers : Headers.t
  ; body : body
  }
[@@deriving sexp_of]

let with_body t body =
  match t.body with
  | Response existing_body ->
    if phys_equal existing_body body then t else { t with body = Response body }
  | Upgrade _ -> raise_s [%message "Attempting to set a body for an upgrade response"]
;;
