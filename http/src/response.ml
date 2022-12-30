open Core

type t =
  { version : Version.t
  ; status : Status.t
  ; reason_phrase : string
  ; mutable headers : Headers.t
  ; mutable body : Body.t
  }
[@@deriving sexp_of]

let create
  ?(version = Version.Http_1_1)
  ?reason_phrase
  ?(headers = Headers.empty)
  ?(body = Body.empty)
  status
  =
  let reason_phrase = Option.value reason_phrase ~default:(Status.to_string status) in
  { version; status; reason_phrase; headers; body }
;;

let version t = t.version
let status t = t.status
let reason_phrase t = t.reason_phrase
let headers t = t.headers
let body t = t.body
let set_headers t headers = t.headers <- headers
