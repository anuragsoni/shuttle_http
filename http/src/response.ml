open Core

type t =
  { version : Version.t
  ; status : Status.t
  ; reason_phrase : string
  ; headers : Headers.t
  }
[@@deriving sexp]

let create ?(version = Version.Http_1_1) ?reason_phrase ?(headers = Headers.empty) status =
  let reason_phrase = Option.value reason_phrase ~default:(Status.to_string status) in
  { version; status; reason_phrase; headers }
;;

let version t = t.version
let status t = t.status
let reason_phrase t = t.reason_phrase
let headers t = t.headers
