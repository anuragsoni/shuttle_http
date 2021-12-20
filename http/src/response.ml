type t =
  { version : Version.t
  ; status : Status.t
  ; reason_phrase : string
  ; headers : Headers.t
  }

let sexp_of_t { version; status; reason_phrase; headers } =
  let open Sexplib0 in
  let open Sexp_conv in
  Sexp.(
    List
      [ List [ Atom "version"; Version.sexp_of_t version ]
      ; List [ Atom "status"; Status.sexp_of_t status ]
      ; List [ Atom "reason_phrase"; sexp_of_string reason_phrase ]
      ; List [ Atom "headers"; Headers.sexp_of_t headers ]
      ])
;;

let create ?(version = Version.v1_1) ?reason_phrase ?(headers = Headers.empty) status =
  let reason_phrase =
    Option.value reason_phrase ~default:(Status.to_reason_phrase status)
  in
  { version; status; reason_phrase; headers }
;;

let version t = t.version
let status t = t.status
let reason_phrase t = t.reason_phrase
let headers t = t.headers
