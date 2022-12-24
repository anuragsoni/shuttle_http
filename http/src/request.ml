open Core

type t =
  { meth : Meth.t
  ; path : string
  ; version : Version.t
  ; headers : Headers.t
  }
[@@deriving sexp]

let create ?(version = Version.Http_1_1) ?(headers = Headers.empty) meth path =
  { meth; path; version; headers }
;;

let meth t = t.meth
let path t = t.path
let version t = t.version
let headers t = t.headers
