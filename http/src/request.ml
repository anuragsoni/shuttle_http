open Sexplib0.Sexp_conv

type t =
  { meth : Meth.t
  ; path : string
  ; headers : Headers.t
  ; version : Version.t
  }
[@@deriving sexp]

let create ?(version = Version.v1_1) ?(headers = Headers.empty) meth path =
  { meth; path; version; headers }
;;

let meth t = t.meth
let path t = t.path
let version t = t.version
let headers t = t.headers
