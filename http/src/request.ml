open Core

type t =
  { meth : Meth.t
  ; path : string
  ; version : Version.t
  ; headers : Headers.t
  ; mutable context : (Univ_map.t[@sexp.opaque])
  ; mutable body : Body.t
  }
[@@deriving sexp_of]

let create
  ?(version = Version.Http_1_1)
  ?(headers = Headers.empty)
  ?(body = Body.Empty)
  meth
  path
  =
  { meth; path; version; headers; context = Univ_map.empty; body }
;;

let meth t = t.meth
let path t = t.path
let version t = t.version
let headers t = t.headers
