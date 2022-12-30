open Core

type t =
  { meth : Meth.t
  ; path : string
  ; version : Version.t
  ; headers : Headers.t
  ; context : (Univ_map.t[@sexp.opaque])
  ; body : Body.t
  }
[@@deriving sexp_of]

let create
  ?(version = Version.Http_1_1)
  ?(headers = Headers.empty)
  ?(body = Body.empty)
  meth
  path
  =
  { meth; path; version; headers; context = Univ_map.empty; body }
;;

let meth t = t.meth
let path t = t.path
let version t = t.version
let headers t = t.headers
let body t = t.body
let with_body t body = if phys_equal t.body body then t else { t with body }

let with_headers t headers =
  if phys_equal t.headers headers then t else { t with headers }
;;
