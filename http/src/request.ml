type t =
  { meth : Meth.t
  ; path : string
  ; headers : Headers.t
  ; version : Version.t
  }

let sexp_of_t { meth; path; headers; version } =
  let open Sexplib0 in
  let open Sexp_conv in
  Sexp.(
    List
      [ List [ Atom "method"; Meth.sexp_of_t meth ]
      ; List [ Atom "path"; sexp_of_string path ]
      ; List [ Atom "headers"; Headers.sexp_of_t headers ]
      ; List [ Atom "version"; Version.sexp_of_t version ]
      ])
;;

let create ?(version = Version.v1_1) ?(headers = Headers.empty) meth path =
  { meth; path; version; headers }
;;

let meth t = t.meth
let path t = t.path
let version t = t.version
let headers t = t.headers
