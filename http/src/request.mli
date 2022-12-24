type t [@@deriving sexp_of]

val create
  :  ?version:Version.t
  -> ?headers:Headers.t
  -> ?body:Body.t
  -> Meth.t
  -> string
  -> t

val meth : t -> Meth.t
val path : t -> string
val version : t -> Version.t
val headers : t -> Headers.t
