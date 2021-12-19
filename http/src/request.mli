type t [@@deriving sexp]

val create : ?version:Version.t -> ?headers:Headers.t -> Meth.t -> string -> t
val version : t -> Version.t
val headers : t -> Headers.t
val meth : t -> Meth.t
val path : t -> string
