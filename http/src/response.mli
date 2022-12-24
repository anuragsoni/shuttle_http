type t [@@deriving sexp]

val create
  :  ?version:Version.t
  -> ?reason_phrase:string
  -> ?headers:Headers.t
  -> Status.t
  -> t

val version : t -> Version.t
val status : t -> Status.t
val reason_phrase : t -> string
val headers : t -> Headers.t
