type t

val create : ?version:Version.t -> ?headers:Headers.t -> Status.t -> t
val version : t -> Version.t
val headers : t -> Headers.t
val status : t -> Status.t
