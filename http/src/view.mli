type t =
  { buf : string
  ; pos : int
  ; len : int
  }

val create : string -> pos:int -> len:int -> t
