open Core

type t =
  { buf : Bigstring.t
  ; pos : int
  ; len : int
  }
[@@deriving sexp_of]
