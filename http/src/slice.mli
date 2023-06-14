open! Core

type t =
  { buf : (bytes[@sexp.opaque])
  ; pos : int
  ; len : int
  }
[@@deriving sexp_of]
