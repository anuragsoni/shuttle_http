open! Core

type t =
  | Empty
  | Fixed of string
[@@deriving sexp_of]

let string x = Fixed x
let empty = Empty

