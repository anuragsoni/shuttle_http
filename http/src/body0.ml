open! Core
open! Async

module Stream = struct
  type t =
    { encoding : [ `Chunked | `Fixed of int ]
    ; reader : string Pipe.Reader.t
    ; mutable read_started : bool
    }
  [@@deriving sexp_of]
end

type t =
  | Empty
  | Fixed of string
  | Stream of Stream.t
[@@deriving sexp_of]
