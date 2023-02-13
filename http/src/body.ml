open! Core
open! Async

module Stream = struct
  type t =
    { encoding : [ `Chunked | `Fixed of int ]
    ; reader : string Pipe.Reader.t
    ; mutable read_started : bool
    }
  [@@deriving sexp_of]

  let of_pipe encoding reader = { encoding; reader; read_started = false }
  let close t = Pipe.close_read t.reader
  let encoding t = t.encoding

  let iter t ~f =
    if t.read_started then raise_s [%message "Only one consumer can read from a stream"];
    t.read_started <- true;
    Pipe.iter t.reader ~f

  let fold t ~init ~f =
    if t.read_started then raise_s [%message "Only one consumer can read from a stream"];
    t.read_started <- true;
    Pipe.fold t.reader ~init ~f

  let read_started t = t.read_started

  let drain t =
    if t.read_started
    then raise_s [%message "Cannot drain a body that's currently being read"];
    Pipe.drain t.reader

  let closed t = Pipe.closed t.reader
end

type t =
  | Empty
  | Fixed of string
  | Stream of Stream.t
[@@deriving sexp_of]

let string x = Fixed x
let empty = Empty
let of_pipe encoding reader = Stream { Stream.encoding; reader; read_started = false }
let stream stream = Stream stream

let to_stream = function
  | Empty -> Stream.of_pipe (`Fixed 0) (Pipe.empty ())
  | Fixed x -> Stream.of_pipe (`Fixed (String.length x)) (Pipe.singleton x)
  | Stream x -> x
