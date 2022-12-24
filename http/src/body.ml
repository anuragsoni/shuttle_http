open! Core
open! Async

type t =
  | Empty
  | Fixed of string
  | Stream of ((module Stream_intf.S)[@sexp.opaque])
[@@deriving sexp_of]

let string x = Fixed x
let empty = Empty
let stream s = Stream s

let stream_of_pipe encoding reader =
  let module M = struct
    type t =
      { reader : string Pipe.Reader.t
      ; mutable read_started : bool
      ; encoding : [ `Fixed of int | `Chunked ]
      }

    let t = { reader; read_started = false; encoding }
    let encoding () = t.encoding
    let closed () = Pipe.closed t.reader
    let read_started () = t.read_started
    let read () = Pipe.read t.reader
    let close () = Pipe.close_read t.reader
  end
  in
  (module M : Stream_intf.S)
;;

let of_pipe encoding reader = Stream (stream_of_pipe encoding reader)

let to_stream = function
  | Empty -> stream_of_pipe (`Fixed 0) (Pipe.empty ())
  | Fixed x -> stream_of_pipe (`Fixed (String.length x)) (Pipe.singleton x)
  | Stream x -> x
;;
