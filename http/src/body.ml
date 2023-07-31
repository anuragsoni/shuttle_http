open! Core
open! Async

module Stream = struct
  include Body0.Stream

  let of_pipe encoding reader = { encoding; reader; read_started = false }
  let close t = Pipe.close_read t.reader
  let encoding t = t.encoding

  let iter_without_pushback t ~f =
    if t.read_started then raise_s [%message "Only one consumer can read from a stream"];
    t.read_started <- true;
    Pipe.iter_without_pushback t.reader ~f
  ;;

  let fold t ~init ~f =
    if t.read_started then raise_s [%message "Only one consumer can read from a stream"];
    t.read_started <- true;
    Pipe.fold t.reader ~init ~f
  ;;

  let fold_without_pushback t ~init ~f =
    if t.read_started then raise_s [%message "Only one consumer can read from a stream"];
    t.read_started <- true;
    Pipe.fold_without_pushback t.reader ~init ~f
  ;;

  let read_started t = t.read_started

  let drain t =
    if t.read_started
    then raise_s [%message "Cannot drain a body that's currently being read"];
    Pipe.drain t.reader
  ;;

  let to_string t =
    if t.read_started
    then raise_s [%message "to_string: Only one consumer can read from a stream"];
    t.read_started <- true;
    let%map rope =
      Pipe.fold_without_pushback t.reader ~init:Rope.empty ~f:(fun rope str ->
        Rope.(rope ^ of_string str))
    in
    Rope.to_string rope
  ;;

  let closed t = Pipe.closed t.reader
end

type t = Body0.t =
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
;;

let to_string = function
  | Empty -> return ""
  | Fixed s -> return s
  | Stream x -> Stream.to_string x
;;
