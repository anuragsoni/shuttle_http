open! Core
open! Async

module Stream = struct
  type t =
    { encoding : [ `Chunked | `Fixed of int ]
    ; reader : string Pipe.Reader.t
    ; mutable read_started : bool
    }
  [@@deriving sexp_of]

  let iter t ~f =
    if t.read_started then raise_s [%message "Only one consumer can read from a stream"];
    t.read_started <- true;
    Pipe.iter t.reader ~f
  ;;
end

type t =
  | Empty
  | Fixed of string
  | Stream of Stream.t
[@@deriving sexp_of]

let writer t writer =
  match t with
  | Empty -> Output_channel.flush writer
  | Fixed x ->
    Output_channel.write writer x;
    Output_channel.flush writer
  | Stream stream ->
    (match stream.encoding with
     | `Fixed _ ->
       Stream.iter stream ~f:(fun v ->
         Output_channel.write writer v;
         Output_channel.flush writer)
     | `Chunked ->
       let%bind () =
         Stream.iter stream ~f:(fun v ->
           if String.is_empty v
           then Deferred.unit
           else (
             Output_channel.writef writer "%x\r\n" (String.length v);
             Output_channel.write writer v;
             Output_channel.write writer "\r\n";
             Output_channel.flush writer))
       in
       Output_channel.write writer "0\r\n\r\n";
       Output_channel.flush writer)
;;
