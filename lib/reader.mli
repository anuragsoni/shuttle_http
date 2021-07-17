(** Alternative to
    {{:https://github.com/janestreet/async_unix/blob/cdd9aba67eec2f30bb3a7a22f92c056742073726/src/reader.mli}
    Async_unix.Reader}, based on the low latency transport in async_rpc. *)

open! Core
open! Async

module Read_chunk_result : sig
  type 'a t =
    | Stop of 'a
        (** [Stop a] indicates that the read loop's handler consumed 0 bytes and that the
            read loop should stop. *)
    | Continue
        (** [Continue] indicates that the read loop's handler consumed all bytes. *)
    | Consumed of int
        (** [Consumed count] indicates that the read loop's handler consumed [count]
            bytes. *)
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val create : ?buf_len:int -> Fd.t -> t
val is_closed : t -> bool
val close : t -> unit Deferred.t

(** [read_one_chunk_at_a_time ~on_chunk] reads bytes into the reader's internal buffer,
    and calls [on_chunk] whenever there is data available. *)
val read_one_chunk_at_a_time
  :  t
  -> on_chunk:(Bigstring.t -> pos:int -> len:int -> 'a Read_chunk_result.t)
  -> ('a, [> `Eof | `Closed ]) Deferred.Result.t
