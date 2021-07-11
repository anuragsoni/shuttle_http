open! Core
open! Async

module Read_chunk_result : sig
  type 'a t =
    | Stop of 'a
    | Continue
    | Consumed of int
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val create : ?buf_len:int -> Fd.t -> t
val is_closed : t -> bool
val close : t -> unit Deferred.t

val read_one_chunk_at_a_time
  :  t
  -> on_chunk:(Bigstring.t -> pos:int -> len:int -> 'a Read_chunk_result.t)
  -> ('a, [> `Eof | `Closed ]) Deferred.Result.t
