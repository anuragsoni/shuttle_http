open! Core
open! Async

module Config : sig
  type t [@@deriving sexp_of]

  val create
    :  ?initial_buffer_size:int
    -> ?max_buffer_size:int
    -> ?write_timeout:Time_ns.Span.t
    -> ?buffering_threshold_in_bytes:int
    -> unit
    -> t

  val default : t
end

type t [@@deriving sexp_of]

val create : Async.Fd.t -> Config.t -> t
val is_closed : t -> bool
val is_open : t -> bool
val close_started : t -> unit Deferred.t
val close_finished : t -> unit Deferred.t
val schedule_bigstring : t -> ?pos:int -> ?len:int -> Bigstring.t -> unit
val write_string : t -> ?pos:int -> ?len:int -> string -> unit
val close : t -> unit Deferred.t
val flush : t -> unit
val flushed : t -> unit Deferred.t
val schedule_iovecs : t -> Bigstring.t Faraday.iovec list -> int
