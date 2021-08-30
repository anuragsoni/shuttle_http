open! Core

type t [@@deriving sexp_of]

val create : int -> t
val capacity : t -> int
val available_to_write : t -> int
val compact : t -> unit
val length : t -> int
val drop : t -> int -> unit
val unsafe_find : t -> char -> int

module To_bytes : Blit.S_distinct with type src := t with type dst := bytes

module To_string : sig
  val sub : (t, string) Blit.sub
  val subo : (t, string) Blit.subo
end

val read : Unix.File_descr.t -> t -> int
val read_assume_fd_is_nonblocking : Unix.File_descr.t -> t -> Unix.Syscall_result.Int.t
val write : Unix.File_descr.t -> t -> int
val write_assume_fd_is_nonblocking : Unix.File_descr.t -> t -> int

module Fill : sig
  val char : t -> char -> unit
  val string : t -> ?pos:int -> ?len:int -> string -> unit
  val bytes : t -> ?pos:int -> ?len:int -> bytes -> unit
  val bigstring : t -> ?pos:int -> ?len:int -> Bigstring.t -> unit
  val bytebuffer : t -> t -> unit
  val int16_be_exn : t -> int -> unit
  val int32_be_exn : t -> int -> unit
end

module Consume : sig
  val stringo : (t, string) Blit.subo
  val int16_be : t -> int
  val int32_be : t -> int
  val unsafe_bigstring : t -> f:(Bigstring.t -> pos:int -> len:int -> int) -> unit
end
