open! Core

type t [@@deriving sexp_of]

val unsafe_buf : t -> Bigstring.t
val pos : t -> int
val create : int -> t
val can_reclaim_space : t -> bool
val capacity : t -> int
val available_to_write : t -> int
val compact : t -> unit
val length : t -> int
val drop : t -> int -> unit
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
end

module Consume : sig
  val stringo : (t, string) Blit.subo
end

val index : t -> char -> int
