(** Extensible buffers using bigstrings. *)

open! Core

type t [@@deriving sexp_of]

(** [create ?max_buffer_size size] returns a new empty bytebuffer. The bytebuffer will be
    resized automatically, up-to max_buffer_size, if attempting to add more than [size]
    characters to the bytebuffer. [max_buffer_size] defaults to [Int.max_value]. *)
val create : ?max_buffer_size:int -> int -> t

val compact : t -> unit
val available_to_write : t -> int
val ensure_space : t -> int -> unit

(** [length] returns the number of characters in the bytebuffer. *)
val length : t -> int

(** [capacity] is the size of the underlying bigstring. *)
val capacity : t -> int

(** [drop n] removes [n] bytes from the beginning of the bytebuffer. This is usually
    called after a user processes some data from the buffer using a view into the internal
    bigstring via [unsafe_peek].

    Raises [invalid_arg] if [n] is greater than the buffer's length. *)
val drop : t -> int -> unit

(** [read buf fd] reads bytes from the file descriptor [fd] and appends them to the end of
    the bytebuffer. Returns the number of bytes actually read.

    Raises [Bigstring_unix.IOError] in the case of input errors, or on EOF. *)
val read : t -> Core_unix.File_descr.t -> int

(** [write buf fd] reads data from the beginning of the buffer and writes them to the file
    descriptor [fd].

    Returns the number of bytes actually written.

    Raises [Core_unix.Unix_error] in case of i/o errors. *)
val write : t -> Core_unix.File_descr.t -> int

(** [read_assume_fd_is_nonblocking buf fd] is similar to [read] but it performs the read
    without yielding to other OCaml-threads. This function should only be called for
    non-blocking file-descriptors.

    Returns the number of bytes actually read.

    Raises Invalid_argument if the designated range is out of bounds. *)
val read_assume_fd_is_nonblocking
  :  t
  -> Core_unix.File_descr.t
  -> Core_unix.Syscall_result.Int.t

(** [write_assume_fd_is_nonblocking buf fd] is similar to [write] buf it performs the
    write without yielding to other OCaml-threads. This function should only be called for
    non-blocking file-descriptors.

    Returns the number of bytes actually written.

    Raises [Core_unix.Unix_error] in case of i/o errors. *)
val write_assume_fd_is_nonblocking : t -> Core_unix.File_descr.t -> int

(** [add_char] appends the charater at the end of the bytebuffer. *)
val add_char : t -> char -> unit

(** [add_string] appends the string at the end of the bytebuffer. *)
val add_string : t -> ?pos:int -> ?len:int -> string -> unit

(** [add_bytes] appends the bytes at the end of the bytebuffer. *)
val add_bytes : t -> ?pos:int -> ?len:int -> bytes -> unit

(** [add_bigstring] appends the bigstring at the end of the bytebuffer. *)
val add_bigstring : t -> ?pos:int -> ?len:int -> Core.Bigstring.t -> unit

(** [add_bytebuffer] appends the contents of the buffer at the end of the bytebuffer. *)
val add_bytebuffer : t -> t -> unit

(** [to_string] returns a copy of the current contents of the bytebuffer.*)
val to_string : t -> string

val unsafe_index : t -> char -> int

module Slice : sig
  type t = private
    { buf : Bigstring.t
    ; pos : int
    ; len : int
    }
  [@@deriving sexp_of]
end

val unsafe_peek : t -> Slice.t
val slice : ?pos:int -> ?len:int -> t -> Slice.t
