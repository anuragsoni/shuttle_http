open! Core

type error =
  | Partial
  | Fail of Error.t

type chunk_kind =
  | Start_chunk
  | Continue_chunk of int

type chunk_parser_result =
  | Chunk_complete of string
  | Done
  | Partial_chunk of string * int

(** Attempts to parse a buffer into a HTTP request. If successful, it returns the parsed
    request and an offset value that indicates the starting point of unconsumed content
    left in the buffer. *)
val parse_request : ?pos:int -> ?len:int -> Bigstring.t -> (Request.t * int, error) result

val parse_chunk_length : ?pos:int -> ?len:int -> Bigstring.t -> (int * int, error) result

val parse_chunk
  :  ?pos:int
  -> ?len:int
  -> Bigstring.t
  -> chunk_kind
  -> (chunk_parser_result * int, error) result

module Private : sig
  val parse_method : string -> (Meth.t * int, error) result
end
