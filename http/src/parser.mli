type error =
  | Partial
  | Msg of string

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
val parse_request : ?pos:int -> ?len:int -> string -> (Http.Request.t * int, error) result

val parse_chunk_length : ?pos:int -> ?len:int -> string -> (int * int, error) result

val parse_chunk
  :  ?pos:int
  -> ?len:int
  -> string
  -> chunk_kind
  -> (chunk_parser_result * int, error) result
