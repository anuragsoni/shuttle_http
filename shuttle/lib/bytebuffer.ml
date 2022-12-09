open! Core

(*= Bytebuffer is split into three regions using two separate indices that are used
    to support read and write operations.
    +--------------------+---------------------------+----------------------------+
    | Consumed Bytes     | Bytes available to read   | Empty space for writing    |
    +--------------------+---------------------------+----------------------------+
    |     0 <=       pos_read         <=          pos_fill              <= capacity

    Consumed Bytes: This is content that's already consumed via a get/read operation.
    This space can be safely reclaimed.

    Bytes available to read: This is the actual content that will be surfaced to users via
    get/read operations on the bytebuffer.

    Empty space for writing: This is space that will be filled by any set/write operations
    on the bytebuffer.
*)
type t =
  { mutable buf : (Bytes.t[@sexp.opaque])
  ; mutable pos_read : int
  ; mutable pos_fill : int
  ; max_buffer_size : int
  ; original_capacity : int
  }
[@@deriving sexp_of]

external read_assume_fd_is_nonblocking
  :  Core_unix.File_descr.t
  -> pos:int
  -> len:int
  -> bytes
  -> Core_unix.Syscall_result.Int.t
  = "shuttle_stubs_unix_read_assume_nonblocking"

let create ?max_buffer_size size =
  let max_buffer_size =
    match max_buffer_size with
    | None -> Int.max_value
    | Some s -> s
  in
  if size > max_buffer_size
  then
    raise_s
      [%message
        "Invalid buffer size"
          ~requested_size:(size : int)
          ~max_buffer_size:(max_buffer_size : int)];
  let buf = Bytes.create size in
  { buf; pos_read = 0; pos_fill = 0; max_buffer_size; original_capacity = size }
;;

let compact t =
  if t.pos_read > 0
  then (
    let len = t.pos_fill - t.pos_read in
    if len = 0
    then (
      t.pos_read <- 0;
      t.pos_fill <- 0)
    else (
      Bytes.blit ~src:t.buf ~dst:t.buf ~src_pos:t.pos_read ~dst_pos:0 ~len;
      t.pos_read <- 0;
      t.pos_fill <- len))
;;

let length t = t.pos_fill - t.pos_read
let capacity t = Bytes.length t.buf
let available_to_write t = Bytes.length t.buf - t.pos_fill

let drop t len =
  if len < 0 || len > length t then invalid_arg "Bytebuffer.drop: Index out of bounds";
  t.pos_read <- t.pos_read + len
;;

let read_assume_fd_is_nonblocking t fd =
  let res =
    read_assume_fd_is_nonblocking
      fd
      t.buf
      ~pos:t.pos_fill
      ~len:(Bytes.length t.buf - t.pos_fill)
  in
  if Core_unix.Syscall_result.Int.is_ok res
  then (
    let count = Core_unix.Syscall_result.Int.ok_exn res in
    if count > 0 then t.pos_fill <- t.pos_fill + count);
  res
;;

let write_assume_fd_is_nonblocking t fd =
  let res =
    Core_unix.write_assume_fd_is_nonblocking fd t.buf ~pos:t.pos_read ~len:(length t)
  in
  if res > 0 then t.pos_read <- t.pos_read + res;
  res
;;

let ensure_space t len =
  if available_to_write t < len
  then (
    let new_length = Bytes.length t.buf + len in
    if new_length > t.max_buffer_size
    then
      raise_s
        [%message
          "Cannot grow Bytebuffer" ~t:(t : t) ~new_length_requested:(new_length : int)];
    let curr_len = t.pos_fill - t.pos_read in
    let len = Int.min t.max_buffer_size (Int.ceil_pow2 new_length) in
    let new_buf = Bytes.create len in
    Bytes.unsafe_blit ~src:t.buf ~dst:new_buf ~src_pos:t.pos_read ~dst_pos:0 ~len:curr_len;
    t.buf <- new_buf;
    t.pos_read <- 0;
    t.pos_fill <- curr_len)
;;

let add_char t ch =
  ensure_space t 1;
  Bytes.unsafe_set t.buf t.pos_fill ch;
  t.pos_fill <- t.pos_fill + 1
;;

let add_gen t ?(pos = 0) ?len ~total_length ~blit str =
  let len =
    match len with
    | Some i -> i
    | None -> total_length - pos
  in
  Ordered_collection_common.check_pos_len_exn ~pos ~len ~total_length;
  ensure_space t len;
  blit ~src:str ~src_pos:pos ~dst:t.buf ~dst_pos:t.pos_fill ~len;
  t.pos_fill <- t.pos_fill + len
;;

let add_string t ?pos ?len str =
  add_gen
    t
    ?pos
    ?len
    ~total_length:(String.length str)
    ~blit:Bytes.From_string.unsafe_blit
    str
;;

let add_bytes t ?pos ?len str =
  add_gen t ?pos ?len ~total_length:(Bytes.length str) ~blit:Bytes.unsafe_blit str
;;

let add_bigstring t ?pos ?len str =
  add_gen
    t
    ?pos
    ?len
    ~total_length:(Bigstring.length str)
    ~blit:Bigstring.To_bytes.unsafe_blit
    str
;;

let add_bytebuffer t buf = add_bytes t ~pos:buf.pos_read ~len:(length buf) buf.buf
let to_string t = Bytes.To_string.sub t.buf ~pos:t.pos_read ~len:(length t)

module Slice = struct
  type t =
    { buf : (Bytes.t[@sexp.opaque])
    ; pos : int
    ; len : int
    }
  [@@deriving sexp_of]
end

let unsafe_peek t = { Slice.buf = t.buf; pos = t.pos_read; len = length t }

let slice ?(pos = 0) ?len t =
  let total_length = length t in
  let len =
    match len with
    | None -> total_length - pos
    | Some l -> l
  in
  Ordered_collection_common.check_pos_len_exn ~pos ~len ~total_length;
  { Slice.buf = t.buf; pos = pos + t.pos_read; len }
;;

let rec index_rec t ch idx len =
  if idx = len
  then -1
  else if Char.equal (Bytes.unsafe_get t.buf (t.pos_read + idx)) ch
  then idx
  else index_rec t ch (idx + 1) len
;;

let unsafe_index t ch = index_rec t ch 0 (length t)
