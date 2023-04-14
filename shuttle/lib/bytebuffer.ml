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
  { mutable buf : (Bigstring.t[@sexp.opaque])
  ; mutable pos_read : int
  ; mutable pos_fill : int
  ; max_buffer_size : int
  }
[@@deriving sexp_of]

exception
  Maximum_buffer_size_exceeded of
    { current_length : int
    ; new_length_requested : int
    }

let create ?max_buffer_size size =
  if size < 0
  then raise_s [%message "Buffer size cannot be negative" ~requested_size:(size : int)];
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
  let buf = Bigstring.create size in
  { buf; pos_read = 0; pos_fill = 0; max_buffer_size }
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
      Bigstring.blit ~src:t.buf ~dst:t.buf ~src_pos:t.pos_read ~dst_pos:0 ~len;
      t.pos_read <- 0;
      t.pos_fill <- len))
;;

let length t = t.pos_fill - t.pos_read
let capacity t = Bigstring.length t.buf
let max_buffer_size t = t.max_buffer_size
let available_to_write t = Bigstring.length t.buf - t.pos_fill

let drop t len =
  if len < 0 || len > length t then invalid_arg "Bytebuffer.drop: Index out of bounds";
  t.pos_read <- t.pos_read + len
;;

let read t fd =
  let count =
    Bigstring_unix.read fd t.buf ~pos:t.pos_fill ~len:(Bigstring.length t.buf - t.pos_fill)
  in
  if count > 0 then t.pos_fill <- t.pos_fill + count;
  count
;;

let write t fd =
  let count = Bigstring_unix.write fd t.buf ~pos:t.pos_read ~len:(length t) in
  if count > 0 then t.pos_read <- t.pos_read + count;
  count
;;

let read_assume_fd_is_nonblocking t fd =
  let res =
    Bigstring_unix.read_assume_fd_is_nonblocking
      fd
      t.buf
      ~pos:t.pos_fill
      ~len:(Bigstring.length t.buf - t.pos_fill)
  in
  if Core_unix.Syscall_result.Int.is_ok res
  then (
    let count = Core_unix.Syscall_result.Int.ok_exn res in
    if count > 0 then t.pos_fill <- t.pos_fill + count);
  res
;;

let write_assume_fd_is_nonblocking t fd =
  let res =
    Bigstring_unix.write_assume_fd_is_nonblocking fd t.buf ~pos:t.pos_read ~len:(length t)
  in
  if res > 0 then t.pos_read <- t.pos_read + res;
  res
;;

let ensure_space t len =
  if available_to_write t < len
  then (
    let new_length = Bigstring.length t.buf + len in
    if new_length > t.max_buffer_size
    then
      raise
        (Maximum_buffer_size_exceeded
           { new_length_requested = new_length; current_length = length t });
    let curr_len = t.pos_fill - t.pos_read in
    let len = Int.min t.max_buffer_size (Int.ceil_pow2 new_length) in
    let new_buf = Bigstring.create len in
    Bigstring.unsafe_blit
      ~src:t.buf
      ~dst:new_buf
      ~src_pos:t.pos_read
      ~dst_pos:0
      ~len:curr_len;
    t.buf <- new_buf;
    t.pos_read <- 0;
    t.pos_fill <- curr_len)
;;

let add_char t ch =
  ensure_space t 1;
  Bigstring.set t.buf t.pos_fill ch;
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
    ~blit:Bigstring.From_string.blit
    str
;;

let add_bytes t ?pos ?len str =
  add_gen t ?pos ?len ~total_length:(Bytes.length str) ~blit:Bigstring.From_bytes.blit str
;;

let add_bigstring t ?pos ?len str =
  add_gen t ?pos ?len ~total_length:(Bigstring.length str) ~blit:Bigstring.blit str
;;

let add_bytebuffer t buf = add_bigstring t ~pos:buf.pos_read ~len:(length buf) buf.buf
let to_string t = Bigstring.To_string.sub t.buf ~pos:t.pos_read ~len:(length t)
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

let unsafe_index t ch =
  let idx = Bigstring.unsafe_find t.buf ch ~pos:t.pos_read ~len:(length t) in
  if idx < 0 then -1 else idx - t.pos_read
;;
