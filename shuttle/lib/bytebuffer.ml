open! Core

type t =
  { mutable buf : (Bigstring.t[@sexp.opaque])
  ; mutable pos_read : int
  ; mutable pos_fill : int
  ; max_buffer_size : int
  }
[@@deriving sexp_of]

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
  let buf = Bigstring.create size in
  { buf; pos_read = 0; pos_fill = 0; max_buffer_size }
;;

let unsafe_buf t = t.buf
let pos t = t.pos_read

let compact t =
  if t.pos_read > 0
  then (
    let len = t.pos_fill - t.pos_read in
    Bigstring.blit ~src:t.buf ~dst:t.buf ~src_pos:t.pos_read ~dst_pos:0 ~len;
    t.pos_read <- 0;
    t.pos_fill <- len)
;;

let length t = t.pos_fill - t.pos_read
let can_reclaim_space t = t.pos_read > 0
let capacity t = Bigstring.length t.buf
let available_to_write t = Bigstring.length t.buf - t.pos_fill

let maybe_grow_buffer t new_length =
  if new_length > t.max_buffer_size
  then
    raise_s
      [%message
        "Cannot grow Bytebuffer" ~t:(t : t) ~new_length_requested:(new_length : int)];
  let len = Int.min t.max_buffer_size (Int.ceil_pow2 new_length) in
  t.buf <- Bigstring.unsafe_destroy_and_resize t.buf ~len
;;

let drop t len =
  if len < 0 || len > length t then invalid_arg "Bytebuffer.drop: Index out of bounds";
  t.pos_read <- t.pos_read + len
;;

let read fd t =
  let count =
    Bigstring_unix.read fd t.buf ~pos:t.pos_fill ~len:(Bigstring.length t.buf - t.pos_fill)
  in
  if count > 0 then t.pos_fill <- t.pos_fill + count;
  count
;;

let write fd t =
  let count = Bigstring_unix.write fd t.buf ~pos:t.pos_read ~len:(length t) in
  if count > 0 then t.pos_read <- t.pos_read + count;
  count
;;

let read_assume_fd_is_nonblocking fd t =
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

let write_assume_fd_is_nonblocking fd t =
  let res =
    Bigstring_unix.write_assume_fd_is_nonblocking fd t.buf ~pos:t.pos_read ~len:(length t)
  in
  if res > 0 then t.pos_read <- t.pos_read + res;
  res
;;

module Fill = struct
  let char t ch =
    if available_to_write t < 1 then maybe_grow_buffer t (Bigstring.length t.buf + 1);
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
    if available_to_write t < len then maybe_grow_buffer t (Bigstring.length t.buf + len);
    blit ~src:str ~src_pos:pos ~dst:t.buf ~dst_pos:t.pos_fill ~len;
    t.pos_fill <- t.pos_fill + len
  ;;

  let string t ?pos ?len str =
    add_gen
      t
      ?pos
      ?len
      ~total_length:(String.length str)
      ~blit:Bigstring.From_string.blit
      str
  ;;

  let bytes t ?pos ?len str =
    add_gen
      t
      ?pos
      ?len
      ~total_length:(Bytes.length str)
      ~blit:Bigstring.From_bytes.blit
      str
  ;;

  let bigstring t ?pos ?len str =
    add_gen t ?pos ?len ~total_length:(Bigstring.length str) ~blit:Bigstring.blit str
  ;;

  let bytebuffer t buf = bigstring t ~pos:buf.pos_read ~len:(length buf) buf.buf
end

module Consume = struct
  let stringo ?pos ?len t =
    let pos, len =
      Ordered_collection_common.get_pos_len_exn ?pos ?len () ~total_length:(length t)
    in
    let res = Bigstring.To_string.sub t.buf ~pos:(pos + t.pos_read) ~len in
    drop t len;
    res
  ;;
end

let index t ch =
  let idx = Bigstring.unsafe_find t.buf ch ~pos:t.pos_read ~len:(length t) in
  if idx < 0 then -1 else idx - t.pos_read
;;
