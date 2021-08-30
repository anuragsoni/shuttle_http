open! Core
open! Async

(*  Bytebuffer is split into three regions using two separate indices that are used
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
  }
[@@deriving sexp_of]

let create size =
  let buf = Bigstring.create size in
  { buf; pos_read = 0; pos_fill = 0 }
;;

let compact t =
  if t.pos_read > 0
  then (
    let len = t.pos_fill - t.pos_read in
    Bigstring.blit ~src:t.buf ~dst:t.buf ~src_pos:t.pos_read ~dst_pos:0 ~len;
    t.pos_read <- 0;
    t.pos_fill <- len)
;;

let length t = t.pos_fill - t.pos_read
let capacity t = Bigstring.length t.buf
let available_to_write t = Bigstring.length t.buf - t.pos_fill

module Optional_syntax = struct
  let is_none t = t < 0
  let unsafe_value t = t
end

let unsafe_find t ch =
  match%optional
    Bigstring.unsafe_find t.buf ~pos:t.pos_read ~len:(t.pos_fill - t.pos_read) ch
  with
  | None -> -1
  | Some idx -> idx - t.pos_read
;;

let resize t size =
  let new_len = (Bigstring.length t.buf + size) * 2 in
  t.buf <- Bigstring.unsafe_destroy_and_resize t.buf ~len:new_len
;;

let drop t len =
  if len > length t then invalid_arg "Index out of bounds";
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
  if Unix.Syscall_result.Int.is_ok res
  then (
    let count = Unix.Syscall_result.Int.ok_exn res in
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

module To_bytes =
  Blit.Make_distinct
    (struct
      type nonrec t = t

      let length t = length t
    end)
    (struct
      type t = bytes

      let create ~len = Bytes.create len
      let length t = Bytes.length t

      let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
        Bigstring.To_bytes.blit
          ~src:src.buf
          ~src_pos:(src_pos + src.pos_read)
          ~dst
          ~dst_pos
          ~len
      ;;
    end)

module To_string =
  Blit.Make_to_string
    (struct
      type nonrec t = t
    end)
    (To_bytes)

module Fill = struct
  let char t ch =
    if available_to_write t < 1 then resize t 1;
    Bigstring.set t.buf t.pos_fill ch;
    t.pos_fill <- t.pos_fill + 1
  ;;

  let add_gen t ?pos ?len ~total_length ~blit str =
    let src_pos, len =
      Ordered_collection_common.get_pos_len_exn ?pos ?len () ~total_length
    in
    if available_to_write t < len then resize t len;
    blit ~src:str ~src_pos ~dst:t.buf ~dst_pos:t.pos_fill ~len;
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

  let int16_be_exn t v =
    if available_to_write t < 2 then invalid_arg "Index out of bounds";
    Bigstring.set_int16_be_exn t.buf ~pos:t.pos_fill v;
    t.pos_fill <- t.pos_fill + 2
  ;;

  let int32_be_exn t v =
    if available_to_write t < 4 then invalid_arg "Index out of bounds";
    Bigstring.set_int32_be_exn t.buf ~pos:t.pos_fill v;
    t.pos_fill <- t.pos_fill + 4
  ;;
end

module Consume = struct
  let stringo ?pos ?len t =
    let res = To_string.subo ?pos ?len t in
    drop t (String.length res);
    res
  ;;

  let int16_be t =
    if length t < 2 then invalid_arg "Index out of bounds";
    let v = Bigstring.get_int16_be t.buf ~pos:t.pos_read in
    t.pos_read <- t.pos_read + 2;
    v
  ;;

  let int32_be t =
    if length t < 4 then invalid_arg "Index out of bounds";
    let v = Bigstring.get_int32_be t.buf ~pos:t.pos_read in
    t.pos_read <- t.pos_read + 4;
    v
  ;;

  let unsafe_bigstring t ~f =
    let len = length t in
    let count = f t.buf ~pos:t.pos_read ~len in
    if count < 0 || count > len
    then invalid_argf "Bytebuffer.consume: Invalid value for consumed count - %d" count ();
    t.pos_read <- t.pos_read + count
  ;;
end
