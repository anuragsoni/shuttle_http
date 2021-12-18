open! Core


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
  { mutable buf : (Bytes.t[@sexp.opaque])
  ; mutable pos_read : int
  ; mutable pos_fill : int
  }
[@@deriving sexp_of]

let create size =
  let buf = Bytes.create size in
  { buf; pos_read = 0; pos_fill = 0 }
;;

let unsafe_buf t = t.buf
let pos t = t.pos_read

let compact t =
  if t.pos_read > 0
  then (
    let len = t.pos_fill - t.pos_read in
    Bytes.blit ~src:t.buf ~dst:t.buf ~src_pos:t.pos_read ~dst_pos:0 ~len;
    t.pos_read <- 0;
    t.pos_fill <- len)
;;

let length t = t.pos_fill - t.pos_read
let can_reclaim_space t = t.pos_read > 0
let capacity t = Bytes.length t.buf
let available_to_write t = Bytes.length t.buf - t.pos_fill

let resize t size =
  let curr_len = Bytes.length t.buf in
  let new_len = (Bytes.length t.buf + size) * 2 in
  let new_buf = Bytes.create new_len in
  Bytes.blit ~src:t.buf ~dst:new_buf ~src_pos:0 ~dst_pos:0 ~len:curr_len;
  t.buf <- new_buf
;;

let drop t len =
  if len < 0 || len > length t then invalid_arg "Bytebuffer.drop: Index out of bounds";
  t.pos_read <- t.pos_read + len
;;

let read fd t =
  let count =
    Unix.read fd ~buf:t.buf ~pos:t.pos_fill ~len:(Bytes.length t.buf - t.pos_fill)
  in
  if count > 0 then t.pos_fill <- t.pos_fill + count;
  count
;;

let write fd t =
  let count = Unix.write fd ~buf:t.buf ~pos:t.pos_read ~len:(length t) in
  if count > 0 then t.pos_read <- t.pos_read + count;
  count
;;

let read_assume_fd_is_nonblocking fd t =
  let count =
    Unix.read_assume_fd_is_nonblocking
      fd
      t.buf
      ~pos:t.pos_fill
      ~len:(Bytes.length t.buf - t.pos_fill)
  in
  if count > 0 then t.pos_fill <- t.pos_fill + count;
  count
;;

let write_assume_fd_is_nonblocking fd t =
  let res =
    Unix.write_assume_fd_is_nonblocking fd t.buf ~pos:t.pos_read ~len:(length t)
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
        Bytes.blit ~src:src.buf ~src_pos:(src_pos + src.pos_read) ~dst ~dst_pos ~len
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
    Bytes.set t.buf t.pos_fill ch;
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
    add_gen t ?pos ?len ~total_length:(String.length str) ~blit:Bytes.From_string.blit str
  ;;

  let bytes t ?pos ?len str =
    add_gen t ?pos ?len ~total_length:(Bytes.length str) ~blit:Bytes.blit str
  ;;

  let bigstring t ?pos ?len str =
    add_gen
      t
      ?pos
      ?len
      ~total_length:(Bigstring.length str)
      ~blit:Bigstring.To_bytes.blit
      str
  ;;

  let bytebuffer t buf = bytes t ~pos:buf.pos_read ~len:(length buf) buf.buf
end

module Consume = struct
  let stringo ?pos ?len t =
    let res = To_string.subo ?pos ?len t in
    drop t (String.length res);
    res
  ;;
end
