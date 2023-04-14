open! Core
open! Shuttle
module Unix = Core_unix

let%expect_test "test bytebuffer" =
  let b = Bytebuffer.create 20 in
  Bytebuffer.add_string b "Hello";
  Bytebuffer.add_char b ' ';
  print_endline (Bytebuffer.to_string b);
  [%expect {| Hello |}]
;;

let%expect_test "test resize" =
  let b = Bytebuffer.create 5 in
  printf "Original capacity: %d\n" (Bytebuffer.capacity b);
  [%expect {| Original capacity: 5 |}];
  Bytebuffer.add_string b "hello";
  printf "Capacity: %d, length: %d\n" (Bytebuffer.capacity b) (Bytebuffer.length b);
  [%expect {| Capacity: 5, length: 5 |}];
  Bytebuffer.add_char b ' ';
  printf "Capacity: %d, length: %d\n" (Bytebuffer.capacity b) (Bytebuffer.length b);
  [%expect {| Capacity: 8, length: 6 |}]
;;

let%expect_test "Can read and write from/to file descriptors" =
  let r, w = Unix.pipe () in
  let read_buf = Bytebuffer.create 32 in
  let write_buf = Bytebuffer.create 32 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close r;
      Unix.close w)
    (fun () ->
      Bytebuffer.add_string write_buf "Hello world this is a line\n";
      printf !"Write buf: %{sexp: Bytebuffer.t}\n" write_buf;
      [%expect
        {|
    Write buf: ((buf <opaque>) (pos_read 0) (pos_fill 27)
     (max_buffer_size 4611686018427387903)) |}];
      let write_result = Bytebuffer.write write_buf w in
      printf !"Wrote %d bytes. Write_buf: %{sexp: Bytebuffer.t}\n" write_result write_buf;
      let read_result = Bytebuffer.read read_buf r in
      printf !"Read %d bytes. Write_buf: %{sexp: Bytebuffer.t}\n" read_result read_buf;
      [%expect
        {|
    Wrote 27 bytes. Write_buf: ((buf <opaque>) (pos_read 27) (pos_fill 27)
     (max_buffer_size 4611686018427387903))
    Read 27 bytes. Write_buf: ((buf <opaque>) (pos_read 0) (pos_fill 27)
     (max_buffer_size 4611686018427387903)) |}];
      printf "%s" (Bytebuffer.to_string read_buf));
  [%expect {| Hello world this is a line |}]
;;
