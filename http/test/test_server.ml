open! Core
open! Async
open! Shuttle
open! Shuttle_http

let%expect_test "Simple http endpoint" =
  let path = Filename_unix.temp_file "shuttle" "sock" in
  Helper.with_server path ~f:(fun () ->
    Helper.with_client path ~f:(fun r w ->
      let test_post_req_with_fixed_body =
        "POST /hello HTTP/1.1\r\n\
         Host: www.example.com   \r\n\
         Content-Length: 5\r\n\
         Connection: close\r\n\
         \r\n\
         Hello\r\n"
      in
      Writer.write w test_post_req_with_fixed_body;
      let reader = Reader.pipe r in
      let buf = Buffer.create 64 in
      let%map () =
        Pipe.iter_without_pushback reader ~f:(fun v -> Buffer.add_string buf v)
      in
      printf "%S" (Buffer.contents buf);
      [%expect {| "HTTP/1.1 200 \r\nContent-Length: 11\r\n\r\nHello World" |}]))
;;

let%expect_test "Test error handler" =
  let path = Filename_unix.temp_file "shuttle" "sock" in
  Helper.with_server path ~f:(fun () ->
    Helper.with_client path ~f:(fun r w ->
      let test_req = "GET /error HTTP/1.1\r\n\r\n" in
      Writer.write w test_req;
      let reader = Reader.pipe r in
      let buf = Buffer.create 64 in
      let%map () =
        Pipe.iter_without_pushback reader ~f:(fun v -> Buffer.add_string buf v)
      in
      printf "%S" (Buffer.contents buf);
      [%expect {| "HTTP/1.1 500 \r\nConnection: close\r\nContent-Length: 0\r\n\r\n" |}]))
;;

let%expect_test "Can read chunked bodies" =
  let test_post_req_with_chunked_body =
    "POST /echo HTTP/1.1\r\n\
     Host: www.example.com\r\n\
     Transfer-Encoding: chunked\r\n\
     Connection: close\r\n\
     \r\n\
     5\r\n\
     Hello\r\n\
     0\r\n\
     \r\n"
  in
  let path = Filename_unix.temp_file "shuttle" "sock" in
  Helper.with_server path ~f:(fun () ->
    Helper.with_client path ~f:(fun r w ->
      Writer.write w test_post_req_with_chunked_body;
      let reader = Reader.pipe r in
      let buf = Buffer.create 64 in
      let%map () =
        Pipe.iter_without_pushback reader ~f:(fun v -> Buffer.add_string buf v)
      in
      printf "%S" (Buffer.contents buf);
      [%expect
        {| "HTTP/1.1 200 \r\nTransfer-Encoding: chunked\r\n\r\n5\r\nHello\r\n0\r\n\r\n" |}]))
;;

let%expect_test "Can catch bat transfer encoding header" =
  let test_post_req_with_bad_transfer_encoding =
    "POST /hello HTTP/1.1\r\n\
     Host: www.example.com   \r\n\
     Transfer-Encoding: foobar\r\n\
     \r\n\
     Hello\r\n"
  in
  let path = Filename_unix.temp_file "shuttle" "sock" in
  Helper.with_server path ~f:(fun () ->
    Helper.with_client path ~f:(fun r w ->
      Writer.write w test_post_req_with_bad_transfer_encoding;
      let reader = Reader.pipe r in
      let buf = Buffer.create 64 in
      let%map () =
        Pipe.iter_without_pushback reader ~f:(fun v -> Buffer.add_string buf v)
      in
      printf "%S" (Buffer.contents buf);
      [%expect
        {| "HTTP/1.1 400 \r\nConnection: close\r\nContent-Length: 0\r\n\r\n" |}]))
;;
