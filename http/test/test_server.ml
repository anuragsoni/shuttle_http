open! Core
open! Async
open! Shuttle
open! Shuttle_http

let handler ctx request =
  match Request.path request with
  | "/error" -> failwith "ERROR"
  | "/echo" -> return (Server.respond_stream ctx (Body.to_stream (Request.body request)))
  | _ -> return (Server.respond_string ctx "Hello World")
;;

let%expect_test "Simple http endpoint" =
  Helper.with_server handler ~f:(fun port ->
    Helper.with_client port ~f:(fun r w ->
      let test_post_req_with_fixed_body =
        "POST /hello HTTP/1.1\r\n\
         Host: www.example.com   \r\n\
         Content-Length: 5\r\n\
         Connection: close\r\n\
         \r\n\
         Hello\r\n"
      in
      let%map () =
        Helper.send_request_and_log_response r w test_post_req_with_fixed_body
      in
      [%expect {| "HTTP/1.1 200 \r\nContent-Length: 11\r\n\r\nHello World" |}]))
;;

let%expect_test "Test default error handler" =
  Helper.with_server handler ~f:(fun port ->
    Helper.with_client port ~f:(fun r w ->
      let%map () =
        Helper.send_request_and_log_response r w "GET /error HTTP/1.1\r\n\r\n"
      in
      [%expect {| "HTTP/1.1 500 \r\nConnection: close\r\nContent-Length: 0\r\n\r\n" |}]))
;;

let%expect_test "Test custom error handler" =
  let error_handler ?exn:_ ?request status =
    let body =
      match request with
      | None -> Body.string "Something bad happened"
      | Some request ->
        Body.string
          (sprintf "Something bad happened in request %s" (Request.path request))
    in
    return (Response.create ~body status)
  in
  Helper.with_server
    ~error_handler
    (fun _ _ -> failwith "ERROR")
    ~f:(fun port ->
      let%bind () =
        Helper.with_client port ~f:(fun r w ->
          let%map () =
            Helper.send_request_and_log_response r w "GET / HTTP/1.1\r\n\r\n"
          in
          [%expect
            {| "HTTP/1.1 500 \r\nContent-Length: 22\r\n\r\nSomething bad happened" |}])
      in
      let test_post_req_with_invalid_body_length =
        "POST /hello HTTP/1.1\r\n\
         Host: www.example.com   \r\n\
         Content-Length: 5\r\n\
         Content-Length: 6\r\n\
         \r\n\
         Hello\r\n"
      in
      Helper.with_client port ~f:(fun r w ->
        let%map () =
          Helper.send_request_and_log_response r w test_post_req_with_invalid_body_length
        in
        [%expect
          {| "HTTP/1.1 400 \r\nContent-Length: 40\r\n\r\nSomething bad happened in request /hello" |}]))
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
  Helper.with_server handler ~f:(fun port ->
    Helper.with_client port ~f:(fun r w ->
      let%map () =
        Helper.send_request_and_log_response r w test_post_req_with_chunked_body
      in
      [%expect
        {| "HTTP/1.1 200 \r\nTransfer-Encoding: chunked\r\n\r\n5\r\nHello\r\n0\r\n\r\n" |}]))
;;

let%expect_test "Can catch bad transfer encoding header" =
  let test_post_req_with_bad_transfer_encoding =
    "POST /hello HTTP/1.1\r\n\
     Host: www.example.com   \r\n\
     Transfer-Encoding: foobar\r\n\
     \r\n\
     Hello\r\n"
  in
  Helper.with_server handler ~f:(fun port ->
    Helper.with_client port ~f:(fun r w ->
      let%map () =
        Helper.send_request_and_log_response r w test_post_req_with_bad_transfer_encoding
      in
      [%expect {| "HTTP/1.1 400 \r\nConnection: close\r\nContent-Length: 0\r\n\r\n" |}]))
;;

let%expect_test "Servers will respond with a timeout if they can't parse request headers \
                 in the given timeframe"
  =
  Helper.with_server
    ~read_header_timeout:(Time_ns.Span.of_ms 100.)
    handler
    ~f:(fun port ->
    Helper.with_client port ~f:(fun r w ->
      let test_post_req_with_fixed_body =
        "POST /hello HTTP/1.1\r\n\
         Host: www.example.com   \r\n\
         Content-Length: 5\r\n\
         Connection: close\r\n\
         \r\n\
         Hello\r\n"
      in
      let%map () =
        let%bind () = after (Time.Span.of_ms 101.) in
        Helper.send_request_and_log_response r w test_post_req_with_fixed_body
      in
      [%expect {| "HTTP/1.1 408 \r\nConnection: close\r\nContent-Length: 0\r\n\r\n" |}]))
;;
