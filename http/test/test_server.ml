open! Core
open! Async
open! Shuttle
open! Shuttle_http

let pipe f =
  Unix.pipe (Info.of_string "test shuttle http")
  >>= fun (`Reader rd, `Writer wr) ->
  let reader = Input_channel.create rd in
  let writer = Output_channel.create wr in
  Monitor.protect
    (fun () -> f reader writer)
    ~finally:(fun () ->
      Output_channel.close writer >>= fun () -> Input_channel.close reader)
;;

let with_server ?error_handler f =
  let stdout = Lazy.force Writer.stdout in
  pipe (fun reader write_to_reader ->
    pipe (fun read_from_writer writer ->
      Deferred.any_unit
        [ Pipe.iter_without_pushback
            ~f:(fun v -> Writer.writef stdout "%S" v)
            (Input_channel.pipe read_from_writer)
        ; f
            (Server.create ?error_handler reader writer)
            ~send_message:(Output_channel.pipe write_to_reader)
        ]))
;;

let test_post_req_with_invalid_body_length =
  "POST /hello HTTP/1.1\r\n\
   Host: www.example.com   \r\n\
   Content-Length: 5\r\n\
   Content-Length: 6\r\n\
   \r\n\
   Hello\r\n"
;;

let%expect_test "test simple server" =
  let test_post_req_with_fixed_body =
    "POST /hello HTTP/1.1\r\n\
     Host: www.example.com   \r\n\
     Content-Length: 5\r\n\
     \r\n\
     Hello\r\n"
  in
  let%map () =
    with_server (fun server ~send_message ->
      let echo request =
        Writer.write_sexp
          ~terminate_with:Writer.Terminate_with.Newline
          ~hum:true
          (Lazy.force Writer.stdout)
          (Request.sexp_of_t request);
        return
          (Response.create
             ~headers:(Headers.of_list [ "Connection", "close" ])
             ~body:(Request.body request)
             `Ok)
      in
      let finished = Server.run server echo in
      let%bind () = Pipe.write send_message test_post_req_with_fixed_body in
      finished)
  in
  [%expect
    {|
    ((meth POST) (path /hello) (version Http_1_1)
     (headers ((Host www.example.com) (Content-Length 5))) (context <opaque>)
     (body (Fixed Hello)))
    "HTTP/1.1 200 \r\nContent-Length: 5\r\nConnection: close\r\n\r\nHello" |}]
;;

let%expect_test "test default error handler" =
  let%map () =
    with_server (fun server ~send_message ->
      let service _request = failwith "ERROR" in
      let finished = Server.run server service in
      let%bind () = Pipe.write send_message "GET /hello HTTP/1.1\r\n\r\n" in
      finished)
  in
  [%expect {| "HTTP/1.1 500 \r\nConnection: close\r\nContent-Length: 0\r\n\r\n" |}]
;;

let%expect_test "test custom error handler" =
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
  let%map () =
    with_server ~error_handler (fun server ~send_message ->
      let finished =
        Server.run server (fun _request ->
          return
            (Response.create ~headers:(Headers.of_list [ "Connection", "close" ]) `Ok))
      in
      let%bind () = Pipe.write send_message test_post_req_with_invalid_body_length in
      finished)
  in
  [%expect
    {| "HTTP/1.1 400 \r\nContent-Length: 40\r\n\r\nSomething bad happened in request /hello" |}]
;;

let%expect_test "Can read chunked bodies" =
  let test_post_req_with_chunked_body =
    "POST /hello HTTP/1.1\r\n\
     Host: www.example.com\r\n\
     Transfer-Encoding: chunked\r\n\
     \r\n\
     5\r\n\
     Hello\r\n\
     0\r\n\
     \r\n"
  in
  let%map () =
    with_server (fun server ~send_message ->
      let echo request =
        Writer.write_sexp
          ~terminate_with:Writer.Terminate_with.Newline
          ~hum:true
          (Lazy.force Writer.stdout)
          (Request.sexp_of_t request);
        return
          (Response.create
             ~headers:(Headers.of_list [ "Connection", "close" ])
             ~body:(Request.body request)
             `Ok)
      in
      let finished = Server.run server echo in
      let%bind () = Pipe.write send_message test_post_req_with_chunked_body in
      finished)
  in
  [%expect
    {|
    ((meth POST) (path /hello) (version Http_1_1)
     (headers ((Host www.example.com) (Transfer-Encoding chunked)))
     (context <opaque>) (body (Stream <opaque>)))
    "HTTP/1.1 200 \r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n5\r\nHello\r\n0\r\n\r\n" |}]
;;

let%expect_test "can catch bad transfer encoding header" =
  let test_post_req_with_bad_transfer_encoding =
    "POST /hello HTTP/1.1\r\n\
     Host: www.example.com   \r\n\
     Transfer-Encoding: foobar\r\n\
     \r\n\
     Hello\r\n"
  in
  let%map () =
    with_server (fun server ~send_message ->
      let echo request =
        Writer.write_sexp
          ~terminate_with:Writer.Terminate_with.Newline
          ~hum:true
          (Lazy.force Writer.stdout)
          (Request.sexp_of_t request);
        return
          (Response.create
             ~headers:(Headers.of_list [ "Connection", "close" ])
             ~body:(Request.body request)
             `Ok)
      in
      let finished = Server.run server echo in
      let%bind () = Pipe.write send_message test_post_req_with_bad_transfer_encoding in
      finished)
  in
  [%expect {| "HTTP/1.1 400 \r\nConnection: close\r\nContent-Length: 0\r\n\r\n" |}]
;;
