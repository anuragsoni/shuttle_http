open! Core
open! Async
open! Shuttle
open! Shuttle_http

let handler request =
  match Request.path request with
  | "/error" -> failwith "ERROR"
  | "/echo" -> return (Response.create ~body:(Request.body request) `Ok)
  | "/no-keep-alive" ->
    return
      (Response.create
         ~headers:(Headers.of_list [ "Connection", "close" ])
         ~body:(Body.string "This connection will be closed")
         `Ok)
  | _ -> return (Response.create ~body:(Body.string "Hello World") `Ok)
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

let%expect_test "Simple http endpoint with http client" =
  Helper.with_server handler ~f:(fun port ->
    let%map response =
      Client.Oneshot.call
        (Client.Address.of_host_and_port (Host_and_port.create ~host:"localhost" ~port))
        (Request.create
           ~headers:
             (Headers.of_rev_list [ "Host", "www.example.com   "; "Connection", "close" ])
           ~body:(Body.string "Hello")
           `POST
           "/hello")
    in
    printf !"%{sexp: Response.t}" response;
    [%expect
      {|
    ((version Http_1_1) (status Ok) (reason_phrase "")
     (headers ((Content-Length 11))) (body (Fixed "Hello World"))) |}])
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
    (fun _ -> failwith "ERROR")
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

let%expect_test "Client can send streaming bodies" =
  Helper.with_server handler ~f:(fun port ->
    let body =
      Body.of_pipe
        `Chunked
        (Pipe.create_reader ~close_on_exception:false (fun writer ->
           Deferred.repeat_until_finished 1 (fun count ->
             if count > 5
             then return (`Finished ())
             else (
               let%map () = Pipe.write writer (sprintf "Hello: %d " count) in
               `Repeat (count + 1)))))
    in
    let%bind response =
      Client.Oneshot.call
        (Client.Address.of_host_and_port (Host_and_port.create ~host:"localhost" ~port))
        (Request.create ~body `POST "/echo")
    in
    let%map body =
      let buf = Buffer.create 32 in
      let%map () =
        Body.Stream.iter
          (Body.to_stream (Response.body response))
          ~f:(fun v ->
            Buffer.add_string buf v;
            Deferred.unit)
      in
      Buffer.contents buf
    in
    print_s
      [%sexp
        { status = (Response.status response : Status.t)
        ; headers = (Response.headers response : Headers.t)
        ; reason_phrase = (Response.reason_phrase response : string)
        }];
    printf "\nBody: %S" body;
    [%expect
      {|
        ((status Ok) (headers ((Transfer-Encoding chunked))) (reason_phrase ""))

        Body: "Hello: 1 Hello: 2 Hello: 3 Hello: 4 Hello: 5 " |}])
;;

let%expect_test "Keep-alives in clients" =
  Helper.with_server handler ~f:(fun port ->
    let%bind client =
      Deferred.Or_error.ok_exn
        (Client.create
           (Client.Address.of_host_and_port
              (Host_and_port.create ~host:"localhost" ~port)))
    in
    let body_to_string response =
      let buf = Buffer.create 32 in
      let%map () =
        Body.Stream.iter
          (Body.to_stream (Response.body response))
          ~f:(fun v ->
            Buffer.add_string buf v;
            Deferred.unit)
      in
      Buffer.contents buf
    in
    Monitor.protect
      ~finally:(fun () ->
        Client.close client;
        Client.closed client)
      (fun () ->
        let%bind response = Client.call client (Request.create `GET "/") in
        print_s
          [%sexp
            { status = (Response.status response : Status.t)
            ; headers = (Response.headers response : Headers.t)
            ; reason_phrase = (Response.reason_phrase response : string)
            }];
        let%bind body = body_to_string response in
        printf "\nBody: %S" body;
        [%expect
          {|
    ((status Ok) (headers ((Content-Length 11))) (reason_phrase ""))

    Body: "Hello World" |}];
        let%bind response =
          Client.call
            client
            (Request.create ~body:(Body.string "This is a body") `POST "/echo")
        in
        print_s
          [%sexp
            { status = (Response.status response : Status.t)
            ; headers = (Response.headers response : Headers.t)
            ; reason_phrase = (Response.reason_phrase response : string)
            }];
        let%map body = body_to_string response in
        printf "\nBody: %S" body;
        [%expect
          {|
    ((status Ok) (headers ((Content-Length 14))) (reason_phrase ""))

    Body: "This is a body" |}]))
;;

let ensure_aborted fn =
  Monitor.try_with fn
  >>= function
  | Ok response ->
    failwithf
      !"Expected request to be aborted, but received a response instead: %{sexp: \
        Response.t}"
      response
      ()
  | Error exn ->
    (match Monitor.extract_exn exn with
     | Client.Request_aborted -> return "Request aborted"
     | exn -> raise exn)
;;

let%expect_test "No requests can be sent if a client is closed" =
  Helper.with_server handler ~f:(fun port ->
    let%bind client =
      Deferred.Or_error.ok_exn
        (Client.create
           (Client.Address.of_host_and_port
              (Host_and_port.create ~host:"localhost" ~port)))
    in
    let body_to_string response =
      let buf = Buffer.create 32 in
      let%map () =
        Body.Stream.iter
          (Body.to_stream (Response.body response))
          ~f:(fun v ->
            Buffer.add_string buf v;
            Deferred.unit)
      in
      Buffer.contents buf
    in
    Monitor.protect
      ~finally:(fun () ->
        Client.close client;
        Client.closed client)
      (fun () ->
        let%bind response = Client.call client (Request.create `GET "/") in
        print_s
          [%sexp
            { status = (Response.status response : Status.t)
            ; headers = (Response.headers response : Headers.t)
            ; reason_phrase = (Response.reason_phrase response : string)
            }];
        let%bind body = body_to_string response in
        printf "\nBody: %S" body;
        [%expect
          {|
    ((status Ok) (headers ((Content-Length 11))) (reason_phrase ""))

    Body: "Hello World" |}];
        Client.close client;
        let%map msg =
          ensure_aborted (fun () ->
            Client.call
              client
              (Request.create ~body:(Body.string "This is a body") `POST "/echo"))
        in
        printf "%s" msg;
        [%expect {| Request aborted |}]))
;;

let%expect_test "Clients are automatically closed if Connection:close header is present \
                 in request"
  =
  Helper.with_server handler ~f:(fun port ->
    let%bind client =
      Deferred.Or_error.ok_exn
        (Client.create
           (Client.Address.of_host_and_port
              (Host_and_port.create ~host:"localhost" ~port)))
    in
    let body_to_string response =
      let buf = Buffer.create 32 in
      let%map () =
        Body.Stream.iter
          (Body.to_stream (Response.body response))
          ~f:(fun v ->
            Buffer.add_string buf v;
            Deferred.unit)
      in
      Buffer.contents buf
    in
    Monitor.protect
      ~finally:(fun () ->
        Client.close client;
        Client.closed client)
      (fun () ->
        let%bind response =
          Client.call
            client
            (Request.create ~headers:(Headers.of_list [ "Connection", "close" ]) `GET "/")
        in
        print_s
          [%sexp
            { status = (Response.status response : Status.t)
            ; headers = (Response.headers response : Headers.t)
            ; reason_phrase = (Response.reason_phrase response : string)
            }];
        let%bind body = body_to_string response in
        printf "\nBody: %S" body;
        [%expect
          {|
    ((status Ok) (headers ((Content-Length 11))) (reason_phrase ""))

    Body: "Hello World" |}];
        let%map msg =
          ensure_aborted (fun () ->
            Client.call
              client
              (Request.create ~body:(Body.string "This is a body") `POST "/echo"))
        in
        printf "%s" msg;
        [%expect {| Request aborted |}]))
;;

let%expect_test "Clients are automatically closed if Connection:close header is present \
                 in response"
  =
  Helper.with_server handler ~f:(fun port ->
    let%bind client =
      Deferred.Or_error.ok_exn
        (Client.create
           (Client.Address.of_host_and_port
              (Host_and_port.create ~host:"localhost" ~port)))
    in
    let body_to_string response =
      let buf = Buffer.create 32 in
      let%map () =
        Body.Stream.iter
          (Body.to_stream (Response.body response))
          ~f:(fun v ->
            Buffer.add_string buf v;
            Deferred.unit)
      in
      Buffer.contents buf
    in
    Monitor.protect
      ~finally:(fun () ->
        Client.close client;
        Client.closed client)
      (fun () ->
        let%bind response = Client.call client (Request.create `GET "/no-keep-alive") in
        print_s
          [%sexp
            { status = (Response.status response : Status.t)
            ; headers = (Response.headers response : Headers.t)
            ; reason_phrase = (Response.reason_phrase response : string)
            }];
        let%bind body = body_to_string response in
        printf "\nBody: %S" body;
        [%expect
          {|
    ((status Ok) (headers ((Content-Length 30) (Connection close)))
     (reason_phrase ""))

    Body: "This connection will be closed" |}];
        let%map msg =
          ensure_aborted (fun () ->
            Client.call
              client
              (Request.create ~body:(Body.string "This is a body") `POST "/echo"))
        in
        printf "%s" msg;
        [%expect {| Request aborted |}]))
;;
