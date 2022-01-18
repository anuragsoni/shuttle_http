open! Core
open! Async
open! Shuttle

let pipe () =
  Unix.pipe (Info.of_string "test shuttle http")
  >>| fun (`Reader reader, `Writer writer) ->
  let a = Input_channel.create reader in
  let b = Output_channel.create writer in
  a, b
;;

let test_post_req_with_fixed_body =
  "POST /hello HTTP/1.1\r\n\
   Host: www.example.com   \r\n\
   Content-Length: 5\r\n\
   Connection: close\r\n\
   \r\n\
   Hello\r\n"
;;

let%expect_test "test simple server" =
  let open Shuttle_http in
  let stdout = Lazy.force Writer.stdout in
  let handler ~body req =
    let body = Body.Reader.pipe body in
    let%bind () =
      Pipe.iter_without_pushback body ~f:(fun v -> Writer.write_line stdout v)
    in
    Writer.writef stdout !"%{sexp: Cohttp.Request.t}\n" req;
    Server.respond_string "World"
  in
  let%bind reader, write_to_reader = pipe () in
  let%bind read_from_writer, writer = pipe () in
  let reader_pipe = Input_channel.pipe read_from_writer in
  let finished = Ivar.create () in
  (Shuttle_http.Server.run_server_loop handler reader writer
  >>> fun () -> Ivar.fill finished ());
  Output_channel.write write_to_reader test_post_req_with_fixed_body;
  Output_channel.schedule_flush write_to_reader;
  let%bind () = Ivar.read finished in
  let%bind () =
    [%expect
      {|
    Hello
    ((headers ((Host www.example.com) (Content-Length 5) (Connection close)))
     (meth POST) (scheme ()) (resource /hello) (version HTTP_1_1)
     (encoding (Fixed 5))) |}]
  in
  let%bind () = Output_channel.close writer in
  let%bind () =
    Pipe.iter_without_pushback reader_pipe ~f:(fun v -> Writer.writef stdout "%S" v)
  in
  [%expect {| "HTTP/1.1 200 OK OK\r\ncontent-length: 5\r\n\r\nWorld" |}]
;;
