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
  "POST /wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg HTTP/1.1\r\n\
   Host: www.kittyhell.com   \r\n\
   User-Agent: Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; rv:1.9.2.3) \
   Gecko/20100401 Firefox/3.6.3 Pathtraq/0.9\r\n\
   Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
   Accept-Language: ja,en-us;q=0.7,en;q=0.3\r\n\
   Accept-Encoding: gzip,deflate\r\n\
   Accept-Charset: Shift_JIS,utf-8;q=0.7,*;q=0.7\r\n\
   Content-Length: 5\r\n\
   Connection: close\r\n\
   Cookie: wp_ozh_wsa_visits=2; wp_ozh_wsa_visit_lasttime=xxxxxxxxxx; \
   __utma=xxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.x; \
   __utmz=xxxxxxxxx.xxxxxxxxxx.x.x.utmccn=(referral)|utmcsr=reader.livedoor.com|utmcct=/reader/|utmcmd=referral\r\n\
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
    Writer.writef stdout !"%{sexp: Cohttp.Request.t}" req;
    Server.respond_string "Hello"
  in
  let%bind reader, writer = pipe () in
  let finished = Ivar.create () in
  (Shuttle_http.Server.run_server_loop handler reader writer
  >>> fun () -> Ivar.fill finished ());
  Output_channel.write writer test_post_req_with_fixed_body;
  Output_channel.schedule_flush writer;
  let%bind () = Ivar.read finished in
  [%expect
    {|
    Hello
    ((headers
      ((Host www.kittyhell.com)
       (User-Agent
        "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3 Pathtraq/0.9")
       (Accept "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
       (Accept-Language "ja,en-us;q=0.7,en;q=0.3") (Accept-Encoding gzip,deflate)
       (Accept-Charset "Shift_JIS,utf-8;q=0.7,*;q=0.7") (Content-Length 5)
       (Connection close)
       (Cookie
        "wp_ozh_wsa_visits=2; wp_ozh_wsa_visit_lasttime=xxxxxxxxxx; __utma=xxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.x; __utmz=xxxxxxxxx.xxxxxxxxxx.x.x.utmccn=(referral)|utmcsr=reader.livedoor.com|utmcct=/reader/|utmcmd=referral")))
     (meth POST) (scheme ())
     (resource /wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg)
     (version HTTP_1_1) (encoding (Fixed 5))) |}]
;;
