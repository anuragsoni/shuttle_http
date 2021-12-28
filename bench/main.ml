open Core

(* Test data taken from
   https://github.com/seanmonstar/httparse/blob/5c385a9b1751f0734db24af731d1926e1d2bc731/benches/parse.rs#L13
   https://github.com/rust-bakery/parser_benchmarks/blob/29b8b49759587d0bb44a75575c004a8b990939de/http/httparse/src/main.rs *)
let req =
  "GET /wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg HTTP/1.1\r\n\
   Host: www.kittyhell.com\r\n\
   User-Agent: Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; rv:1.9.2.3) \
   Gecko/20100401 Firefox/3.6.3 Pathtraq/0.9\r\n\
   Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
   Accept-Language: ja,en-us;q=0.7,en;q=0.3\r\n\
   Accept-Encoding: gzip,deflate\r\n\
   Accept-Charset: Shift_JIS,utf-8;q=0.7,*;q=0.7\r\n\
   Keep-Alive: 115\r\n\
   Connection: keep-alive\r\n\
   Cookie: wp_ozh_wsa_visits=2; wp_ozh_wsa_visit_lasttime=xxxxxxxxxx; \
   __utma=xxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.xxxxxxxxxx.x; \
   __utmz=xxxxxxxxx.xxxxxxxxxx.x.x.utmccn=(referral)|utmcsr=reader.livedoor.com|utmcct=/reader/|utmcmd=referral\r\n\
   \r\n"
;;

open Core_bench

let hex_str = "fffffffe"
let hex_chunk_size = Printf.sprintf "%s\r\n" hex_str

let tests =
  [ Bench.Test.create ~name:"H1 (httparse example)" (fun () ->
        match H11.Parser.parse_request req with
        | Error _ -> assert false
        | Ok _ -> ())
  ; Bench.Test.create ~name:"Parse chunk size" (fun () ->
        match H11.Parser.parse_chunk_length hex_chunk_size with
        | Error _ -> assert false
        | Ok _ -> ())
  ; Bench.Test.create ~name:"Parse hex number" (fun () ->
        Int64.of_string ("0x" ^ hex_str))
  ]
;;

let () = Command.run (Bench.make_command tests)
