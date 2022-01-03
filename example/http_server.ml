open Core
open Async
open Shuttle
open H11

let text =
  "CHAPTER I. Down the Rabbit-Hole  Alice was beginning to get very tired of sitting by \
   her sister on the bank, and of having nothing to do: once or twice she had peeped \
   into the book her sister was reading, but it had no pictures or conversations in it, \
   <and what is the use of a book,> thought Alice <without pictures or conversations?> \
   So she was considering in her own mind (as well as she could, for the hot day made \
   her feel very sleepy and stupid), whether the pleasure of making a daisy-chain would \
   be worth the trouble of getting up and picking the daisies, when suddenly a White \
   Rabbit with pink eyes ran close by her. There was nothing so very remarkable in that; \
   nor did Alice think it so very much out of the way to hear the Rabbit say to itself, \
   <Oh dear! Oh dear! I shall be late!> (when she thought it over afterwards, it \
   occurred to her that she ought to have wondered at this, but at the time it all \
   seemed quite natural); but when the Rabbit actually took a watch out of its \
   waistcoat-pocket, and looked at it, and then hurried on, Alice started to her feet, \
   for it flashed across her mind that she had never before seen a rabbit with either a \
   waistcoat-pocket, or a watch to take out of it, and burning with curiosity, she ran \
   across the field after it, and fortunately was just in time to see it pop down a \
   large rabbit-hole under the hedge. In another moment down went Alice after it, never \
   once considering how in the world she was to get out again. The rabbit-hole went \
   straight on like a tunnel for some way, and then dipped suddenly down, so suddenly \
   that Alice had not a moment to think about stopping herself before she found herself \
   falling down a very deep well. Either the well was very deep, or she fell very \
   slowly, for she had plenty of time as she went down to look about her and to wonder \
   what was going to happen next. First, she tried to look down and make out what she \
   was coming to, but it was too dark to see anything; then she looked at the sides of \
   the well, and noticed that they were filled with cupboards......"
;;

module IO = struct
  module Deferred = Deferred
  module Reader = Input_channel

  module Writer = struct
    include Output_channel

    let write t buf = write t buf
  end
end

module Server = Server.Make (IO)

let benchmark =
  let open Cohttp in
  let headers = Header.of_list [ "content-length", Int.to_string (String.length text) ] in
  let handler conn =
    let request = Server.Connection.request conn in
    let target = Request.resource request in
    match target with
    | "/" ->
      let response = Response.make ~headers ~status:`OK () in
      Server.Connection.respond_with_string conn response text
    | "/post" ->
      let meth = Request.meth request in
      (match meth with
      | `POST ->
        let request_body = Server.Connection.request_body conn in
        let response =
          Response.make
            ~headers:(Cohttp.Header.of_list [ "transfer-encoding", "chunked" ])
            ~status:`OK
            ()
        in
        Server.Connection.respond_with_stream conn response request_body (fun body sink ->
            Server.Body.Reader.iter body ~f:sink)
      | m ->
        failwithf
          "Unexpected method %S for path /echo"
          (Cohttp.Code.string_of_method m)
          ())
    | "/echo" ->
      let meth = Request.meth request in
      (match meth with
      | `POST ->
        let response = Response.make ~headers ~status:`OK () in
        Server.Connection.respond_with_string conn response text
      | m ->
        failwithf
          "Unexpected method %S for path /echo"
          (Cohttp.Code.string_of_method m)
          ())
    | path -> failwithf "path %S not found." path ()
  in
  handler
;;

let error_handler ?request:_ status =
  let response =
    let open Cohttp in
    Response.make
      ~headers:(Header.of_list [ "Content-Length", "0"; "Connection", "close" ])
      ~status
      ()
  in
  return (response, "")
;;

let main port max_accepts_per_batch () =
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  let%bind server =
    Connection.listen
      ~input_buffer_size:0x4000
      ~output_buffer_size:0x4000
      ~on_handler_error:`Raise
      ~backlog:11_000
      ~max_connections:10_000
      ~max_accepts_per_batch
      where_to_listen
      ~f:(fun _addr reader writer -> Server.run reader writer benchmark error_handler)
  in
  Deferred.forever () (fun () ->
      Clock.after Time.Span.(of_sec 0.5)
      >>| fun () -> Log.Global.printf "conns: %d" (Tcp.Server.num_connections server));
  Deferred.never ()
;;

let () =
  Command.async
    ~summary:"Start a hello world Async server"
    Command.Param.(
      map
        (both
           (flag
              "-p"
              (optional_with_default 8080 int)
              ~doc:"int Source port to listen on")
           (flag "-a" (optional_with_default 1 int) ~doc:"int Maximum accepts per batch"))
        ~f:(fun (port, accepts) () -> main port accepts ()))
  |> Command.run
;;
