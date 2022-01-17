open! Core
open! Async
open! Shuttle
open Ppx_log_async.No_global
module Logger = Log.Make_global ()

let log = Lazy.force Logger.log

type response = Http.Response.t * Body.Writer.t

let write_response writer res =
  let module Writer = Output_channel in
  let open Cohttp in
  let headers =
    Header.add_transfer_encoding (Response.headers res) (Response.encoding res)
  in
  let res = { res with headers } in
  Writer.write writer (Code.string_of_version (Response.version res));
  Writer.write_char writer ' ';
  Writer.write writer (Code.string_of_status (Response.status res));
  Writer.write_char writer ' ';
  Writer.write
    writer
    (Code.reason_phrase_of_code (Code.code_of_status (Response.status res)));
  Writer.write writer "\r\n";
  Header.iter
    (fun key data ->
      Writer.write writer key;
      Writer.write writer ": ";
      Writer.write writer data;
      Writer.write writer "\r\n")
    (Response.headers res);
  Writer.write writer "\r\n"
;;

let run_server_loop handle_request reader writer =
  let rec loop reader writer handle_request =
    match%bind Io.Request.read reader with
    | `Eof -> Deferred.unit
    | `Invalid msg ->
      [%log.error log "Error while parsing http request: %S" msg];
      Deferred.unit
    | `Ok req ->
      let req_body = Body.Reader.Private.create req reader in
      let%bind res, res_body = handle_request ~body:req_body req in
      let keep_alive =
        Http.Request.is_keep_alive req && Http.Response.is_keep_alive res
      in
      write_response writer res;
      let%bind () = Body.Writer.Private.write res_body writer in
      let%bind () = Body.Reader.drain req_body in
      if keep_alive then loop reader writer handle_request else Deferred.unit
  in
  loop reader writer handle_request
;;

let respond ?(headers = Http.Header.init ()) ?(body = Body.Writer.empty) status =
  let encoding = Body.Writer.encoding body in
  let resp = Http.Response.make ~status ~encoding ~headers () in
  return (resp, body)
;;

let respond_string ?headers ?(status = `OK) body =
  respond ?headers ~body:(Body.Writer.string body) status
;;

let respond_stream ?headers ?(status = `OK) body =
  respond ?headers ~body:(Body.Writer.stream body) status
;;
