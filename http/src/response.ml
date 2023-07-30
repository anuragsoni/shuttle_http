open Core
include Response0

let create
  ?(version = Version.Http_1_1)
  ?reason_phrase
  ?(headers = [])
  ?(body = Body.empty)
  status
  =
  let reason_phrase = Option.value reason_phrase ~default:(Status.to_string status) in
  { version; status; reason_phrase; headers; body = Response body }
;;

let upgrade ?(headers = []) handler =
  let reason_phrase = Status.to_reason_phrase `Switching_protocols in
  { version = Http_1_1
  ; status = `Switching_protocols
  ; reason_phrase
  ; headers
  ; body = Upgrade handler
  }
;;

let version t = t.version
let status t = t.status
let reason_phrase t = t.reason_phrase
let headers t = t.headers

let body t =
  match t.body with
  | Response b -> b
  | Upgrade _ -> Body.empty
;;

let transfer_encoding t =
  match List.rev @@ Headers.find_multi t.headers "Transfer-Encoding" with
  | x :: _ when String.Caseless.equal x "chunked" -> `Chunked
  | _x :: _ -> `Bad_response
  | [] ->
    (match
       List.dedup_and_sort
         ~compare:String.Caseless.compare
         (Headers.find_multi t.headers "Content-Length")
     with
     | [] -> `Fixed 0
     | [ x ] ->
       let len =
         try Int.of_string x with
         | _ -> -1
       in
       if Int.(len >= 0) then `Fixed len else `Bad_response
     | _ -> `Bad_response)
;;

let keep_alive t =
  match Headers.find t.headers "connection" with
  | Some x when String.Caseless.equal x "close" -> false
  | _ -> true
;;

let add_transfer_encoding t encoding =
  match encoding with
  | `Chunked ->
    if Headers.mem t.headers "Transfer-Encoding"
    then t
    else
      { t with headers = Headers.add t.headers ~key:"Transfer-Encoding" ~data:"chunked" }
  | `Fixed len ->
    if Headers.mem t.headers "Content-Length"
    then t
    else
      { t with
        headers = Headers.add t.headers ~key:"Content-Length" ~data:(Int.to_string len)
      }
;;

let iter_headers t ~f = Headers.iter t.headers ~f

let add_header_unless_exists t ~key ~data =
  if Headers.mem t.headers key
  then t
  else { t with headers = Headers.add t.headers ~key ~data }
;;

let add_header t ~key ~data = { t with headers = Headers.add t.headers ~key ~data }
let header_exists t key = Headers.mem t.headers key

let remove_header t key =
  if Headers.mem t.headers key
  then { t with headers = Headers.remove t.headers key }
  else t
;;

let header_multi t name = Headers.find_multi t.headers name
let header t name = Headers.find t.headers name
