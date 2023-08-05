open! Core
open! Async
open! Shuttle_http

let default_error_handler ?exn:_ ?request:_ status =
  return
    (Response.create
       ~headers:[ "Connection", "close"; "Content-Length", "0" ]
       ~body:Body.empty
       status)
;;

module Config = struct
  type t =
    { error_handler : Server.error_handler
    ; subprotocol : string list -> string option
    ; response_headers : (string * string) list
    ; opcode : [ `Text | `Binary ] option
    ; buffer_size : int
    }

  let create
    ?(buffer_size = 0x4000)
    ?(error_handler = default_error_handler)
    ?(response_headers = [])
    ?opcode
    ?(subprotocol = fun _ -> None)
    ()
    =
    { buffer_size; error_handler; opcode; response_headers; subprotocol }
  ;;

  let default = create ()
end

let create ?(config = Config.default) websocket_handler request =
  match Request.meth request with
  | `GET ->
    (match Request.header request "Sec-WebSocket-Key" with
     | None -> config.error_handler ~request `Bad_request
     | Some v ->
       let accept_key =
         Websocket.sec_websocket_accept_header_value ~sec_websocket_key:v
       in
       let subprotocol =
         match Request.header_multi request "Sec-WebSocket-Protocol" with
         | [] -> None
         | xs ->
           let protocols =
             List.concat_map xs ~f:(fun str ->
               str |> String.split ~on:',' |> List.map ~f:String.strip)
           in
           config.subprotocol protocols
       in
       let handler ?unconsumed_data fd =
         match unconsumed_data with
         | Some payload ->
           raise_s
             [%message
               "Websocket upgrade request contained unconsumed data" ~data:payload]
         | None ->
           let reader = Reader.create ~buf_len:config.buffer_size fd in
           let writer = Writer.create ~buf_len:config.buffer_size fd in
           let ws =
             Websocket.create
               ?opcode:config.opcode
               ~role:Websocket.Websocket_role.Server
               reader
               writer
           in
           websocket_handler ws
       in
       let headers =
         config.response_headers
         |> Headers.replace ~key:"Upgrade" ~data:"WebSocket"
         |> Headers.replace ~key:"Connection" ~data:"Upgrade"
         |> Headers.replace ~key:"Sec-WebSocket-Accept" ~data:accept_key
         |> fun headers ->
         match subprotocol with
         | None -> headers
         | Some protocol ->
           Headers.replace headers ~key:"Sec-WebSocket-Protocol" ~data:protocol
       in
       return (Response.upgrade ~headers handler))
  | _ -> config.error_handler ~request `Method_not_allowed
;;
