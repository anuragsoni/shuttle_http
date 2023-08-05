open! Core
open! Async
open! Shuttle_http

let create ?(buffer_size = 0x4000) ?opcode handler request =
  match Request.meth request with
  | `GET ->
    (match Request.header request "Sec-WebSocket-Key" with
     | None -> return (Response.create `Bad_request)
     | Some v ->
       let accept_key =
         Websocket.sec_websocket_accept_header_value ~sec_websocket_key:v
       in
       let handler (upgrade_context : Upgrade_context.t) =
         match upgrade_context.unconsumed_data with
         | Some payload ->
           raise_s
             [%message
               "Websocket upgrade request contained unconsumed data" ~data:payload]
         | None ->
           let reader = Reader.create ~buf_len:buffer_size upgrade_context.fd in
           let writer = Writer.create ~buf_len:buffer_size upgrade_context.fd in
           let ws =
             Websocket.create ?opcode ~role:Websocket.Websocket_role.Server reader writer
           in
           handler ws
       in
       return
         (Response.upgrade
            ~headers:
              [ "Upgrade", "websocket"
              ; "Connection", "Upgrade"
              ; "Sec-WebSocket-Accept", accept_key
              ]
            handler))
  | _ -> return (Response.create `Method_not_allowed)
;;
