open! Core
open! Async
open! Shuttle

module IO = struct
  module Deferred = Deferred
  module Reader = Input_channel

  module Writer = struct
    include Output_channel

    let write t buf = write t buf
  end
end

module Server = H11.Server.Make (IO)

module Connection = struct
  type 'ctx t =
    { conn : Server.Connection.t
    ; ctx : 'ctx
    }

  let user_context t = t.ctx

  type response = Server.Connection.response

  let request t = Server.Connection.request t.conn

  let request_body t =
    Pipe.create_reader ~close_on_exception:false (fun writer ->
        Server.Body.Reader.iter (Server.Connection.request_body t.conn) ~f:(fun chunk ->
            Pipe.write_if_open writer chunk))
  ;;

  let make_handler ctx handler conn =
    let ctx = { conn; ctx } in
    handler ctx
  ;;

  let respond_with_string t response buf =
    Server.Connection.respond_with_string t.conn response buf
  ;;

  let respond_with_stream t response reader =
    let body = Server.Body.Reader.create (fun () -> Pipe.read reader) in
    Server.Connection.respond_with_stream t.conn response body
  ;;
end

let run addr reader writer handler error_handler =
  Server.run reader writer (Connection.make_handler addr handler) error_handler
;;
