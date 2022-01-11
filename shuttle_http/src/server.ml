open! Core
open! Async
open! Shuttle

type response = Cohttp.Response.t * Body.t [@@deriving sexp_of]

type response_action =
  [ `Expert of
    Cohttp.Response.t * (Input_channel.t -> Output_channel.t -> unit Deferred.t)
  | `Response of response
  ]

type 'r respond_t =
  ?flush:bool
  -> ?headers:Cohttp.Header.t
  -> ?body:Body.t
  -> Cohttp.Code.status_code
  -> 'r Deferred.t

let read_body req rd =
  match Io.Request.has_body req with
  (* TODO maybe attempt to read body *)
  | `No | `Unknown -> `Empty
  | `Yes ->
    (* Create a Pipe for the body *)
    let reader = Io.Request.make_body_reader req rd in
    let pipe = Body.Private.pipe_of_body Io.Request.read_body_chunk reader in
    `Pipe pipe
;;

let collect_errors writer ~f =
  let monitor = Output_channel.monitor writer in
  (* don't propagate errors up, we handle them here *)
  Monitor.detach_and_get_error_stream monitor |> (ignore : exn Stream.t -> unit);
  choose
    [ choice (Monitor.get_next_error monitor) (fun e ->
          Error (Exn.Reraised ("Cohttp_async.Server.collect_errors", e)))
    ; choice (try_with ~name:"Cohttp_async.Server.collect_errors" f) Fn.id
    ]
;;

let handle_client handle_request sock rd wr =
  collect_errors wr ~f:(fun () ->
      let rec loop rd wr sock handle_request =
        if Input_channel.is_closed rd
        then Deferred.unit
        else
          Io.Request.read rd
          >>= function
          | `Eof | `Invalid _ -> Deferred.unit
          | `Ok req ->
            let req_body = read_body req rd in
            handle_request ~body:req_body sock req
            >>= (function
            | `Expert (res, handler) ->
              Io.Response.write_header res wr
              >>= fun () -> handler rd wr >>= fun () -> loop rd wr sock handle_request
            | `Response (res, res_body) ->
              let keep_alive = Cohttp.Request.is_keep_alive req in
              let flush = Cohttp.Response.flush res in
              let res =
                let headers =
                  Cohttp.Header.add_unless_exists
                    (Cohttp.Response.headers res)
                    "connection"
                    (if keep_alive then "keep-alive" else "close")
                in
                { res with Cohttp.Response.headers }
              in
              Io.Response.write_header res wr
              >>= fun () ->
              Output_channel.schedule_flush wr;
              let writer = Io.Response.make_body_writer ~flush res wr in
              (Body.Private.write_body Io.Response.write_body res_body) writer
              >>= fun () ->
              Body.Private.drain req_body
              >>= fun () ->
              if keep_alive then loop rd wr sock handle_request else Deferred.unit)
      in
      loop rd wr sock handle_request)
  >>| fun res -> Result.ok_exn res
;;

let create
    ?max_connections
    ?max_accepts_per_batch
    ?backlog
    where_to_connect
    handle_request
  =
  Connection.listen
    ?max_connections
    ?max_accepts_per_batch
    ?backlog
    ~input_buffer_size:0x4000
    ~output_buffer_size:0x4000
    ~on_handler_error:`Raise
    ~f:(fun addr ic oc -> handle_client handle_request addr ic oc)
    where_to_connect
;;

let respond ?(flush = true) ?(headers = Cohttp.Header.init ()) ?(body = `Empty) status =
  let encoding = Body.transfer_encoding body in
  let resp = Cohttp.Response.make ~status ~flush ~encoding ~headers () in
  return (`Response (resp, body))
;;

let respond_string ?flush ?headers ?(status = `OK) body =
  respond ?flush ?headers ~body:(`String body) status
;;
