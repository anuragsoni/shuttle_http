open! Core
open! Async_kernel
open Async_unix
open Import
module Unix = Core.Unix

module Config = struct
  (* Same as the default value of [buffer_age_limit] for [Async_unix.Writer] *)
  let default_write_timeout = Time_ns.Span.of_min 2.
  let default_max_buffer_size = Int.max_value
  let default_initial_buffer_size = 64 * 1024

  type t =
    { initial_buffer_size : int
    ; max_buffer_size : int
    ; write_timeout : Time_ns.Span.t
    }
  [@@deriving sexp_of]

  let validate t =
    if t.initial_buffer_size <= 0
       || t.initial_buffer_size > t.max_buffer_size
       || Time_ns.Span.( <= ) t.write_timeout Time_ns.Span.zero
    then raise_s [%sexp "Shuttle.Config.validate: invalid config", { t : t }];
    t
  ;;

  let create
      ?(initial_buffer_size = default_initial_buffer_size)
      ?(max_buffer_size = default_max_buffer_size)
      ?(write_timeout = default_write_timeout)
      ()
    =
    validate { initial_buffer_size; max_buffer_size; write_timeout }
  ;;
end

type flush =
  { pos : Int63.t
  ; ivar : unit Ivar.t
  }
[@@deriving sexp_of]

type t =
  { fd : Fd.t
  ; config : Config.t
  ; buf : Bytebuffer.t
  ; monitor : Monitor.t
  ; flushes : flush Queue.t
  ; mutable close_state : [ `Open | `Start_close | `Closed ]
  ; close_started : unit Ivar.t
  ; close_finished : unit Ivar.t
  ; mutable writer_state : [ `Active | `Stopped | `Inactive ]
  ; mutable bytes_written : Int63.t
  }
[@@deriving sexp_of, fields]

let create ?initial_buffer_size ?max_buffer_size ?write_timeout fd =
  let config = Config.create ?initial_buffer_size ?max_buffer_size ?write_timeout () in
  set_nonblock fd;
  { fd
  ; config
  ; flushes = Queue.create ()
  ; writer_state = `Inactive
  ; buf = Bytebuffer.create config.initial_buffer_size
  ; monitor = Monitor.create ()
  ; close_state = `Open
  ; close_started = Ivar.create ()
  ; close_finished = Ivar.create ()
  ; bytes_written = Int63.zero
  }
;;

let is_closed t =
  match t.close_state with
  | `Open -> false
  | `Closed | `Start_close -> true
;;

let close_started t = Ivar.read t.close_started
let close_finished t = Ivar.read t.close_finished
let is_open = Fn.non is_closed

let flushed t =
  if Bytebuffer.length t.buf = 0
  then Deferred.unit
  else if is_closed t
  then Deferred.never ()
  else (
    let flush =
      { pos = Int63.( + ) t.bytes_written (Int63.of_int (Bytebuffer.length t.buf))
      ; ivar = Ivar.create ()
      }
    in
    Queue.enqueue t.flushes flush;
    Ivar.read flush.ivar)
;;

let dequeue_flushes t =
  while
    (not (Queue.is_empty t.flushes))
    && Int63.( <= ) (Queue.peek_exn t.flushes).pos t.bytes_written
  do
    Ivar.fill (Queue.dequeue_exn t.flushes).ivar ()
  done
;;

let write t =
  match Bytebuffer.write_assume_fd_is_nonblocking (Fd.file_descr_exn t.fd) t.buf with
  | n ->
    assert (n >= 0);
    Bytebuffer.compact t.buf;
    t.bytes_written <- Int63.( + ) t.bytes_written (Int63.of_int n);
    dequeue_flushes t;
    `Ok
  | exception Unix.Unix_error ((EWOULDBLOCK | EAGAIN | EINTR), _, _) -> `Ok
  | exception
      Unix.Unix_error
        ( ( EPIPE
          | ECONNRESET
          | EHOSTUNREACH
          | ENETDOWN
          | ENETRESET
          | ENETUNREACH
          | ETIMEDOUT )
        , _
        , _ ) -> `Eof
  | exception exn -> raise exn
;;

let close t =
  (match t.close_state with
  | `Closed | `Start_close -> ()
  | `Open ->
    t.close_state <- `Start_close;
    Ivar.fill t.close_started ();
    Deferred.any_unit [ after (Time.Span.of_sec 5.); flushed t ]
    >>> fun () ->
    t.close_state <- `Closed;
    Fd.close t.fd >>> fun () -> Ivar.fill t.close_finished ());
  close_finished t
;;

let stop_writer t = t.writer_state <- `Stopped

module Single_write_result = struct
  type t =
    | Continue
    | Stop
end

let single_write t =
  (* match Faraday.operation t.buf with *)
  if Bytebuffer.length t.buf > 0
  then (
    match write t with
    | `Ok -> Single_write_result.Continue
    | `Eof -> Stop)
  else Continue
;;

let rec write_everything t =
  match single_write t with
  | Stop -> stop_writer t
  | Continue ->
    if not (Bytebuffer.length t.buf > 0)
    then (
      t.writer_state <- `Inactive;
      if is_closed t then stop_writer t)
    else wait_and_write_everything t

and wait_and_write_everything t =
  Clock_ns.with_timeout t.config.write_timeout (Fd.ready_to t.fd `Write)
  >>> fun result ->
  match result with
  | `Result `Ready -> write_everything t
  | `Timeout ->
    Log.Global.sexp
      ~level:`Error
      [%message
        "Async_transport.Writer timed out waiting to write on file descriptor. Closing \
         the writer."
          ~timeout:(t.config.write_timeout : Time_ns.Span.t)
          (t : t)];
    stop_writer t
  | `Result ((`Bad_fd | `Closed) as result) ->
    raise_s
      [%sexp
        "Async_transport.Writer: fd changed"
        , { t : t; ready_to_result = (result : [ `Bad_fd | `Closed ]) }]
;;

let is_writing t =
  match t.writer_state with
  | `Active -> true
  | `Inactive -> false
  | `Stopped -> false
;;

let flush t =
  if (not (is_writing t)) && Bytebuffer.length t.buf > 0
  then (
    t.writer_state <- `Active;
    Scheduler.within ~monitor:t.monitor (fun () -> write_everything t))
;;

let ensure_can_write t =
  match t.writer_state with
  | `Inactive | `Active -> ()
  | `Stopped -> raise_s [%sexp "Attempting to write to a closed writer", { t : t }]
;;

let schedule_bigstring t ?pos ?len buf =
  ensure_can_write t;
  Bytebuffer.Fill.bigstring t.buf buf ?pos ?len
;;

let write_string t ?pos ?len buf =
  ensure_can_write t;
  Bytebuffer.Fill.string t.buf buf ?pos ?len
;;
