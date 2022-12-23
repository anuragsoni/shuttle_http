open! Core
open! Async_kernel
open Async_unix
module Unix = Core_unix
open! Async_kernel_require_explicit_time_source
module Logger = Log.Make_global ()

module Flush_result = struct
  type t =
    | Flushed
    | Remote_closed
    | Error
  [@@deriving sexp_of]
end

type flush =
  { pos : Int63.t
  ; ivar : Flush_result.t Ivar.t
  }
[@@deriving sexp_of]

type close_state =
  | Open
  | Start_close
  | Closed
[@@deriving sexp_of]

type writer_state =
  | Active
  | Stopped
  | Inactive
[@@deriving sexp_of]

type t =
  { fd : Fd.t
  ; write_timeout : Time_ns.Span.t
  ; buf : Bytebuffer.t
  ; monitor : Monitor.t
  ; flushes : flush Queue.t
  ; mutable close_state : close_state
  ; close_started : unit Ivar.t
  ; close_finished : unit Ivar.t
  ; remote_closed : unit Ivar.t
  ; mutable writer_state : writer_state
  ; mutable bytes_written : Int63.t
  ; time_source : Time_source.t
  }
[@@deriving sexp_of]

let default_write_timeout = Time_ns.Span.of_min 2.

let create ?max_buffer_size ?buf_len ?write_timeout ?time_source fd =
  Fd.with_file_descr_exn fd ignore ~nonblocking:true;
  let buf_len =
    match buf_len with
    | None -> 64 * 1024
    | Some v -> v
  in
  if buf_len <= 0 then raise_s [%sexp "Buffer size must be greater than 0"];
  let time_source =
    match time_source with
    | None -> Time_source.wall_clock ()
    | Some t -> Time_source.read_only t
  in
  let write_timeout =
    match write_timeout with
    | Some v -> v
    | None -> default_write_timeout
  in
  if Time_ns.Span.( <= ) write_timeout Time_ns.Span.zero
  then raise_s [%message "Write timeout cannot be less than zero"];
  { fd
  ; flushes = Queue.create ()
  ; write_timeout
  ; writer_state = Inactive
  ; buf = Bytebuffer.create ?max_buffer_size buf_len
  ; monitor = Monitor.create ()
  ; close_state = Open
  ; remote_closed = Ivar.create ()
  ; close_started = Ivar.create ()
  ; close_finished = Ivar.create ()
  ; bytes_written = Int63.zero
  ; time_source
  }
;;

let wakeup_flushes_with_error t error =
  while not (Queue.is_empty t.flushes) do
    Ivar.fill (Queue.dequeue_exn t.flushes).ivar error
  done
;;

let is_closed t =
  match t.close_state with
  | Open -> false
  | Closed | Start_close -> true
;;

let flushed_or_fail t =
  if Bytebuffer.length t.buf = 0
  then return Flush_result.Flushed
  else if is_closed t
  then return Flush_result.Error
  else (
    let flush =
      { pos = Int63.( + ) t.bytes_written (Int63.of_int (Bytebuffer.length t.buf))
      ; ivar = Ivar.create ()
      }
    in
    Queue.enqueue t.flushes flush;
    Ivar.read flush.ivar)
;;

let flushed t =
  match%bind flushed_or_fail t with
  | Flush_result.Flushed -> Deferred.unit
  | Error | Remote_closed -> Deferred.never ()
;;

let close_started t = Ivar.read t.close_started
let close_finished t = Ivar.read t.close_finished

let close' t =
  match t.close_state with
  | Closed | Start_close -> ()
  | Open ->
    t.close_state <- Start_close;
    Ivar.fill t.close_started ();
    Deferred.any_unit
      [ Time_source.after t.time_source (Time_ns.Span.of_sec 5.)
      ; Deferred.ignore_m (flushed_or_fail t)
      ]
    >>> fun () ->
    t.close_state <- Closed;
    Fd.close t.fd >>> fun () -> Ivar.fill t.close_finished ()
;;

let close t =
  close' t;
  close_finished t
;;

let stop_writer t reason =
  wakeup_flushes_with_error t reason;
  t.writer_state <- Stopped;
  close' t
;;

let monitor t = t.monitor
let remote_closed t = Ivar.read t.remote_closed
let is_open = Fn.non is_closed

let dequeue_flushes t =
  while
    (not (Queue.is_empty t.flushes))
    && Int63.( <= ) (Queue.peek_exn t.flushes).pos t.bytes_written
  do
    Ivar.fill (Queue.dequeue_exn t.flushes).ivar Flush_result.Flushed
  done
;;

let write_nonblocking t =
  match Bytebuffer.write_assume_fd_is_nonblocking t.buf (Fd.file_descr_exn t.fd) with
  | n ->
    assert (n >= 0);
    `Ok n
  | exception Unix.Unix_error ((EWOULDBLOCK | EAGAIN | EINTR), _, _) -> `Poll_again
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
  | exception exn ->
    stop_writer t Flush_result.Error;
    raise exn
;;

let rec write_everything t =
  if Bytebuffer.length t.buf <= 0
  then t.writer_state <- Inactive
  else (
    match write_nonblocking t with
    | `Eof ->
      Ivar.fill t.remote_closed ();
      stop_writer t Flush_result.Remote_closed
    | `Poll_again -> wait_and_write_everything t
    | `Ok n ->
      Bytebuffer.compact t.buf;
      t.bytes_written <- Int63.( + ) t.bytes_written (Int63.of_int n);
      dequeue_flushes t;
      if Bytebuffer.length t.buf <= 0
      then t.writer_state <- Inactive
      else wait_and_write_everything t)

and wait_and_write_everything t =
  Time_source.with_timeout t.time_source t.write_timeout (Fd.ready_to t.fd `Write)
  >>> fun result ->
  match result with
  | `Result `Ready -> write_everything t
  | `Timeout ->
    Logger.sexp
      ~level:`Error
      [%message
        "Shuttle.Output_channel timed out waiting to write on file descriptor. Closing \
         the writer."
          ~timeout:(t.write_timeout : Time_ns.Span.t)
          (t : t)];
    stop_writer t Flush_result.Error
  | `Result ((`Bad_fd | `Closed) as result) ->
    stop_writer t Flush_result.Error;
    raise_s
      [%sexp
        "Shuttle.Output_channel: fd changed"
        , { t : t; ready_to_result = (result : [ `Bad_fd | `Closed ]) }]
;;

let is_writing t =
  match t.writer_state with
  | Active -> true
  | Inactive -> false
  | Stopped -> false
;;

let schedule_flush t =
  if (not (is_writing t)) && Bytebuffer.length t.buf > 0
  then (
    t.writer_state <- Active;
    Scheduler.within ~monitor:t.monitor (fun () -> write_everything t))
;;

let flush t =
  let flush_result = flushed t in
  schedule_flush t;
  flush_result
;;

let flush_or_fail t =
  let flush_result = flushed_or_fail t in
  schedule_flush t;
  flush_result
;;

let ensure_can_write t =
  match t.writer_state with
  | Inactive | Active -> ()
  | Stopped -> raise_s [%sexp "Attempting to write to a closed writer", { t : t }]
;;

let can_write t =
  match t.writer_state with
  | Inactive | Active -> true
  | Stopped -> false
;;

let write_bigstring t ?pos ?len buf =
  ensure_can_write t;
  Bytebuffer.add_bigstring t.buf buf ?pos ?len
;;

let schedule_bigstring t ?pos ?len buf = write_bigstring t ?pos ?len buf

let write t ?pos ?len buf =
  ensure_can_write t;
  Bytebuffer.add_string t.buf buf ?pos ?len
;;

let write_string t ?pos ?len buf = write t ?pos ?len buf
let writef t fmt = ksprintf (fun str -> write t str) fmt

let write_char t ch =
  ensure_can_write t;
  Bytebuffer.add_char t.buf ch
;;

let write_from_pipe t reader =
  let finished = Ivar.create () in
  let consumer =
    (* Add a consumer so the pipe will take the output_channel into account when it checks
       if the reader contents have been flushed. *)
    Pipe.add_consumer reader ~downstream_flushed:(fun () ->
      let%map () = flushed t in
      `Ok)
  in
  let rec loop () =
    if can_write t && is_open t
    then (
      (* use [read_now'] as [iter] doesn't allow working on chunks of values at a time. *)
      match Pipe.read_now' ~consumer reader with
      | `Eof -> Ivar.fill finished ()
      | `Nothing_available -> Pipe.values_available reader >>> fun _ -> loop ()
      | `Ok bufs ->
        Queue.iter bufs ~f:(fun buf -> write t buf);
        schedule_flush t;
        Pipe.Consumer.values_sent_downstream consumer;
        flushed t >>> loop)
  in
  loop ();
  choose
    [ choice (Ivar.read finished) (fun () -> `Finished)
    ; choice (close_finished t) (fun () -> `Closed)
    ]
  >>| function
  | `Finished -> ()
  | `Closed ->
    (* Close the pipe (both read and write ends) since the channel is closed. This is
       desirable so all future calls to [Pipe.write] fail. *)
    Pipe.close_read reader
;;

let pipe t =
  let reader, writer = Pipe.create () in
  don't_wait_for (write_from_pipe t reader);
  writer
;;
