open Core
open Async_kernel
open Async_unix
open! Async_kernel_require_explicit_time_source

type t =
  { fd : Fd.t
  ; mutable is_closed : bool
  ; closed : unit Ivar.t
  ; buf : Bytebuffer.t
  ; time_source : Time_source.t
  }
[@@deriving sexp_of]

let create ?max_buffer_size ?buf_len ?time_source fd =
  Fd.with_file_descr_exn fd ignore ~nonblocking:true;
  let time_source =
    match time_source with
    | None -> Time_source.wall_clock ()
    | Some t -> Time_source.read_only t
  in
  let buf_len =
    match buf_len with
    | None -> 64 * 1024
    | Some buf_len ->
      if buf_len > 0
      then buf_len
      else
        raise_s
          [%message "Reader.create got negative buf_len" (buf_len : int) (fd : Fd.t)]
  in
  { fd
  ; is_closed = false
  ; closed = Ivar.create ()
  ; buf = Bytebuffer.create ?max_buffer_size buf_len
  ; time_source
  }
;;

let consume t n = Bytebuffer.drop t.buf n
let is_closed t = t.is_closed
let closed t = Ivar.read t.closed

let close t =
  if not t.is_closed
  then (
    t.is_closed <- true;
    Fd.close t.fd >>> fun () -> Ivar.fill_exn t.closed ());
  closed t
;;

let unread_bytes t = Bytebuffer.length t.buf
let fd t = t.fd

exception Timeout

let refill_with_timeout t span =
  Bytebuffer.compact t.buf;
  if Bytebuffer.available_to_write t.buf = 0 then Bytebuffer.ensure_space t.buf 1;
  let result = Bytebuffer.read_assume_fd_is_nonblocking t.buf (Fd.file_descr_exn t.fd) in
  if Unix.Syscall_result.Int.is_ok result
  then (
    match Unix.Syscall_result.Int.ok_exn result with
    | 0 -> return `Eof
    | n ->
      assert (n > 0);
      return `Ok)
  else (
    match Unix.Syscall_result.Int.error_exn result with
    | EAGAIN | EWOULDBLOCK | EINTR ->
      let event = Time_source.Event.after t.time_source span in
      let interrupt =
        match%bind Time_source.Event.fired event with
        | Time_source.Event.Fired.Aborted () -> Deferred.never ()
        | Time_source.Event.Fired.Happened () -> Deferred.unit
      in
      let rec loop t =
        Fd.interruptible_ready_to ~interrupt t.fd `Read
        >>= function
        | `Interrupted ->
          (match Time_source.Event.abort event () with
           | Time_source.Event.Abort_result.Previously_happened () -> raise Timeout
           | Ok | Previously_aborted () ->
             raise_s
               [%message
                 "Input_channel.refill_with_timeout bug. Timeout event can't be aborted \
                  if Fd is interrupted"])
        | `Ready ->
          let result =
            Bytebuffer.read_assume_fd_is_nonblocking t.buf (Fd.file_descr_exn t.fd)
          in
          if Unix.Syscall_result.Int.is_ok result
          then (
            match Unix.Syscall_result.Int.ok_exn result with
            | 0 ->
              Time_source.Event.abort_if_possible event ();
              return `Eof
            | n ->
              assert (n > 0);
              Time_source.Event.abort_if_possible event ();
              return `Ok)
          else (
            match Unix.Syscall_result.Int.error_exn result with
            | EAGAIN | EWOULDBLOCK | EINTR -> loop t
            | EPIPE
            | ECONNRESET
            | EHOSTUNREACH
            | ENETDOWN
            | ENETRESET
            | ENETUNREACH
            | ETIMEDOUT ->
              Time_source.Event.abort_if_possible event ();
              return `Eof
            | error ->
              Time_source.Event.abort_if_possible event ();
              raise (Unix.Unix_error (error, "read", "")))
        | `Closed ->
          Time_source.Event.abort_if_possible event ();
          return `Eof
        | `Bad_fd ->
          let%bind () = close t in
          Time_source.Event.abort_if_possible event ();
          raise_s
            [%message
              "Shuttle.Input_channel.refill_with_timeout: bad file descriptor"
                ~fd:(t.fd : Fd.t)]
      in
      loop t
    | EPIPE | ECONNRESET | EHOSTUNREACH | ENETDOWN | ENETRESET | ENETUNREACH | ETIMEDOUT
      -> return `Eof
    | error -> raise (Unix.Unix_error (error, "read", "")))
;;

let refill t =
  Bytebuffer.compact t.buf;
  if Bytebuffer.available_to_write t.buf = 0 then Bytebuffer.ensure_space t.buf 1;
  let result = Bytebuffer.read_assume_fd_is_nonblocking t.buf (Fd.file_descr_exn t.fd) in
  if Unix.Syscall_result.Int.is_ok result
  then (
    match Unix.Syscall_result.Int.ok_exn result with
    | 0 -> return `Eof
    | n ->
      assert (n > 0);
      return `Ok)
  else (
    match Unix.Syscall_result.Int.error_exn result with
    | EAGAIN | EWOULDBLOCK | EINTR ->
      let rec loop t =
        Fd.ready_to t.fd `Read
        >>= function
        | `Ready ->
          let result =
            Bytebuffer.read_assume_fd_is_nonblocking t.buf (Fd.file_descr_exn t.fd)
          in
          if Unix.Syscall_result.Int.is_ok result
          then (
            match Unix.Syscall_result.Int.ok_exn result with
            | 0 -> return `Eof
            | n ->
              assert (n > 0);
              return `Ok)
          else (
            match Unix.Syscall_result.Int.error_exn result with
            | EAGAIN | EWOULDBLOCK | EINTR -> loop t
            | EPIPE
            | ECONNRESET
            | EHOSTUNREACH
            | ENETDOWN
            | ENETRESET
            | ENETUNREACH
            | ETIMEDOUT -> return `Eof
            | error -> raise (Unix.Unix_error (error, "read", "")))
        | `Closed -> return `Eof
        | `Bad_fd ->
          raise_s
            [%message "Shuttle.Input_channel.read: bad file descriptor" ~fd:(t.fd : Fd.t)]
      in
      loop t
    | EPIPE | ECONNRESET | EHOSTUNREACH | ENETDOWN | ENETRESET | ENETUNREACH | ETIMEDOUT
      -> return `Eof
    | error -> raise (Unix.Unix_error (error, "read", "")))
;;

let view t = Bytebuffer.unsafe_peek t.buf

let transfer t writer =
  let finished = Ivar.create () in
  upon (Pipe.closed writer) (fun () -> Ivar.fill_if_empty finished ());
  let rec loop () =
    refill t
    >>> function
    | `Eof -> Ivar.fill_if_empty finished ()
    | `Ok ->
      let payload = Bytebuffer.to_string t.buf in
      Bytebuffer.drop t.buf (String.length payload);
      Pipe.write writer payload >>> fun () -> loop ()
  in
  loop ();
  Ivar.read finished
;;

let pipe t =
  let reader, writer = Pipe.create () in
  (transfer t writer >>> fun () -> close t >>> fun () -> Pipe.close writer);
  reader
;;

let drain t = Pipe.drain (pipe t)
