open Core
open Async

let set_nonblock fd = Fd.with_file_descr_exn fd ignore ~nonblocking:true

module Read_chunk_result = struct
  type 'a t =
    | Stop of 'a
    | Continue
    | Consumed of int
  [@@deriving sexp_of]
end

type t =
  { fd : Fd.t
  ; mutable reading : bool
  ; mutable is_closed : bool
  ; closed : unit Ivar.t
  ; mutable buf : (Bigstring.t[@sexp.opaque])
  ; mutable pos : int
  ; mutable max : int
  }
[@@deriving sexp_of]

let create ?(buf_len = 64 * 1024) fd =
  let buf_len =
    if buf_len > 0
    then buf_len
    else
      raise_s [%message "Reader.create got negative buf_len" (buf_len : int) (fd : Fd.t)]
  in
  set_nonblock fd;
  { fd
  ; reading = false
  ; is_closed = false
  ; closed = Ivar.create ()
  ; buf = Bigstring.create buf_len
  ; pos = 0
  ; max = 0
  }
;;

let is_closed t = t.is_closed
let closed t = Ivar.read t.closed

let close t =
  if not t.is_closed
  then (
    t.is_closed <- true;
    Fd.close t.fd >>> fun () -> Ivar.fill t.closed ());
  closed t
;;

let shift t =
  if t.pos > 0
  then (
    let len = t.max - t.pos in
    if len > 0 then Bigstring.blit ~src:t.buf ~dst:t.buf ~src_pos:t.pos ~dst_pos:0 ~len;
    t.pos <- 0;
    t.max <- len)
;;

let refill t =
  shift t;
  let result =
    Bigstring_unix.read_assume_fd_is_nonblocking
      (Fd.file_descr_exn t.fd)
      t.buf
      ~pos:t.max
      ~len:(Bigstring.length t.buf - t.max)
  in
  if Unix.Syscall_result.Int.is_ok result
  then (
    match Unix.Syscall_result.Int.ok_exn result with
    | 0 -> `Eof
    | n ->
      assert (n > 0);
      t.max <- t.max + n;
      `Read_some)
  else (
    match Unix.Syscall_result.Int.error_exn result with
    | EAGAIN | EWOULDBLOCK | EINTR -> `Nothing_available
    | EPIPE | ECONNRESET | EHOSTUNREACH | ENETDOWN | ENETRESET | ENETUNREACH | ETIMEDOUT
      -> `Eof
    | error -> raise (Unix.Unix_error (error, "read", "")))
;;

module Driver = struct
  type 'a state =
    | Running
    | Stopped of 'a stop_reason

  and 'a stop_reason =
    | Handler_raised
    | Eof_reached
    | Stopped_by_user of 'a

  type nonrec 'a t =
    { reader : t
    ; on_chunk : Bigstring.t -> pos:int -> len:int -> 'a Read_chunk_result.t
    ; interrupt : unit Ivar.t
    ; mutable state : 'a state
    }

  let is_running t =
    match t.state with
    | Running -> true
    | Stopped _ -> false
  ;;

  let interrupt t reason =
    assert (is_running t);
    t.state <- Stopped reason;
    Ivar.fill t.interrupt ()
  ;;

  let can_process_chunk t = (not t.reader.is_closed) && is_running t

  let rec process_chunks t =
    if can_process_chunk t
    then (
      let len = t.reader.max - t.reader.pos in
      if len > 0
      then (
        match t.on_chunk t.reader.buf ~pos:t.reader.pos ~len with
        | Stop x -> interrupt t (Stopped_by_user x)
        | Continue -> t.reader.pos <- t.reader.pos + len
        | Consumed d ->
          if d > len || d < 0
          then
            raise_s
              [%message
                "on_chunk returned an invalid value for consumed bytes"
                  (len : int)
                  ~consumed:(d : int)];
          t.reader.pos <- t.reader.pos + d;
          process_chunks t))
  ;;

  let process_incoming t =
    if can_process_chunk t
    then (
      match refill t.reader with
      | `Eof -> interrupt t Eof_reached
      | `Nothing_available -> ()
      | `Read_some -> process_chunks t)
  ;;

  let stop_watching_on_error t ~monitor =
    let parent = Monitor.current () in
    Monitor.detach_and_iter_errors monitor ~f:(fun exn ->
        if is_running t then interrupt t Handler_raised;
        Monitor.send_exn parent exn)
  ;;

  let run reader ~on_chunk =
    let t = { reader; interrupt = Ivar.create (); state = Running; on_chunk } in
    let monitor =
      Monitor.create ~here:[%here] ~name:"Async_transport.Reader.Driver.run" ()
    in
    stop_watching_on_error t ~monitor;
    match%bind
      Scheduler.within' ~monitor (fun () ->
          let interrupt = Deferred.any_unit [ Ivar.read t.interrupt; closed t.reader ] in
          Fd.interruptible_every_ready_to ~interrupt t.reader.fd `Read process_incoming t)
    with
    | `Bad_fd | `Unsupported ->
      raise_s
        [%message
          "Async_transport.Reader.run: fd doesn't support watching"
            ~fd:(t.reader.fd : Fd.t)]
    | `Closed | `Interrupted ->
      (match t.state with
      | Running ->
        assert (Fd.is_closed t.reader.fd || t.reader.is_closed);
        return (Error `Closed)
      | Stopped (Stopped_by_user x) -> return (Ok x)
      | Stopped Handler_raised -> Deferred.never ()
      | Stopped Eof_reached -> return (Error `Eof))
  ;;
end

let read_one_chunk_at_a_time t ~on_chunk = Driver.run t ~on_chunk
