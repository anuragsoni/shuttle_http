open Core
open Async_kernel
open Async_unix

let set_nonblock fd = Fd.with_file_descr_exn fd ignore ~nonblocking:true

type 'a handle_chunk_result =
  [ `Stop of 'a
  | `Continue of int
  | `Wait of unit Deferred.t
  ]
[@@deriving sexp_of]

type t =
  { fd : Fd.t
  ; mutable reading : bool
  ; mutable is_closed : bool
  ; closed : unit Ivar.t
  ; buf : Bytebuffer.t
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
  ; buf = Bytebuffer.create buf_len
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

let refill t =
  Bytebuffer.compact t.buf;
  let result = Bytebuffer.read_assume_fd_is_nonblocking (Fd.file_descr_exn t.fd) t.buf in
  if Unix.Syscall_result.Int.is_ok result
  then (
    match Unix.Syscall_result.Int.ok_exn result with
    | 0 -> `Eof
    | n ->
      assert (n > 0);
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
    | Waiting of unit Deferred.t

  type nonrec 'a t =
    { reader : t
    ; on_chunk : Bigstring.t -> pos:int -> len:int -> 'a handle_chunk_result
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

  let can_process_chunk t =
    (not t.reader.is_closed)
    && is_running t
    && (Bytebuffer.can_reclaim_space t.reader.buf
       || Bytebuffer.available_to_write t.reader.buf > 0)
  ;;

  let process_chunks t =
    if can_process_chunk t
    then (
      let len = Bytebuffer.length t.reader.buf in
      if len > 0
      then (
        let buf = Bytebuffer.unsafe_buf t.reader.buf in
        let pos = Bytebuffer.pos t.reader.buf in
        let len = Bytebuffer.length t.reader.buf in
        match t.on_chunk buf ~pos ~len with
        | `Stop x -> interrupt t (Stopped_by_user x)
        | `Continue count -> Bytebuffer.drop t.reader.buf count
        | `Wait p -> if not (Deferred.is_determined p) then interrupt t (Waiting p)))
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

  let eof_to_response t =
    let len = Bytebuffer.length t.reader.buf in
    if len = 0
    then return `Eof
    else (
      let buf = Bytebuffer.unsafe_buf t.reader.buf in
      let pos = Bytebuffer.pos t.reader.buf in
      let len = Bytebuffer.length t.reader.buf in
      return (`Eof_with_unconsumed (Bigstring.sub buf ~pos ~len)))
  ;;

  let rec run reader ~on_chunk =
    let t = { reader; interrupt = Ivar.create (); state = Running; on_chunk } in
    let monitor =
      Monitor.create ~here:[%here] ~name:"Shuttle.Input_channel.Driver.run" ()
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
          "Shuttle.Input_channel.run: fd doesn't support watching"
            ~fd:(t.reader.fd : Fd.t)]
    | `Closed | `Interrupted ->
      (match t.state with
      | Running ->
        assert (Fd.is_closed t.reader.fd || t.reader.is_closed);
        eof_to_response t
      | Stopped (Stopped_by_user x) -> return (`Stopped x)
      | Stopped Handler_raised -> Deferred.never ()
      | Stopped (Waiting p) ->
        let%bind () = p in
        if is_closed reader then eof_to_response t else run reader ~on_chunk
      | Stopped Eof_reached -> eof_to_response t)
  ;;
end

let read_one_chunk_at_a_time t ~on_chunk =
  if t.is_closed
  then
    raise_s [%message "Shuttle.Input_channel: attempting to read from a closed channel"];
  if t.reading
  then raise_s [%message "Shuttle.Input_channel: already reading from input channel"];
  t.reading <- true;
  Monitor.protect
    ~run:`Now
    ~here:[%here]
    ~finally:(fun () ->
      t.reading <- false;
      Deferred.unit)
    (fun () -> Driver.run t ~on_chunk)
;;

let drain t =
  read_one_chunk_at_a_time t ~on_chunk:(fun _buf ~pos:_ ~len -> `Continue len)
  >>| function
  | `Eof -> ()
  | `Eof_with_unconsumed _ | `Stopped _ -> assert false
;;
