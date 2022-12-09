open Core
open Async_kernel
open Async_unix
module Logger = Log.Make_global ()

type t =
  { fd : Fd.t
  ; mutable reading : bool
  ; mutable is_closed : bool
  ; closed : unit Ivar.t
  ; buf : Bytebuffer.t
  }
[@@deriving sexp_of]

let create ?max_buffer_size ?buf_len fd =
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
  ; reading = false
  ; is_closed = false
  ; closed = Ivar.create ()
  ; buf = Bytebuffer.create ?max_buffer_size buf_len
  }
;;

let consume t n = Bytebuffer.drop t.buf n
let is_closed t = t.is_closed
let closed t = Ivar.read t.closed

let close t =
  if not t.is_closed
  then (
    t.is_closed <- true;
    Fd.close t.fd >>> fun () -> Ivar.fill t.closed ());
  closed t
;;

let refill_nonblocking t =
  Bytebuffer.compact t.buf;
  if Bytebuffer.available_to_write t.buf = 0 then Bytebuffer.ensure_space t.buf 1;
  let result =
    Fd.syscall_result_exn ~nonblocking:true t.fd t.buf (fun fd buf ->
      Bytebuffer.read_assume_fd_is_nonblocking buf fd)
  in
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

let view t = Bytebuffer.unsafe_peek t.buf

let rec refill t =
  if Fd.supports_nonblock t.fd
  then (
    match refill_nonblocking t with
    | `Read_some -> return `Ok
    | `Eof -> return `Eof
    | `Nothing_available ->
      Fd.ready_to t.fd `Read
      >>= (function
      | `Ready -> refill t
      | `Closed -> return `Eof
      | `Bad_fd ->
        raise_s
          [%message "Shuttle.Input_channel.read: bad file descriptor" ~fd:(t.fd : Fd.t)]))
  else (
    Bytebuffer.compact t.buf;
    if Bytebuffer.available_to_write t.buf = 0 then Bytebuffer.ensure_space t.buf 1;
    match%map
      Fd.syscall_in_thread t.fd ~name:"read" (fun fd ->
        let count = Bytebuffer.read t.buf fd in
        if count = 0 then `Eof else `Ok)
    with
    | `Already_closed -> `Eof
    | `Error (Bigstring_unix.IOError (0, End_of_file)) -> `Eof
    | `Error exn -> Exn.reraise exn "Error while performing a read syscall"
    | `Ok c -> c)
;;

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

let rec read_line_slow t acc =
  if Bytebuffer.length t.buf = 0
  then (
    match%bind refill t with
    | `Eof ->
      (match acc with
       | [] -> return `Eof
       | xs -> return (`Eof_with_unconsumed xs))
    | `Ok -> read_line_slow t acc)
  else (
    let idx = Bytebuffer.unsafe_index t.buf '\n' in
    if idx > -1
    then (
      let { Bytebuffer.Slice.buf; pos; _ } = Bytebuffer.unsafe_peek t.buf in
      let len = idx in
      if len >= 1 && Char.equal (Bigstring.get buf (pos + idx - 1)) '\r'
      then (
        let line = Bigstring.To_string.sub buf ~pos ~len:(idx - 1) in
        Bytebuffer.drop t.buf (len + 1);
        return (`Ok (line :: acc)))
      else (
        let line = Bigstring.To_string.sub buf ~pos ~len in
        Bytebuffer.drop t.buf (len + 1);
        return (`Ok (line :: acc))))
    else (
      let curr = Bytebuffer.to_string t.buf in
      Bytebuffer.drop t.buf (String.length curr);
      read_line_slow t (curr :: acc)))
;;

let read_line t =
  let idx = Bytebuffer.unsafe_index t.buf '\n' in
  if idx > -1
  then (
    let { Bytebuffer.Slice.buf; pos; _ } = Bytebuffer.unsafe_peek t.buf in
    let len = idx in
    if len >= 1 && Char.equal (Bigstring.get buf (pos + idx - 1)) '\r'
    then (
      let line = Bigstring.To_string.sub buf ~pos ~len:(idx - 1) in
      Bytebuffer.drop t.buf (len + 1);
      return (`Ok line))
    else (
      let line = Bigstring.To_string.sub buf ~pos ~len in
      Bytebuffer.drop t.buf (len + 1);
      return (`Ok line)))
  else (
    match%map read_line_slow t [] with
    | `Eof -> `Eof
    | `Eof_with_unconsumed xs -> `Ok (String.concat (List.rev xs))
    | `Ok chunks -> `Ok (String.concat (List.rev chunks)))
;;

let lines t =
  let r, w = Pipe.create () in
  let finished =
    Deferred.create (fun ivar ->
      let rec loop t =
        read_line t
        >>> function
        | `Eof -> Ivar.fill ivar ()
        | `Ok v ->
          if Pipe.is_closed w
          then Ivar.fill ivar ()
          else Pipe.write w v >>> fun () -> loop t
      in
      loop t)
  in
  upon finished (fun () -> close t >>> fun () -> Pipe.close w);
  r
;;

let rec read t len =
  let view = view t in
  if view.len > 0
  then (
    let to_read = min view.len len in
    let buf = Bigstring.to_string view.buf ~pos:view.pos ~len:to_read in
    consume t to_read;
    return (`Ok buf))
  else
    refill t
    >>= function
    | `Eof -> return `Eof
    | `Ok -> read t len
;;

let open_file ?buf_len filename =
  let%map fd = Unix.openfile filename ~mode:[ `Rdonly ] in
  create ?buf_len fd
;;

let with_file ?buf_len filename ~f =
  let%bind t = open_file ?buf_len filename in
  Monitor.protect ~run:`Now ~finally:(fun () -> close t) (fun () -> f t)
;;
