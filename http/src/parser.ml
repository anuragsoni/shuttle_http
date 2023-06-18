open Core

exception Fail of Error.t
exception Partial

let tchar_map =
  Array.init 256 ~f:(fun idx ->
    match Char.of_int_exn idx with
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!'
    | '#'
    | '$'
    | '%'
    | '&'
    | '\''
    | '*'
    | '+'
    | '-'
    | '.'
    | '^'
    | '_'
    | '`'
    | '|'
    | '~' -> true
    | _ -> false)
;;

module Source = struct
  type t =
    { buffer : Bigstring.t
    ; mutable pos : int
    ; upper_bound : int
    }

  let[@inline always] unsafe_get t idx = Bigstring.get t.buffer (t.pos + idx)
  let[@inline always] unsafe_advance t count = t.pos <- t.pos + count
  let[@inline always] length t = t.upper_bound - t.pos
  let[@inline always] is_empty t = t.pos = t.upper_bound

  let[@inline always] to_string t ~pos ~len =
    let b = Bytes.create len in
    Bigstring.To_bytes.unsafe_blit
      ~src:t.buffer
      ~dst:b
      ~src_pos:(t.pos + pos)
      ~dst_pos:0
      ~len;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b
  ;;

  let[@inline always] is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false
  ;;

  let[@inline always] to_string_trim t ~pos ~len =
    let last = ref (t.pos + len - 1) in
    let pos = ref (t.pos + pos) in
    while is_space (Bigstring.get t.buffer !pos) && !pos < !last do
      incr pos
    done;
    while is_space (Bigstring.get t.buffer !last) && !last > !pos do
      decr last
    done;
    let len = !last - !pos + 1 in
    let b = Bytes.create len in
    Bigstring.To_bytes.unsafe_blit ~src:t.buffer ~dst:b ~src_pos:!pos ~dst_pos:0 ~len;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b
  ;;

  let[@inline always] index t ch =
    let idx = Bigstring.unsafe_find t.buffer ch ~pos:t.pos ~len:(length t) in
    if idx < 0 then -1 else idx - t.pos
  ;;

  let[@inline always] consume_eol t =
    if length t < 2 then raise_notrace Partial;
    if Char.(
         Bigstring.get t.buffer t.pos = '\r' && Bigstring.get t.buffer (t.pos + 1) = '\n')
    then unsafe_advance t 2
    else raise_notrace (Fail (Error.of_string "Expected EOL"))
  ;;

  let[@inline always] consume_space t =
    if length t < 1 then raise_notrace Partial;
    if Char.(Bigstring.get t.buffer t.pos = ' ')
    then unsafe_advance t 1
    else raise_notrace (Fail (Error.of_string "Expected space"))
  ;;

  let[@inline always] parse_reason_phrase t =
    let pos = index t '\r' in
    if pos = -1
    then raise_notrace Partial
    else if pos = 0
    then ""
    else (
      let phrase = to_string t ~pos:0 ~len:pos in
      unsafe_advance t pos;
      phrase)
  ;;

  let parse_header tchar_map source =
    let pos = index source ':' in
    if pos = -1
    then raise_notrace Partial
    else if pos = 0
    then raise_notrace (Fail (Error.of_string "Invalid header: Empty header key"));
    for idx = 0 to pos - 1 do
      if not (Array.unsafe_get tchar_map (Char.to_int (unsafe_get source idx)))
      then raise_notrace (Fail (Error.of_string "Invalid Header Key"))
    done;
    let key = to_string source ~pos:0 ~len:pos in
    unsafe_advance source (pos + 1);
    let pos = index source '\r' in
    if pos = -1 then raise_notrace Partial;
    let v = to_string_trim source ~pos:0 ~len:pos in
    unsafe_advance source pos;
    key, v
  ;;
end

let[@inline always] ( .![] ) source idx = Source.unsafe_get source idx
let invalid_method = Fail (Error.of_string "Invalid Method")

let invalid_status_code =
  Fail (Error.of_string "Status codes must be three digit numbers")
;;

let status source =
  if Source.length source < 3 then raise_notrace Partial;
  if Char.is_digit source.![0] && Char.is_digit source.![1] && Char.is_digit source.![2]
  then (
    match Status.of_string (Source.to_string source ~pos:0 ~len:3) with
    | Ok code ->
      Source.unsafe_advance source 3;
      code
    | Error err -> raise_notrace (Fail err))
  else raise_notrace invalid_status_code
;;

let meth source =
  let pos = Source.index source ' ' in
  if pos = -1 then raise_notrace Partial;
  let meth =
    match pos with
    | 3 ->
      (match source.![0], source.![1], source.![2] with
       | 'G', 'E', 'T' -> `GET
       | 'P', 'U', 'T' -> `PUT
       | _ -> raise_notrace invalid_method)
    | 4 ->
      (match source.![0], source.![1], source.![2], source.![3] with
       | 'H', 'E', 'A', 'D' -> `HEAD
       | 'P', 'O', 'S', 'T' -> `POST
       | _ -> raise_notrace invalid_method)
    | 5 ->
      (match source.![0], source.![1], source.![2], source.![3], source.![4] with
       | 'P', 'A', 'T', 'C', 'H' -> `PATCH
       | 'T', 'R', 'A', 'C', 'E' -> `TRACE
       | _ -> raise_notrace invalid_method)
    | 6 ->
      (match
         source.![0], source.![1], source.![2], source.![3], source.![4], source.![5]
       with
       | 'D', 'E', 'L', 'E', 'T', 'E' -> `DELETE
       | _ -> raise_notrace invalid_method)
    | 7 ->
      (match
         ( source.![0]
         , source.![1]
         , source.![2]
         , source.![3]
         , source.![4]
         , source.![5]
         , source.![6] )
       with
       | 'C', 'O', 'N', 'N', 'E', 'C', 'T' -> `CONNECT
       | 'O', 'P', 'T', 'I', 'O', 'N', 'S' -> `OPTIONS
       | _ -> raise_notrace invalid_method)
    | _ -> raise_notrace invalid_method
  in
  Source.unsafe_advance source (pos + 1);
  meth
;;

let rec headers source =
  if (not (Source.is_empty source)) && Char.(Source.unsafe_get source 0 = '\r')
  then (
    Source.consume_eol source;
    [])
  else (
    let header = Source.parse_header tchar_map source in
    Source.consume_eol source;
    header :: headers source)
;;

let chunk_length source =
  let length = ref 0 in
  let stop = ref false in
  let state = ref `Ok in
  let count = ref 0 in
  let processing_chunk = ref true in
  let in_chunk_extension = ref false in
  while not !stop do
    if Source.is_empty source
    then (
      stop := true;
      state := `Partial)
    else if !count = 16 && not !in_chunk_extension
    then (
      stop := true;
      state := `Chunk_too_big)
    else (
      let ch = Source.unsafe_get source 0 in
      Source.unsafe_advance source 1;
      incr count;
      match ch with
      | '0' .. '9' as ch when !processing_chunk ->
        let curr = Char.to_int ch - Char.to_int '0' in
        length := (!length lsl 4) lor curr
      | 'a' .. 'f' as ch when !processing_chunk ->
        let curr = Char.to_int ch - Char.to_int 'a' + 10 in
        length := (!length lsl 4) lor curr
      | 'A' .. 'F' as ch when !processing_chunk ->
        let curr = Char.to_int ch - Char.to_int 'A' + 10 in
        length := (!length lsl 4) lor curr
      | ';' when not !in_chunk_extension ->
        in_chunk_extension := true;
        processing_chunk := false
      | ('\t' | ' ') when !processing_chunk -> processing_chunk := false
      | ('\t' | ' ') when (not !in_chunk_extension) && not !processing_chunk -> ()
      | '\r' ->
        if Source.is_empty source
        then (
          stop := true;
          state := `Partial)
        else if Char.(Source.unsafe_get source 0 = '\n')
        then (
          Source.unsafe_advance source 1;
          stop := true)
        else (
          stop := true;
          state := `Expected_newline)
      | _ when !in_chunk_extension ->
        (* Chunk extensions aren't very common, see:
           https://tools.ietf.org/html/rfc7230#section-4.1.1 Chunk extensions aren't
           pre-defined, and they are specific to invidividual connections. In the future
           we might surface these to the user somehow, but for now we will ignore any
           extensions. TODO: Should there be any limit on the size of chunk extensions we
           parse? We might want to error if a request contains really large chunk
           extensions. *)
        ()
      | ch ->
        stop := true;
        state := `Invalid_char ch)
  done;
  match !state with
  | `Ok -> !length
  | `Partial -> raise_notrace Partial
  | `Expected_newline -> raise_notrace (Fail (Error.of_string "Expected_newline"))
  | `Chunk_too_big -> raise_notrace (Fail (Error.of_string "Chunk size is too large"))
  | `Invalid_char ch ->
    raise_notrace (Fail (Error.create "Invalid chunk_length character" ch sexp_of_char))
;;

let version source =
  if Source.length source < 8 then raise_notrace Partial;
  if Char.equal source.![0] 'H'
     && Char.equal source.![1] 'T'
     && Char.equal source.![2] 'T'
     && Char.equal source.![3] 'P'
     && Char.equal source.![4] '/'
     && Char.equal source.![5] '1'
     && Char.equal source.![6] '.'
     && Char.equal source.![7] '1'
  then (
    Source.unsafe_advance source 8;
    Version.Http_1_1)
  else raise_notrace (Fail (Error.of_string "Invalid HTTP Version"))
;;

let token source =
  let pos = Source.index source ' ' in
  if pos = -1 then raise_notrace Partial;
  let res = Source.to_string source ~pos:0 ~len:pos in
  Source.unsafe_advance source (pos + 1);
  res
;;

let request source =
  let meth = meth source in
  let path = token source in
  let version = version source in
  Source.consume_eol source;
  let headers = headers source in
  Request.create ~version ~headers meth path
;;

let response source =
  let version = version source in
  Source.consume_space source;
  let status = status source in
  Source.consume_space source;
  let reason_phrase = Source.parse_reason_phrase source in
  Source.consume_eol source;
  let headers = headers source in
  Response.create ~version ~headers ~reason_phrase status
;;

let take len source =
  let available = Source.length source in
  let to_consume = min len available in
  if to_consume = 0 then raise_notrace Partial;
  let payload = Source.to_string source ~pos:0 ~len:to_consume in
  Source.unsafe_advance source to_consume;
  payload
;;

type chunk_kind =
  | Start_chunk
  | Continue_chunk of int

type chunk_parser_result =
  | Chunk_complete of string
  | Done
  | Partial_chunk of string * int

let chunk chunk_kind source =
  match chunk_kind with
  | Start_chunk ->
    let chunk_length = chunk_length source in
    if chunk_length = 0
    then (
      Source.consume_eol source;
      Done)
    else (
      let current_chunk = take chunk_length source in
      let current_chunk_length = String.length current_chunk in
      if current_chunk_length = chunk_length
      then (
        Source.consume_eol source;
        Chunk_complete current_chunk)
      else Partial_chunk (current_chunk, chunk_length - current_chunk_length))
  | Continue_chunk len ->
    let chunk = take len source in
    let current_chunk_length = String.length chunk in
    if current_chunk_length = len
    then (
      Source.consume_eol source;
      Chunk_complete chunk)
    else Partial_chunk (chunk, len - current_chunk_length)
;;

type error =
  | Partial
  | Fail of Error.t

let run_parser ?(pos = 0) ?len buf p =
  let total_length = Bigstring.length buf in
  let len =
    match len with
    | Some v -> v
    | None -> total_length - pos
  in
  Ordered_collection_common.check_pos_len_exn ~pos ~len ~total_length;
  let source = Source.{ buffer = buf; pos; upper_bound = pos + len } in
  match p source with
  | exception Partial -> Error Partial
  | exception Fail m -> Error (Fail m)
  | v ->
    let consumed = source.pos - pos in
    Ok (v, consumed)
;;

let parse_request ?pos ?len buf = run_parser ?pos ?len buf request
let parse_response ?pos ?len buf = run_parser ?pos ?len buf response
let parse_chunk_length ?pos ?len buf = run_parser ?pos ?len buf chunk_length
let parse_chunk ?pos ?len buf chunk_kind = run_parser ?pos ?len buf (chunk chunk_kind)

module Private = struct
  let parse_method ?pos ?len buf = run_parser ?pos ?len buf meth
end
