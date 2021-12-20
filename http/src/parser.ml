external unsafe_memchr
  :  bytes
  -> int
  -> char
  -> int
  -> int
  = "shuttle_parser_bytes_memchr_stub"
  [@@noalloc]

external unsafe_memcmp
  :  bytes
  -> int
  -> string
  -> int
  -> int
  -> int
  = "shuttle_parser_bytes_memcmp_string"
  [@@noalloc]

let[@inline always] is_tchar = function
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
  | _ -> false
;;

module Source = struct
  type t =
    { buffer : bytes
    ; mutable pos : int
    ; min_off : int
    ; upper_bound : int
    }

  let of_bytes ?pos ?len buffer =
    let buf_len = Bytes.length buffer in
    let pos = Option.value pos ~default:0 in
    if pos < 0 || pos > buf_len
    then
      invalid_arg
        (Printf.sprintf
           "Shuttle_http.Parser.Source.of_bigstring: Invalid offset %d. Buffer length: %d"
           pos
           buf_len);
    let len = Option.value len ~default:(buf_len - pos) in
    if len < 0 || pos + len > buf_len
    then
      invalid_arg
        (Printf.sprintf
           "Shuttle_http.Parser.Source.of_bigstring: Invalid len %d. offset: %d, \
            buffer_length: %d, requested_length: %d"
           len
           pos
           buf_len
           (pos + len));
    { buffer; pos; min_off = pos; upper_bound = pos + len }
  ;;

  let[@inline always] get t idx =
    if idx < 0 || t.pos + idx >= t.upper_bound
    then invalid_arg "Shuttle_http.Parser.Source.get: Index out of bounds";
    Bytes.unsafe_get t.buffer (t.pos + idx)
  ;;

  let[@inline always] advance t count =
    if count < 0 || t.pos + count > t.upper_bound
    then
      invalid_arg
        (Printf.sprintf
           "Shuttle_http.Parser.Source.advance: Index out of bounds. Requested count: %d"
           count);
    t.pos <- t.pos + count
  ;;

  let[@inline always] length t = t.upper_bound - t.pos

  let[@inline always] to_string t ~pos ~len =
    if pos < 0
       || t.pos + pos >= t.upper_bound
       || len < 0
       || t.pos + pos + len > t.upper_bound
    then
      invalid_arg
        (Format.asprintf
           "Shuttle_http.Parser.Source.substring: Index out of bounds., Requested off: \
            %d, len: %d"
           pos
           len);
    Bytes.sub_string t.buffer (t.pos + pos) len
  ;;

  let[@inline always] consumed t = t.pos - t.min_off

  let[@inline always] index t ch =
    let res = unsafe_memchr t.buffer t.pos ch (length t) in
    if res = -1 then -1 else res - t.pos
  ;;

  let for_all_is_tchar t ~pos ~len =
    if pos < 0
       || t.pos + pos >= t.upper_bound
       || len < 0
       || t.pos + pos + len > t.upper_bound
    then
      invalid_arg
        (Format.asprintf
           "Shuttle_http.Parser.Source.substring: Index out of bounds. Requested off: \
            %d, len: %d"
           pos
           len);
    let idx = ref pos in
    while !idx < len && is_tchar (get t !idx) do
      incr idx
    done;
    !idx = len
  ;;

  let[@inline always] unsafe_memcmp t str len = unsafe_memcmp t.buffer t.pos str 0 len
end

exception Msg of string
exception Partial

let string str source =
  let len = String.length str in
  if Source.length source < len
  then raise_notrace Partial
  else if Source.unsafe_memcmp source str len = 0
  then Source.advance source len
  else raise_notrace (Msg (Printf.sprintf "Could not match: %S" str))
;;

let any_char source =
  if Source.length source = 0
  then raise_notrace Partial
  else (
    let c = Source.get source 0 in
    Source.advance source 1;
    c)
;;

let eol = string "\r\n"

(* token = 1*tchar tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "^"
   / "_" / "`" / "|" / "~" / DIGIT / ALPHA ; any VCHAR, except delimiters *)

let token source =
  let pos = Source.index source ' ' in
  if pos = -1
  then raise_notrace Partial
  else (
    let res = Source.to_string source ~pos:0 ~len:pos in
    Source.advance source (pos + 1);
    res)
;;

let meth source =
  let token = token source in
  match Meth.of_string token with
  | Some m -> m
  | None -> raise_notrace (Msg (Printf.sprintf "Unexpected HTTP verb %S" token))
;;

let version_source source =
  let _ = string "HTTP/1." source in
  any_char source
;;

let version source =
  let ch = version_source source in
  match ch with
  | '1' -> Version.v1_1
  | '0' -> { Version.major = 1; minor = 0 }
  | _ -> raise_notrace (Msg "Invalid http version")
;;

let header source =
  let pos = Source.index source ':' in
  if pos = -1
  then raise_notrace Partial
  else if pos = 0
  then raise_notrace (Msg "Invalid header: Empty header key")
  else if Source.for_all_is_tchar source ~pos:0 ~len:pos
  then (
    let key = Source.to_string source ~pos:0 ~len:pos in
    Source.advance source (pos + 1);
    while Source.length source > 0 && Source.get source 0 = ' ' do
      Source.advance source 1
    done;
    let pos = Source.index source '\r' in
    if pos = -1
    then raise_notrace Partial
    else (
      let v = Source.to_string source ~pos:0 ~len:pos in
      Source.advance source pos;
      key, String.trim v))
  else raise_notrace (Msg "Invalid Header Key")
;;

let headers =
  let rec loop source acc =
    let len = Source.length source in
    if len > 0 && Source.get source 0 = '\r'
    then (
      eol source;
      Headers.of_list acc)
    else (
      let v = header source in
      eol source;
      loop source (v :: acc))
  in
  fun source -> loop source []
;;

let chunk_length source =
  let length = ref 0 in
  let stop = ref false in
  let state = ref `Ok in
  let count = ref 0 in
  let processing_chunk = ref true in
  let in_chunk_extension = ref false in
  while not !stop do
    if Source.length source = 0
    then (
      stop := true;
      state := `Partial)
    else if !count = 16 && not !in_chunk_extension
    then (
      stop := true;
      state := `Chunk_too_big)
    else (
      let ch = Source.get source 0 in
      Source.advance source 1;
      incr count;
      match ch with
      | '0' .. '9' as ch when !processing_chunk ->
        let curr = Char.code ch - Char.code '0' in
        length := (!length lsl 4) lor curr
      | 'a' .. 'f' as ch when !processing_chunk ->
        let curr = Char.code ch - Char.code 'a' + 10 in
        length := (!length lsl 4) lor curr
      | 'A' .. 'F' as ch when !processing_chunk ->
        let curr = Char.code ch - Char.code 'A' + 10 in
        length := (!length lsl 4) lor curr
      | ';' when not !in_chunk_extension ->
        in_chunk_extension := true;
        processing_chunk := false
      | ('\t' | ' ') when !processing_chunk -> processing_chunk := false
      | ('\t' | ' ') when (not !in_chunk_extension) && not !processing_chunk -> ()
      | '\r' ->
        if Source.length source = 0
        then (
          stop := true;
          state := `Partial)
        else if Source.get source 0 = '\n'
        then (
          Source.advance source 1;
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
  | `Expected_newline -> raise_notrace (Msg "Expected_newline")
  | `Chunk_too_big -> raise_notrace (Msg "Chunk size is too large")
  | `Invalid_char ch ->
    raise_notrace (Msg (Printf.sprintf "Invalid chunk_length character %C" ch))
;;

let version source =
  let version = version source in
  eol source;
  version
;;

let request source =
  let meth = meth source in
  let path = token source in
  let version = version source in
  let headers = headers source in
  Request.create ~version ~headers meth path
;;

let run_parser ?pos ?len buf p =
  let source = Source.of_bytes ?pos ?len buf in
  match p source with
  | (exception (Partial as exn)) | (exception (Msg _ as exn)) -> Error exn
  | v -> Ok (v, Source.consumed source)
;;

let parse_request ?pos ?len buf = run_parser ?pos ?len buf request
let parse_chunk_length ?pos ?len buf = run_parser ?pos ?len buf chunk_length