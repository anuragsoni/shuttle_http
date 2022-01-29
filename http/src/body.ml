open Core
open Async
open Shuttle

type encoding = Http.Transfer.encoding =
  | Chunked
  | Fixed of int64
  | Unknown
[@@deriving sexp]

module Reader = struct
  type t =
    { encoding : encoding
    ; reader : string Pipe.Reader.t
    }
  [@@deriving sexp_of]

  let empty = { encoding = Http.Transfer.Fixed 0L; reader = Pipe.empty () }

  module Private = struct
    let fixed_reader len chan =
      Pipe.create_reader ~close_on_exception:false (fun writer ->
          Deferred.repeat_until_finished len (fun len ->
              Input_channel.read chan len
              >>= function
              | `Eof -> return (`Finished ())
              | `Ok chunk ->
                let consumed = String.length chunk in
                Pipe.write_if_open writer chunk
                >>= fun () ->
                if consumed = len
                then return (`Finished ())
                else return (`Repeat (len - consumed))))
    ;;

    let chunked_reader chan =
      Pipe.create_reader ~close_on_exception:false (fun writer ->
          Deferred.repeat_until_finished Parser.Start_chunk (fun state ->
              let view = Input_channel.view chan in
              let buf = Input_channel.View.buf view in
              let pos = Input_channel.View.pos view in
              let len = Input_channel.View.length view in
              match Parser.parse_chunk ~pos ~len buf state with
              | Error (Msg msg) ->
                Log.Global.error "Error while parsing chunk: %s" msg;
                failwith msg
              | Error Partial ->
                Input_channel.refill chan
                >>| (function
                | `Ok -> `Repeat state
                | `Buffer_is_full -> `Finished ()
                | `Eof -> `Finished ())
              | Ok (parse_result, consumed) ->
                Input_channel.View.consume view consumed;
                (match parse_result with
                | Parser.Chunk_complete chunk ->
                  Pipe.write_if_open writer chunk >>| fun () -> `Repeat Parser.Start_chunk
                | Parser.Done -> return (`Finished ())
                | Parser.Partial_chunk (chunk, to_consume) ->
                  Pipe.write_if_open writer chunk
                  >>| fun () -> `Repeat (Parser.Continue_chunk to_consume))))
    ;;

    let create req chan =
      match Http.Header.get_transfer_encoding (Http.Request.headers req) with
      | Http.Transfer.Unknown -> empty
      | Http.Transfer.Fixed 0L -> empty
      | Http.Transfer.Fixed len as encoding ->
        let reader = fixed_reader (Int64.to_int_exn len) chan in
        { encoding; reader }
      | Http.Transfer.Chunked as encoding -> { encoding; reader = chunked_reader chan }
    ;;
  end

  let encoding t = t.encoding
  let pipe t = t.reader
  let drain t = Pipe.drain t.reader
end

module Writer = struct
  type kind =
    | Empty
    | Fixed of string
    | Stream of string Pipe.Reader.t
  [@@deriving sexp_of]

  type t =
    { encoding : encoding
    ; kind : kind
    }
  [@@deriving sexp_of]

  let encoding t = t.encoding
  let empty = { encoding = Http.Transfer.Fixed 0L; kind = Empty }

  let string x =
    { encoding = Http.Transfer.Fixed (Int64.of_int (String.length x)); kind = Fixed x }
  ;;

  let stream ?(encoding = Http.Transfer.Chunked) x = { encoding; kind = Stream x }

  module Private = struct
    let is_chunked t =
      match t.encoding with
      | Http.Transfer.Chunked -> true
      | _ -> false
    ;;

    let make_writer t =
      match t.encoding with
      | Http.Transfer.Chunked ->
        fun writer buf ->
          (* avoid writing empty payloads as that is used to indicate the end of a
             stream. *)
          if String.is_empty buf
          then Deferred.unit
          else (
            Output_channel.writef writer "%x\r\n" (String.length buf);
            Output_channel.write writer buf;
            Output_channel.write writer "\r\n";
            Output_channel.flush writer)
      | _ ->
        fun writer buf ->
          if String.is_empty buf
          then Deferred.unit
          else (
            Output_channel.write writer buf;
            Output_channel.flush writer)
    ;;

    let write t writer =
      Deferred.create (fun ivar ->
          match t.kind with
          | Empty -> Ivar.fill ivar ()
          | Fixed x ->
            Output_channel.write writer x;
            Output_channel.flush writer >>> fun () -> Ivar.fill ivar ()
          | Stream xs ->
            let write_chunk = make_writer t in
            Pipe.iter xs ~f:(fun buf -> write_chunk writer buf)
            >>> fun () ->
            if is_chunked t
            then (
              Output_channel.write writer "0\r\n\r\n";
              Output_channel.flush writer >>> fun () -> Ivar.fill ivar ())
            else Ivar.fill ivar ())
    ;;
  end
end
