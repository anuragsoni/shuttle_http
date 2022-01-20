open Core
open Async
open Shuttle

type encoding = Http.Transfer.encoding

let sexp_of_encoding t = Cohttp.Transfer.sexp_of_encoding t

module Reader = struct
  type t =
    { encoding : encoding
    ; reader : string Pipe.Reader.t
    }
  [@@deriving sexp_of]

  let empty = { encoding = Http.Transfer.Fixed 0L; reader = Pipe.empty () }

  module Private = struct
    let create req chan =
      match Http.Request.has_body req with
      | `No | `Unknown -> empty
      | `Yes ->
        let reader = Io.Request.make_body_reader req chan in
        let reader =
          Pipe.create_reader ~close_on_exception:false (fun writer ->
              Deferred.repeat_until_finished () (fun () ->
                  match%bind Io.Request.read_body_chunk reader with
                  | Done -> return (`Finished ())
                  | Chunk buf ->
                    if Pipe.is_closed writer
                    then return (`Repeat ())
                    else (
                      let%map () = Pipe.write writer buf in
                      `Repeat ())
                  | Final_chunk buf ->
                    if Pipe.is_closed writer
                    then return (`Finished ())
                    else (
                      let%map () = Pipe.write writer buf in
                      `Finished ())))
        in
        { encoding = Http.Request.encoding req; reader }
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
          Output_channel.writef writer "%x\r\n" (String.length buf);
          Output_channel.write writer buf;
          Output_channel.write writer "\r\n";
          Output_channel.flush writer
      | _ ->
        fun writer buf ->
          Output_channel.write writer buf;
          Output_channel.flush writer
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
