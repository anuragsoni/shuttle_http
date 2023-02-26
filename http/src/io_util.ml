open! Core
open! Async
open! Shuttle

let keep_alive headers =
  match Headers.find headers "connection" with
  | Some x when String.Caseless.equal x "close" -> false
  | _ -> true
;;

let get_transfer_encoding headers =
  match List.rev @@ Headers.find_multi headers "Transfer-Encoding" with
  | x :: _ when String.Caseless.equal x "chunked" -> `Chunked
  | _x :: _ -> `Bad_request
  | [] ->
    (match
       List.dedup_and_sort
         ~compare:String.Caseless.compare
         (Headers.find_multi headers "Content-Length")
     with
     | [] -> `Fixed 0
     (* TODO: check for exceptions when converting to int *)
     | [ x ] ->
       let len =
         try Int.of_string x with
         | _ -> -1
       in
       if Int.(len >= 0) then `Fixed len else `Bad_request
     | _ -> `Bad_request)
;;

let parse_body reader headers =
  match get_transfer_encoding headers with
  | `Fixed 0 -> Ok Body.empty
  | `Fixed len ->
    let view = Input_channel.view reader in
    if view.len >= len
    then (
      let chunk = Bigstring.to_string view.buf ~pos:view.pos ~len in
      Input_channel.consume reader len;
      Ok (Body.string chunk))
    else (
      let pipe =
        Pipe.create_reader ~close_on_exception:false (fun writer ->
          Deferred.repeat_until_finished len (fun len ->
            let view = Input_channel.view reader in
            if view.len > 0
            then (
              let to_read = min len view.len in
              let chunk = Bigstring.to_string view.buf ~pos:view.pos ~len:to_read in
              Input_channel.consume reader to_read;
              let%map () = Pipe.write_if_open writer chunk in
              if to_read = len then `Finished () else `Repeat (len - to_read))
            else (
              match%map Input_channel.refill reader with
              | `Eof -> `Finished ()
              | `Ok -> `Repeat len)))
      in
      Ok (Body.of_pipe (`Fixed len) pipe))
  | `Chunked ->
    let pipe =
      Pipe.create_reader ~close_on_exception:false (fun writer ->
        Deferred.repeat_until_finished Parser.Start_chunk (fun state ->
          let view = Input_channel.view reader in
          match Parser.parse_chunk ~pos:view.pos ~len:view.len view.buf state with
          | Error Partial ->
            (match%map Input_channel.refill reader with
             | `Eof -> `Finished ()
             | `Ok -> `Repeat state)
          | Error (Fail error) -> Error.raise error
          | Ok (parse_result, consumed) ->
            Input_channel.consume reader consumed;
            (match parse_result with
             | Parser.Chunk_complete chunk ->
               let%map () = Pipe.write_if_open writer chunk in
               `Repeat Parser.Start_chunk
             | Parser.Done -> return (`Finished ())
             | Parser.Partial_chunk (chunk, to_consume) ->
               let%map () = Pipe.write_if_open writer chunk in
               `Repeat (Parser.Continue_chunk to_consume))))
    in
    Ok (Body.of_pipe `Chunked pipe)
  | `Bad_request -> Or_error.error_s [%sexp "Invalid transfer encoding"]
;;
