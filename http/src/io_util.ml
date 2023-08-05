open! Core
open! Async

let write_body body writer =
  match body with
  | Body.Empty -> Output_channel.flush writer
  | Fixed x ->
    Output_channel.write writer x;
    Output_channel.flush writer
  | Stream stream ->
    (match Body.Stream.encoding stream with
     | `Fixed _ ->
       Body.Stream.iter stream ~f:(fun v ->
         Output_channel.write writer v;
         Output_channel.flush writer)
     | `Chunked ->
       let%bind () =
         Body.Stream.iter stream ~f:(fun v ->
           if String.is_empty v
           then Deferred.unit
           else (
             Output_channel.writef writer "%x\r\n" (String.length v);
             Output_channel.write writer v;
             Output_channel.write writer "\r\n";
             Output_channel.flush writer))
       in
       Output_channel.write writer "0\r\n\r\n";
       Output_channel.flush writer)
;;

let parse_body reader transfer_encoding =
  match transfer_encoding with
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
  | `Bad_request | `Bad_response -> Or_error.error_s [%sexp "Invalid transfer encoding"]
;;
