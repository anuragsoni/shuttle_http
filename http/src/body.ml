open! Core
open! Async

type t =
  [ Cohttp.Body.t
  | `Pipe of string Pipe.Reader.t
  ]
[@@deriving sexp_of]

let transfer_encoding = function
  | #Cohttp.Body.t as t -> Cohttp.Body.transfer_encoding t
  | `Pipe _ -> Cohttp.Transfer.Chunked
;;

let empty = `Empty
let of_string s = Cohttp.Body.of_string s

let pipe () =
  let reader, writer = Pipe.create () in
  `Pipe reader, writer
;;

let to_string = function
  | #Cohttp.Body.t as b -> return (Cohttp.Body.to_string b)
  | `Pipe p ->
    let buf = Buffer.create 128 in
    Pipe.iter_without_pushback p ~f:(fun v -> Buffer.add_string buf v)
    >>| fun () -> Buffer.contents buf
;;

let drain = function
  | #Cohttp.Body.t -> Deferred.unit
  | `Pipe p -> Pipe.drain p
;;

let to_pipe = function
  | `Empty -> Pipe.empty ()
  | `String s -> Pipe.singleton s
  | `Strings xs -> Pipe.of_list xs
  | `Pipe p -> p
;;

let write_body write_body (body : t) writer =
  match body with
  | `Empty -> return ()
  | `String s -> write_body writer s
  | `Strings sl -> Deferred.List.iter sl ~f:(write_body writer)
  | `Pipe p -> Pipe.iter p ~f:(write_body writer)
;;

let pipe_of_body read_chunk ic =
  let open Cohttp.Transfer in
  Pipe.create_reader ~close_on_exception:false (fun writer ->
      Deferred.repeat_until_finished () (fun () ->
          read_chunk ic
          >>= function
          | Chunk buf ->
            (* Even if [writer] has been closed, the loop must continue reading
             * from the input channel to ensure that it is left in a proper state
             * for the next request to be processed (in the case of keep-alive).
             *
             * The only case where [writer] will be closed is when
             * [Pipe.close_read] has been called on its read end. This could be
             * done by a request handler to signal that it does not need to
             * inspect the remainder of the body to fulfill the request.
             *)
            Pipe.write_when_ready writer ~f:(fun write -> write buf)
            >>| fun _ -> `Repeat ()
          | Final_chunk buf ->
            Pipe.write_when_ready writer ~f:(fun write -> write buf)
            >>| fun _ -> `Finished ()
          | Done -> return (`Finished ())))
;;

module Private = struct
  let write_body = write_body
  let pipe_of_body = pipe_of_body
  let drain = drain
end
