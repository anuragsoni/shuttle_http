module Make (IO : Io_intf.S) = struct
  open IO

  module Deferred = struct
    include Deferred

    let unit = return ()

    module Infix = struct
      let return x = return x
      let ( >>= ) = ( >>= )
    end

    module Option = struct
      let none = return None
      let some x = return (Some x)
    end
  end

  open Deferred.Infix

  module Pull = struct
    type 'a t = { mutable next : unit -> 'a option Deferred.t }

    let create next =
      let t = { next } in
      let next () =
        next ()
        >>= function
        | None ->
          t.next <- (fun () -> return None);
          Deferred.Option.none
        | r -> return r
      in
      t.next <- next;
      t
    ;;

    let read t = t.next ()
    let empty () = { next = (fun () -> Deferred.Option.none) }

    let of_list xs =
      let xs = ref xs in
      let next () =
        match !xs with
        | [] -> Deferred.Option.none
        | x :: xs' ->
          xs := xs';
          Deferred.Option.some x
      in
      { next }
    ;;

    let iter t ~f =
      let rec loop t ~f =
        t.next ()
        >>= function
        | None -> Deferred.unit
        | Some v -> f v >>= fun () -> loop t ~f
      in
      loop t ~f
    ;;
  end

  type t =
    { reader : Reader.t
    ; writer : Writer.t
    ; handler : Request.t -> string Pull.t -> (Response.t * string Pull.t) Deferred.t
    ; error_handler : ?request:Request.t -> Status.t -> (Response.t * string) Deferred.t
    }

  let rec parse_request conn =
    let view = Reader.view conn.reader in
    match
      Parser.parse_request
        ~pos:(Reader.View.pos view)
        ~len:(Reader.View.length view)
        (Reader.View.buf view)
    with
    | Ok (req, consumed) ->
      Reader.View.consume view consumed;
      return req
    | Error Parser.Partial ->
      Reader.refill reader
      >>= (function
      | `Eof -> failwith "EOF"
      | `Ok -> parse_request reader)
    | Error (Msg msg) -> failwith msg
  ;;

  let run reader writer handler error_handler =
    let conn = { reader; writer; handler; error_handler } in
    let rec loop conn = return () in
    loop conn
  ;;
end
