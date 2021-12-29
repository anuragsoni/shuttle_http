module type S = sig
  module IO : Io_intf.S
  open IO

  type response
  type sink = string -> pos:int -> len:int -> unit Deferred.t

  module Context : sig
    type t

    val request : t -> Request.t
    val respond_with_string : t -> Response.t -> string -> response
    val respond_with_stream : t -> Response.t -> (unit -> string option Deferred.t) -> response
  end

  type 'a t

  val run
    :  Reader.t
    -> Writer.t
    -> (Request.t -> 'a * sink)
    -> ('a -> Context.t -> response)
    -> (?request:Request.t -> Status.t -> (Response.t * string) Deferred.t)
    -> unit Deferred.t
end
