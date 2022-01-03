module type S = sig
  module IO : Io_intf.S
  open IO

  module Pull : sig
    type 'a t

    val create : (unit -> 'a option Deferred.t) -> 'a t
    val read : 'a t -> 'a option Deferred.t
    val empty : unit -> 'a t
    val of_list : 'a list -> 'a t
    val iter : 'a t -> f:('a -> unit Deferred.t) -> unit Deferred.t
    val drain : 'a t -> unit Deferred.t
  end

  module Body : sig
    type t

    val string : string -> t
    val stream : string Pull.t -> t
  end

  module Connection : sig
    type t
    type response

    val request : t -> Cohttp.Request.t
    val request_body : t -> string Pull.t
    val respond_with_string : t -> Cohttp.Response.t -> string -> response Deferred.t
  end

  val run
    :  Reader.t
    -> Writer.t
    -> (Connection.t -> Connection.response Deferred.t)
    -> (?request:Cohttp.Request.t
        -> Cohttp.Code.status_code
        -> (Cohttp.Response.t * string) Deferred.t)
    -> unit Deferred.t
end
