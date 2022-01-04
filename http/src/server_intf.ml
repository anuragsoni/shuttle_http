module type S = sig
  module IO : Io_intf.S
  open IO

  module Body : sig
    module Reader : sig
      type t

      val create : (unit -> [ `Ok of string | `Eof ] Deferred.t) -> t
      val iter : t -> f:(string -> unit Deferred.t) -> unit Deferred.t
      val drain : t -> unit Deferred.t
      val read : t -> [ `Ok of string | `Eof ] Deferred.t
    end
  end

  module Connection : sig
    type t
    type response
    type sink = string -> unit Deferred.t

    val request : t -> Cohttp.Request.t
    val request_body : t -> Body.Reader.t
    val respond_with_string : t -> Cohttp.Response.t -> string -> response Deferred.t

    val respond_with_stream
      :  t
      -> Cohttp.Response.t
      -> Body.Reader.t
      -> response Deferred.t
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
