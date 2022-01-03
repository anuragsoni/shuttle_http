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

  type 'a t

  val run
    :  Reader.t
    -> Writer.t
    -> (Cohttp.Request.t -> string Pull.t -> (Cohttp.Response.t * Body.t) Deferred.t)
    -> (?request:Cohttp.Request.t
        -> Cohttp.Code.status_code
        -> (Cohttp.Response.t * string) Deferred.t)
    -> unit Deferred.t
end
