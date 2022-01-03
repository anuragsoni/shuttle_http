module type S = sig
  module Deferred : sig
    type +'a t

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
  end

  module Reader : sig
    type t

    module View : sig
      type t

      val buf : t -> string
      val pos : t -> int
      val length : t -> int
      val consume : t -> int -> unit
    end

    val refill : t -> [ `Ok | `Eof ] Deferred.t
    val view : t -> View.t
  end

  module Writer : sig
    type t

    val write : t -> string -> unit
    val write_char : t -> char -> unit
    val flush : t -> unit Deferred.t
  end
end
