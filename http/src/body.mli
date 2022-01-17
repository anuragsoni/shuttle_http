open! Core
open! Async
open! Shuttle

module Reader : sig
  type t [@@deriving sexp_of]

  val create : Http.Request.t -> Input_channel.t -> t
  val encoding : t -> Http.Transfer.encoding
  val pipe : t -> string Pipe.Reader.t
  val drain : t -> unit Deferred.t
end

module Writer : sig
  type t [@@deriving sexp_of]

  val encoding : t -> Http.Transfer.encoding
  val empty : t
  val string : string -> t
  val stream : string Pipe.Reader.t -> t
  val write : t -> Output_channel.t -> unit Deferred.t
end
