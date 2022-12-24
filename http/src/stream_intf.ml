open! Core
open! Async

module type S = sig
  val encoding : unit -> [ `Fixed of int | `Chunked ]
  val close : unit -> unit
  val read : unit -> [ `Ok of string | `Eof ] Deferred.t
  val closed : unit -> unit Deferred.t
  val read_started : unit -> bool
end
