open! Core
open! Async
open! Shuttle

module Reader : sig
  type t [@@deriving sexp_of]

  val encoding : t -> Http.Transfer.encoding
  val pipe : t -> string Pipe.Reader.t
  val drain : t -> unit Deferred.t

  module Private : sig
    val create : Http.Request.t -> Input_channel.t -> t
  end
end

module Writer : sig
  type t [@@deriving sexp_of]

  val encoding : t -> Http.Transfer.encoding
  val empty : t
  val string : string -> t
  val stream : ?encoding:Http.Transfer.encoding -> string Pipe.Reader.t -> t

  module Private : sig
    val write : t -> Output_channel.t -> unit Deferred.t
  end
end
