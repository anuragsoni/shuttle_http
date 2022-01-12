open! Core
open! Async
open! Shuttle

module IO :
  Cohttp.S.IO
    with type 'a t = 'a Deferred.t
     and type ic = Input_channel.t
     and type oc = Output_channel.t

module Request : Cohttp.S.Http_io with type t := Http.Request.t and module IO := IO
module Response : Cohttp.S.Http_io with type t := Http.Response.t and module IO := IO
