open Async

val create_connection_handler
  :  ?config:Httpaf.Config.t
  -> error_handler:Httpaf.Server_connection.error_handler
  -> request_handler:Httpaf.Server_connection.request_handler
  -> Shuttle.Input_channel.t
  -> Shuttle.Output_channel.t
  -> unit Deferred.t
