type t =
  { version : Version.t
  ; status : Status.t
  ; headers : Headers.t
  }

let create ?(version = Version.v1_1) ?(headers = Headers.empty) status =
  { version; status; headers }
;;

let version t = t.version
let status t = t.status
let headers t = t.headers
