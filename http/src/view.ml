type t =
  { buf : string
  ; pos : int
  ; len : int
  }

let create buf ~pos ~len = { buf; pos; len }
