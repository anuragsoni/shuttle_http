open! Core

type t [@@deriving sexp]

val of_rev_list : (string * string) list -> t
val of_list : (string * string) list -> t
val to_rev_list : t -> (string * string) list
val to_list : t -> (string * string) list
val iter : t -> f:(key:string -> data:string -> unit) -> unit
val mem : t -> string -> bool
val find : t -> string -> string option
val find_multi : t -> string -> string list
val empty : t
val is_empty : t -> bool
val add_unless_exists : t -> key:string -> data:string -> t
val add : t -> key:string -> data:string -> t
val remove : t -> string -> t
