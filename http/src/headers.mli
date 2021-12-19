type t [@@deriving sexp]

val empty : t
val of_list : (string * string) list -> t
val of_rev_list : (string * string) list -> t
val to_list : t -> (string * string) list
val to_rev_list : t -> (string * string) list
val iter : t -> f:(key:string -> data:string -> unit) -> unit
val fold : t -> init:'a -> f:('a -> key:string -> data:string -> 'a) -> 'a
val find : t -> string -> string option
val find_multi : t -> string -> string list
