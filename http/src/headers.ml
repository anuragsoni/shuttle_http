open Core

type t = (string, string) List.Assoc.t [@@deriving sexp]

let of_rev_list xs = xs
let of_list xs = List.rev xs
let iter t ~f = List.iter t ~f:(fun (key, data) -> f ~key ~data)

let rec mem t key =
  match t with
  | [] -> false
  | (k, _) :: t -> String.Caseless.equal k key || mem t key
;;

let rec find t key =
  match t with
  | [] -> None
  | (k, v) :: t -> if String.Caseless.equal k key then Some v else find t key
;;

let rec find_multi t key =
  match t with
  | [] -> []
  | (k, v) :: t ->
    if String.Caseless.equal k key then v :: find_multi t key else find_multi t key
;;

let empty = []
let add_unless_exists t ~key ~data = if not (mem t key) then (key, data) :: t else t
