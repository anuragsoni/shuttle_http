open Core

type t = (string, string) List.Assoc.t [@@deriving sexp]

let of_rev_list xs = xs
let of_list xs = List.rev xs
let to_rev_list xs = xs
let to_list xs = List.rev xs
let iter t ~f = List.iter t ~f:(fun (key, data) -> f ~key ~data)

let rec mem t key =
  match t with
  | [] -> false
  | (k, _) :: t -> String.Caseless.equal k key || mem t key

let rec find t key =
  match t with
  | [] -> None
  | (k, v) :: t -> if String.Caseless.equal k key then Some v else find t key

let rec find_multi t key =
  match t with
  | [] -> []
  | (k, v) :: t ->
    if String.Caseless.equal k key then v :: find_multi t key else find_multi t key

let empty = []

let is_empty = function
  | [] -> true
  | _ -> false

let add_unless_exists t ~key ~data = if not (mem t key) then (key, data) :: t else t
let add t ~key ~data = (key, data) :: t

exception Stop

let remove t name =
  let rec loop t name seen =
    match t with
    | [] -> if seen then [] else raise_notrace Stop
    | ((key, _) as x) :: xs ->
      if String.Caseless.equal key name then loop xs name true else x :: loop xs name seen
  in
  try loop t name false with
  | Stop -> t
