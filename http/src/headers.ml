open Import
module List = ListLabels

type t = (string * string) list

let empty = []
let of_rev_list xs = xs
let to_rev_list xs = xs
let of_list xs = List.rev xs
let to_list xs = List.rev xs
let iter xs ~f = List.iter ~f:(fun (key, data) -> f ~key ~data) xs

let fold xs ~init ~f =
  List.fold_left xs ~init ~f:(fun acc (key, data) -> f acc ~key ~data)
;;

let mem t key =
  let rec loop t =
    match t with
    | [] -> false
    | (k, _) :: ts -> if caseless_equal k key then true else loop ts
  in
  loop t
;;

let find t key =
  let rec loop t =
    match t with
    | [] -> None
    | (k, v) :: ts -> if caseless_equal k key then Some v else loop ts
  in
  loop t
;;

let find_multi t key =
  let rec aux acc = function
    | [] -> List.rev acc
    | (k, v) :: xs when caseless_equal key k -> aux (v :: acc) xs
    | _ :: xs -> aux acc xs
  in
  aux [] t
;;

let add_if_missing t ~key ~data = if mem t key then t else (key, data) :: t
