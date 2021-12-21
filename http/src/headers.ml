module List = ListLabels

let caseless_equal a b =
  if a == b
  then true
  else (
    let len = String.length a in
    len = String.length b
    &&
    let stop = ref false in
    let idx = ref 0 in
    while (not !stop) && !idx < len do
      let c1 = String.unsafe_get a !idx in
      let c2 = String.unsafe_get b !idx in
      if Char.lowercase_ascii c1 <> Char.lowercase_ascii c2 then stop := true;
      incr idx
    done;
    not !stop)
;;

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
