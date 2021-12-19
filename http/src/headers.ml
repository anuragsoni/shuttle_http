open! Base

type t = (string, string) List.Assoc.t [@@deriving sexp]

let empty = []
let of_rev_list xs = xs
let to_rev_list xs = xs
let of_list xs = List.rev xs
let to_list xs = List.rev xs
let iter xs ~f = List.iter ~f:(fun (key, data) -> f ~key ~data) xs
let fold xs ~init ~f = List.fold xs ~init ~f:(fun acc (key, data) -> f acc ~key ~data)
let find t key = List.Assoc.find t ~equal:String.Caseless.equal key

let find_multi t key =
  let rec aux acc = function
    | [] -> List.rev acc
    | (k, v) :: xs when String.Caseless.equal key k -> aux (v :: acc) xs
    | _ :: xs -> aux acc xs
  in
  aux [] t
;;
