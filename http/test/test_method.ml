open Core
open Shuttle_http

let%test_unit "Http Methods can be coverted to strings and back" =
  let a = Meth.all in
  let b =
    a
    |> List.map ~f:Meth.to_string
    |> List.map ~f:(fun v -> Option.value_exn (Meth.of_string v))
  in
  [%test_result: Meth.t list] ~expect:a b
;;
