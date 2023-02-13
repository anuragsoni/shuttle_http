open Core
open Shuttle_http

let%test_unit "Http Methods can be coverted to strings and back" =
  let a = Meth.all in
  let b =
    a
    |> List.map ~f:Meth.to_string
    |> List.map ~f:(fun v -> Or_error.ok_exn (Meth.of_string v))
  in
  [%test_result: Meth.t list] ~expect:a b

let%test_unit "Meth.of_string (Meth.to_string m) is never none" =
  Quickcheck.test ~sexp_of:[%sexp_of: Meth.t] Meth.quickcheck_generator ~f:(fun meth ->
    [%test_result: Meth.t Or_error.t]
      ~expect:(Ok meth)
      (Meth.of_string (Meth.to_string meth)))
