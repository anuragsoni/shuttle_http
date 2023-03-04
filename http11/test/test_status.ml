open Core
open Shuttle_http

let%test_unit "Http Status can be coverted to int and back" =
  let a = Status.all in
  let b =
    a
    |> List.map ~f:Status.to_int
    |> List.map ~f:(fun v -> Or_error.ok_exn (Status.of_int v))
  in
  [%test_result: Status.t list] ~expect:a b
;;

let%test_unit "Status.of_string (Int.to_string (Status.to_int c)) always succeeds" =
  let a = Status.all in
  let b =
    a
    |> List.map ~f:Status.to_int
    |> List.map ~f:Int.to_string
    |> List.map ~f:(fun v -> Or_error.ok_exn (Status.of_string v))
  in
  [%test_result: Status.t list] ~expect:a b
;;

let%test_unit "Status.of_int (Int.of_string (Status.to_string c)) always succeeds" =
  let a = Status.all in
  let b =
    a
    |> List.map ~f:Status.to_string
    |> List.map ~f:Int.of_string
    |> List.map ~f:(fun v -> Or_error.ok_exn (Status.of_int v))
  in
  [%test_result: Status.t list] ~expect:a b
;;

let%test_unit "Status.of_int (Status.to_int c) always succeeds" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: Status.t]
    Status.quickcheck_generator
    ~f:(fun status ->
    [%test_result: Status.t Or_error.t]
      ~expect:(Ok status)
      (Status.of_int (Status.to_int status)))
;;
