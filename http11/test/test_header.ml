open! Core
open Shuttle_http

let%expect_test "header operations" =
  printf !"%{sexp: Headers.t}\n" Headers.empty;
  [%expect {| () |}];
  printf
    !"%{sexp: Headers.t}\n"
    (Headers.empty
     |> Headers.add ~key:"foo" ~data:"bar"
     |> Headers.add ~key:"foo" ~data:"baz"
     |> Headers.add_unless_exists ~key:"foo" ~data:"this won't be added");
  [%expect {| ((foo baz) (foo bar)) |}];
  let headers =
    Headers.of_rev_list [ "foo", "bar"; "hello", "world"; "foo", "second foo" ]
  in
  printf !"%{sexp: Headers.t}" headers;
  [%expect {| ((foo bar) (hello world) (foo "second foo")) |}];
  printf !"%{sexp: Headers.t}" (Headers.remove headers "foo");
  [%expect {| ((hello world)) |}];
  printf !"%{sexp: Headers.t}" (Headers.remove headers "HeaderKeyNotInList");
  [%expect {| ((foo bar) (hello world) (foo "second foo")) |}];
  printf !"%{sexp: string option}" (Headers.find headers "FOO");
  [%expect {| (bar) |}];
  printf !"%{sexp: string list}" (Headers.find_multi headers "FOO");
  [%expect {| (bar "second foo") |}];
  printf !"%{sexp: string option}" (Headers.find headers "BAZ");
  [%expect {| () |}];
  printf !"%{sexp: bool}" (Headers.mem headers "FOO");
  [%expect {| true |}]
;;

let tchar_map =
  Array.init 256 ~f:(fun idx ->
    match Char.of_int_exn idx with
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!'
    | '#'
    | '$'
    | '%'
    | '&'
    | '\''
    | '*'
    | '+'
    | '-'
    | '.'
    | '^'
    | '_'
    | '`'
    | '|'
    | '~' -> true
    | _ -> false)
;;

let tchar_generator =
  let open Base_quickcheck in
  Generator.union
    Generator.
      [ char_digit
      ; char_lowercase
      ; char_uppercase
      ; of_list
          [ '!'; '#'; '$'; '%'; '&'; '\''; '*'; '+'; '-'; '.'; '^'; '_'; '`'; '|'; '~' ]
      ]
;;

let header_name_generator = Base_quickcheck.Generator.string_non_empty_of tchar_generator

let header_generator =
  Base_quickcheck.Generator.map2 header_name_generator String.gen_nonempty ~f:(fun a b ->
    a, b)
;;

let headers_generator =
  let open Base_quickcheck.Generator.Let_syntax in
  let%map xs = List.quickcheck_generator header_generator in
  Headers.of_rev_list xs
;;

let%test_unit "Adding a header to headers always results in a non_empty headers" =
  let gen =
    let open Base_quickcheck.Generator.Let_syntax in
    let%map a = headers_generator
    and b = header_generator in
    a, b
  in
  Quickcheck.test
    ~sexp_of:[%sexp_of: Headers.t * (string * string)]
    gen
    ~f:(fun (headers, (key, data)) ->
    [%test_result: bool] ~expect:false (Headers.is_empty (Headers.add headers ~key ~data)))
;;

let%test_unit "Headers.to_rev_list (Headers.of_rev_list xs) = xs" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: (string * string) list]
    (Base_quickcheck.Generator.list header_generator)
    ~f:(fun keys ->
    [%test_result: (string * string) list]
      ~expect:keys
      (Headers.to_rev_list (Headers.of_rev_list keys)))
;;

let%test_unit "Headers.to_list (Headers.of_list xs) = xs" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: (string * string) list]
    (Base_quickcheck.Generator.list header_generator)
    ~f:(fun keys ->
    [%test_result: (string * string) list]
      ~expect:keys
      (Headers.to_list (Headers.of_list keys)))
;;

let%test_unit "Headers.to_list (Headers.of_rev_list xs) = List.rev xs" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: (string * string) list]
    (Base_quickcheck.Generator.list header_generator)
    ~f:(fun keys ->
    [%test_result: (string * string) list]
      ~expect:(List.rev keys)
      (Headers.to_list (Headers.of_rev_list keys)))
;;

let%test_unit "Header lookups perform case insensitive comparisons" =
  let gen =
    let open Base_quickcheck.Generator.Let_syntax in
    let%map a = headers_generator
    and b = header_name_generator in
    a, b
  in
  Quickcheck.test ~sexp_of:[%sexp_of: Headers.t * string] gen ~f:(fun (headers, key) ->
    [%test_eq: bool]
      (Headers.mem headers key)
      (Headers.mem headers (String.lowercase key));
    [%test_eq: bool]
      (Headers.mem headers key)
      (Headers.mem headers (String.uppercase key)))
;;

let%test_unit "Attempting to remove a header name that doesn't exist in header set does \
               not modify the headers"
  =
  let gen =
    let open Base_quickcheck.Generator.Let_syntax in
    let%map a = headers_generator
    and b = header_name_generator in
    a, b
  in
  Quickcheck.test ~sexp_of:[%sexp_of: Headers.t * string] gen ~f:(fun (headers, key) ->
    if not (Headers.mem headers key)
    then
      [%test_result: (string * string) list]
        ~expect:(Headers.to_rev_list headers)
        (Headers.to_rev_list (Headers.remove headers key)))
;;

let%test_unit "Removing a header name from a list of headers removes all entries with \
               the name"
  =
  let gen =
    let open Base_quickcheck.Generator.Let_syntax in
    let%map a = headers_generator
    and b = header_name_generator in
    a, b
  in
  Quickcheck.test ~sexp_of:[%sexp_of: Headers.t * string] gen ~f:(fun (headers, key) ->
    if (not (Headers.is_empty headers)) && Headers.mem headers key
    then (
      let headers = Headers.remove headers key in
      [%test_result: bool] ~expect:false (Headers.mem headers key)))
;;
