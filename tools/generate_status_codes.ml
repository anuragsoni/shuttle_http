open Core

module Csv_line = struct
  type t =
    { value : int option
    ; description : string
    ; reference : string
    }
  [@@deriving sexp]

  let spec =
    let open Delimited_kernel.Read.Let_syntax in
    let%map_open value =
      at_header "Value" ~f:(fun v ->
          try Some (Int.of_string v) with
          | _ -> None)
    and description = at_header "Description" ~f:Fn.id
    and reference = at_header "Reference" ~f:Fn.id in
    { value; description; reference }
  ;;

  let lines_from_file name =
    In_channel.with_file name ~f:(fun chan ->
        Delimited_kernel.Read.read_lines ~header:`Yes spec chan)
  ;;
end

module Status_code = struct
  module Kind = struct
    module T = struct
      type t =
        | Informational
        | Success
        | Redirection
        | Client_error
        | Server_error
      [@@deriving sexp, compare]
    end

    include T

    let of_code c =
      if c >= 100 && c < 200
      then Informational
      else if c >= 200 && c < 300
      then Success
      else if c >= 300 && c < 400
      then Redirection
      else if c >= 400 && c < 500
      then Client_error
      else if c >= 500 && c < 600
      then Server_error
      else failwith "Invalid Status Code"
    ;;

    include Comparable.Make (T)
  end

  type status =
    { kind : Kind.t
    ; code : int
    ; reason_phrase : string
    ; label : string
    ; reference : string
    }
  [@@deriving sexp]

  let format_description desc =
    String.lowercase desc
    |> String.map ~f:(function
           | ' ' -> '_'
           | '-' -> '_'
           | ch when Char.is_alpha ch -> ch
           | c -> failwith @@ sprintf "Invalid description: %C" c)
    |> String.capitalize
  ;;

  let status_of_csv_line { Csv_line.value; description; reference } =
    let open Option.Let_syntax in
    let%bind code = value in
    if String.("(Unused)" = description || "Unassigned" = description)
    then None
    else
      Some
        { kind = Kind.of_code code
        ; code
        ; label = format_description description
        ; reason_phrase = description
        ; reference
        }
  ;;

  type t =
    { informational : status list
    ; success : status list
    ; redirection : status list
    ; client_error : status list
    ; server_error : status list
    }
  [@@deriving sexp]

  let sort_by_code xs =
    let compare a b = Int.compare a.code b.code in
    List.sort ~compare xs
  ;;

  let of_csv_lines lines =
    lines
    |> List.map ~f:status_of_csv_line
    |> List.filter_opt
    |> List.fold
         ~init:
           { informational = []
           ; success = []
           ; redirection = []
           ; client_error = []
           ; server_error = []
           }
         ~f:(fun acc status ->
           match status.kind with
           | Kind.Informational ->
             { acc with informational = status :: acc.informational }
           | Success -> { acc with success = status :: acc.success }
           | Redirection -> { acc with redirection = status :: acc.redirection }
           | Client_error -> { acc with client_error = status :: acc.client_error }
           | Server_error -> { acc with server_error = status :: acc.server_error })
    |> fun { informational; success; redirection; client_error; server_error } ->
    { informational = sort_by_code informational
    ; success = sort_by_code success
    ; redirection = sort_by_code redirection
    ; client_error = sort_by_code client_error
    ; server_error = sort_by_code server_error
    }
  ;;
end

let process_csv file is_mli =
  let status_codes = Csv_line.lines_from_file file |> Status_code.of_csv_lines in
  let codes_to_type kind codes =
    let lines =
      List.concat
        [ [ sprintf "type %s = [" kind ]
        ; List.map codes ~f:(fun c ->
              sprintf
                "| `%s %s"
                c.Status_code.label
                (if String.(c.reference = "") then "" else sprintf "(* %s *)" c.reference))
        ; [ "] [@@deriving sexp]" ]
        ]
    in
    Out_channel.output_lines stdout lines;
    Out_channel.newline stdout
  in
  let status_to_code kind codes =
    let lines =
      List.concat
        [ [ sprintf "let %s_to_code = function" kind ]
        ; List.map codes ~f:(fun status ->
              sprintf "| `%s -> %d" status.Status_code.label status.code)
        ]
    in
    Out_channel.output_lines stdout lines;
    Out_channel.newline stdout
  in
  let status_to_string kind codes =
    let lines =
      List.concat
        [ [ sprintf "let %s_to_string = function" kind ]
        ; List.map codes ~f:(fun status ->
              sprintf "| `%s -> %S" status.Status_code.label (Int.to_string status.code))
        ]
    in
    Out_channel.output_lines stdout lines;
    Out_channel.newline stdout
  in
  let status_to_reason_phrase kind codes =
    let lines =
      List.concat
        [ [ sprintf "let %s_to_reason_phrase = function" kind ]
        ; List.map codes ~f:(fun status ->
              sprintf "| `%s -> %S" status.Status_code.label status.reason_phrase)
        ]
    in
    Out_channel.output_lines stdout lines;
    Out_channel.newline stdout
  in
  let generate_for_kind kind codes =
    codes_to_type kind codes;
    if not is_mli
    then (
      status_to_code kind codes;
      status_to_string kind codes;
      status_to_reason_phrase kind codes)
  in
  Out_channel.output_lines
    stdout
    [ "(* https://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml *)" ];
  generate_for_kind "informational" status_codes.informational;
  generate_for_kind "success" status_codes.success;
  generate_for_kind "redirection" status_codes.redirection;
  generate_for_kind "client_error" status_codes.client_error;
  generate_for_kind "server_error" status_codes.server_error;
  Out_channel.output_lines
    stdout
    [ "type t = ["
    ; "| informational"
    ; "| success"
    ; "| redirection"
    ; "| client_error"
    ; "| server_error"
    ; "] [@@deriving sexp]"
    ];
  Out_channel.newline stdout;
  let codebranch_for_kind kind = sprintf "| #%s as c -> %s_to_code c" kind kind in
  let codebranch_for_reason_phrase kind =
    sprintf "| #%s as c -> %s_to_reason_phrase c" kind kind
  in
  let codebranch_for_to_string kind = sprintf "| #%s as c -> %s_to_string c" kind kind in
  if not is_mli
  then (
    Out_channel.output_lines
      stdout
      ("let to_code = function"
      :: List.map
           [ "informational"; "success"; "redirection"; "client_error"; "server_error" ]
           ~f:codebranch_for_kind);
    Out_channel.output_lines
      stdout
      ("let to_string = function"
      :: List.map
           [ "informational"; "success"; "redirection"; "client_error"; "server_error" ]
           ~f:codebranch_for_to_string);
    Out_channel.output_lines
      stdout
      ("let to_reason_phrase = function"
      :: List.map
           [ "informational"; "success"; "redirection"; "client_error"; "server_error" ]
           ~f:codebranch_for_reason_phrase))
  else
    Out_channel.output_lines
      stdout
      [ "val to_code : t -> int"
      ; "val to_reason_phrase : t -> string"
      ; "val to_string : t -> string"
      ]
;;

let command =
  Command.basic
    ~summary:"Process csv containing http status codes"
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: Filename.arg_type)
      and mli = flag "-mli" no_arg ~doc:" Generate mli" in
      fun () -> process_csv filename mli)
;;

let () = Command.run command
