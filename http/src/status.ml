(* https://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml *)
type informational =
  [ `Continue (* [RFC7231, Section 6.2.1] *)
  | `Switching_protocols (* [RFC7231, Section 6.2.2] *)
  | `Processing (* [RFC2518] *)
  | `Early_hints (* [RFC8297] *)
  ]
[@@deriving sexp, compare, hash]

let informational_to_code = function
  | `Continue -> 100
  | `Switching_protocols -> 101
  | `Processing -> 102
  | `Early_hints -> 103
;;

let informational_to_string = function
  | `Continue -> "100"
  | `Switching_protocols -> "101"
  | `Processing -> "102"
  | `Early_hints -> "103"
;;

let informational_to_reason_phrase = function
  | `Continue -> "Continue"
  | `Switching_protocols -> "Switching Protocols"
  | `Processing -> "Processing"
  | `Early_hints -> "Early Hints"
;;

type success =
  [ `Ok (* [RFC7231, Section 6.3.1] *)
  | `Created (* [RFC7231, Section 6.3.2] *)
  | `Accepted (* [RFC7231, Section 6.3.3] *)
  | `Non_authoritative_information (* [RFC7231, Section 6.3.4] *)
  | `No_content (* [RFC7231, Section 6.3.5] *)
  | `Reset_content (* [RFC7231, Section 6.3.6] *)
  | `Partial_content (* [RFC7233, Section 4.1] *)
  | `Multi_status (* [RFC4918] *)
  | `Already_reported (* [RFC5842] *)
  | `Im_used (* [RFC3229] *)
  ]
[@@deriving sexp, compare, hash]

let success_to_code = function
  | `Ok -> 200
  | `Created -> 201
  | `Accepted -> 202
  | `Non_authoritative_information -> 203
  | `No_content -> 204
  | `Reset_content -> 205
  | `Partial_content -> 206
  | `Multi_status -> 207
  | `Already_reported -> 208
  | `Im_used -> 226
;;

let success_to_string = function
  | `Ok -> "200"
  | `Created -> "201"
  | `Accepted -> "202"
  | `Non_authoritative_information -> "203"
  | `No_content -> "204"
  | `Reset_content -> "205"
  | `Partial_content -> "206"
  | `Multi_status -> "207"
  | `Already_reported -> "208"
  | `Im_used -> "226"
;;

let success_to_reason_phrase = function
  | `Ok -> "OK"
  | `Created -> "Created"
  | `Accepted -> "Accepted"
  | `Non_authoritative_information -> "Non-Authoritative Information"
  | `No_content -> "No Content"
  | `Reset_content -> "Reset Content"
  | `Partial_content -> "Partial Content"
  | `Multi_status -> "Multi-Status"
  | `Already_reported -> "Already Reported"
  | `Im_used -> "IM Used"
;;

type redirection =
  [ `Multiple_choices (* [RFC7231, Section 6.4.1] *)
  | `Moved_permanently (* [RFC7231, Section 6.4.2] *)
  | `Found (* [RFC7231, Section 6.4.3] *)
  | `See_other (* [RFC7231, Section 6.4.4] *)
  | `Not_modified (* [RFC7232, Section 4.1] *)
  | `Use_proxy (* [RFC7231, Section 6.4.5] *)
  | `Temporary_redirect (* [RFC7231, Section 6.4.7] *)
  | `Permanent_redirect (* [RFC7538] *)
  ]
[@@deriving sexp, compare, hash]

let redirection_to_code = function
  | `Multiple_choices -> 300
  | `Moved_permanently -> 301
  | `Found -> 302
  | `See_other -> 303
  | `Not_modified -> 304
  | `Use_proxy -> 305
  | `Temporary_redirect -> 307
  | `Permanent_redirect -> 308
;;

let redirection_to_string = function
  | `Multiple_choices -> "300"
  | `Moved_permanently -> "301"
  | `Found -> "302"
  | `See_other -> "303"
  | `Not_modified -> "304"
  | `Use_proxy -> "305"
  | `Temporary_redirect -> "307"
  | `Permanent_redirect -> "308"
;;

let redirection_to_reason_phrase = function
  | `Multiple_choices -> "Multiple Choices"
  | `Moved_permanently -> "Moved Permanently"
  | `Found -> "Found"
  | `See_other -> "See Other"
  | `Not_modified -> "Not Modified"
  | `Use_proxy -> "Use Proxy"
  | `Temporary_redirect -> "Temporary Redirect"
  | `Permanent_redirect -> "Permanent Redirect"
;;

type client_error =
  [ `Bad_request (* [RFC7231, Section 6.5.1] *)
  | `Unauthorized (* [RFC7235, Section 3.1] *)
  | `Payment_required (* [RFC7231, Section 6.5.2] *)
  | `Forbidden (* [RFC7231, Section 6.5.3] *)
  | `Not_found (* [RFC7231, Section 6.5.4] *)
  | `Method_not_allowed (* [RFC7231, Section 6.5.5] *)
  | `Not_acceptable (* [RFC7231, Section 6.5.6] *)
  | `Proxy_authentication_required (* [RFC7235, Section 3.2] *)
  | `Request_timeout (* [RFC7231, Section 6.5.7] *)
  | `Conflict (* [RFC7231, Section 6.5.8] *)
  | `Gone (* [RFC7231, Section 6.5.9] *)
  | `Length_required (* [RFC7231, Section 6.5.10] *)
  | `Precondition_failed (* [RFC7232, Section 4.2][RFC8144, Section 3.2] *)
  | `Payload_too_large (* [RFC7231, Section 6.5.11] *)
  | `Uri_too_long (* [RFC7231, Section 6.5.12] *)
  | `Unsupported_media_type (* [RFC7231, Section 6.5.13][RFC7694, Section 3] *)
  | `Range_not_satisfiable (* [RFC7233, Section 4.4] *)
  | `Expectation_failed (* [RFC7231, Section 6.5.14] *)
  | `Misdirected_request (* [RFC7540, Section 9.1.2] *)
  | `Unprocessable_entity (* [RFC4918] *)
  | `Locked (* [RFC4918] *)
  | `Failed_dependency (* [RFC4918] *)
  | `Too_early (* [RFC8470] *)
  | `Upgrade_required (* [RFC7231, Section 6.5.15] *)
  | `Precondition_required (* [RFC6585] *)
  | `Too_many_requests (* [RFC6585] *)
  | `Request_header_fields_too_large (* [RFC6585] *)
  | `Unavailable_for_legal_reasons (* [RFC7725] *)
  ]
[@@deriving sexp, compare, hash]

let client_error_to_code = function
  | `Bad_request -> 400
  | `Unauthorized -> 401
  | `Payment_required -> 402
  | `Forbidden -> 403
  | `Not_found -> 404
  | `Method_not_allowed -> 405
  | `Not_acceptable -> 406
  | `Proxy_authentication_required -> 407
  | `Request_timeout -> 408
  | `Conflict -> 409
  | `Gone -> 410
  | `Length_required -> 411
  | `Precondition_failed -> 412
  | `Payload_too_large -> 413
  | `Uri_too_long -> 414
  | `Unsupported_media_type -> 415
  | `Range_not_satisfiable -> 416
  | `Expectation_failed -> 417
  | `Misdirected_request -> 421
  | `Unprocessable_entity -> 422
  | `Locked -> 423
  | `Failed_dependency -> 424
  | `Too_early -> 425
  | `Upgrade_required -> 426
  | `Precondition_required -> 428
  | `Too_many_requests -> 429
  | `Request_header_fields_too_large -> 431
  | `Unavailable_for_legal_reasons -> 451
;;

let client_error_to_string = function
  | `Bad_request -> "400"
  | `Unauthorized -> "401"
  | `Payment_required -> "402"
  | `Forbidden -> "403"
  | `Not_found -> "404"
  | `Method_not_allowed -> "405"
  | `Not_acceptable -> "406"
  | `Proxy_authentication_required -> "407"
  | `Request_timeout -> "408"
  | `Conflict -> "409"
  | `Gone -> "410"
  | `Length_required -> "411"
  | `Precondition_failed -> "412"
  | `Payload_too_large -> "413"
  | `Uri_too_long -> "414"
  | `Unsupported_media_type -> "415"
  | `Range_not_satisfiable -> "416"
  | `Expectation_failed -> "417"
  | `Misdirected_request -> "421"
  | `Unprocessable_entity -> "422"
  | `Locked -> "423"
  | `Failed_dependency -> "424"
  | `Too_early -> "425"
  | `Upgrade_required -> "426"
  | `Precondition_required -> "428"
  | `Too_many_requests -> "429"
  | `Request_header_fields_too_large -> "431"
  | `Unavailable_for_legal_reasons -> "451"
;;

let client_error_to_reason_phrase = function
  | `Bad_request -> "Bad Request"
  | `Unauthorized -> "Unauthorized"
  | `Payment_required -> "Payment Required"
  | `Forbidden -> "Forbidden"
  | `Not_found -> "Not Found"
  | `Method_not_allowed -> "Method Not Allowed"
  | `Not_acceptable -> "Not Acceptable"
  | `Proxy_authentication_required -> "Proxy Authentication Required"
  | `Request_timeout -> "Request Timeout"
  | `Conflict -> "Conflict"
  | `Gone -> "Gone"
  | `Length_required -> "Length Required"
  | `Precondition_failed -> "Precondition Failed"
  | `Payload_too_large -> "Payload Too Large"
  | `Uri_too_long -> "URI Too Long"
  | `Unsupported_media_type -> "Unsupported Media Type"
  | `Range_not_satisfiable -> "Range Not Satisfiable"
  | `Expectation_failed -> "Expectation Failed"
  | `Misdirected_request -> "Misdirected Request"
  | `Unprocessable_entity -> "Unprocessable Entity"
  | `Locked -> "Locked"
  | `Failed_dependency -> "Failed Dependency"
  | `Too_early -> "Too Early"
  | `Upgrade_required -> "Upgrade Required"
  | `Precondition_required -> "Precondition Required"
  | `Too_many_requests -> "Too Many Requests"
  | `Request_header_fields_too_large -> "Request Header Fields Too Large"
  | `Unavailable_for_legal_reasons -> "Unavailable For Legal Reasons"
;;

type server_error =
  [ `Internal_server_error (* [RFC7231, Section 6.6.1] *)
  | `Not_implemented (* [RFC7231, Section 6.6.2] *)
  | `Bad_gateway (* [RFC7231, Section 6.6.3] *)
  | `Service_unavailable (* [RFC7231, Section 6.6.4] *)
  | `Gateway_timeout (* [RFC7231, Section 6.6.5] *)
  | `Http_version_not_supported (* [RFC7231, Section 6.6.6] *)
  | `Variant_also_negotiates (* [RFC2295] *)
  | `Insufficient_storage (* [RFC4918] *)
  | `Loop_detected (* [RFC5842] *)
  | `Not_extended (* [RFC2774] *)
  | `Network_authentication_required (* [RFC6585] *)
  ]
[@@deriving sexp, compare, hash]

let server_error_to_code = function
  | `Internal_server_error -> 500
  | `Not_implemented -> 501
  | `Bad_gateway -> 502
  | `Service_unavailable -> 503
  | `Gateway_timeout -> 504
  | `Http_version_not_supported -> 505
  | `Variant_also_negotiates -> 506
  | `Insufficient_storage -> 507
  | `Loop_detected -> 508
  | `Not_extended -> 510
  | `Network_authentication_required -> 511
;;

let server_error_to_string = function
  | `Internal_server_error -> "500"
  | `Not_implemented -> "501"
  | `Bad_gateway -> "502"
  | `Service_unavailable -> "503"
  | `Gateway_timeout -> "504"
  | `Http_version_not_supported -> "505"
  | `Variant_also_negotiates -> "506"
  | `Insufficient_storage -> "507"
  | `Loop_detected -> "508"
  | `Not_extended -> "510"
  | `Network_authentication_required -> "511"
;;

let server_error_to_reason_phrase = function
  | `Internal_server_error -> "Internal Server Error"
  | `Not_implemented -> "Not Implemented"
  | `Bad_gateway -> "Bad Gateway"
  | `Service_unavailable -> "Service Unavailable"
  | `Gateway_timeout -> "Gateway Timeout"
  | `Http_version_not_supported -> "HTTP Version Not Supported"
  | `Variant_also_negotiates -> "Variant Also Negotiates"
  | `Insufficient_storage -> "Insufficient Storage"
  | `Loop_detected -> "Loop Detected"
  | `Not_extended -> "Not Extended"
  | `Network_authentication_required -> "Network Authentication Required"
;;

type t =
  [ informational
  | success
  | redirection
  | client_error
  | server_error
  ]
[@@deriving sexp, compare, hash]

let to_int = function
  | #informational as c -> informational_to_code c
  | #success as c -> success_to_code c
  | #redirection as c -> redirection_to_code c
  | #client_error as c -> client_error_to_code c
  | #server_error as c -> server_error_to_code c
;;

let to_string = function
  | #informational as c -> informational_to_string c
  | #success as c -> success_to_string c
  | #redirection as c -> redirection_to_string c
  | #client_error as c -> client_error_to_string c
  | #server_error as c -> server_error_to_string c
;;

let to_reason_phrase = function
  | #informational as c -> informational_to_reason_phrase c
  | #success as c -> success_to_reason_phrase c
  | #redirection as c -> redirection_to_reason_phrase c
  | #client_error as c -> client_error_to_reason_phrase c
  | #server_error as c -> server_error_to_reason_phrase c
;;
