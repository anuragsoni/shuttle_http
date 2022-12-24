(* https://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml *)
type informational =
  [ `Continue (* [RFC7231, Section 6.2.1] *)
  | `Switching_protocols (* [RFC7231, Section 6.2.2] *)
  | `Processing (* [RFC2518] *)
  | `Early_hints (* [RFC8297] *)
  ]
[@@deriving sexp, compare, hash]

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

type t =
  [ informational
  | success
  | redirection
  | client_error
  | server_error
  ]
[@@deriving sexp, compare, hash]

val to_int : t -> int
val to_string : t -> string
val to_reason_phrase : t -> string
