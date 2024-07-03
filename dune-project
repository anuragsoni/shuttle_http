(lang dune 3.1)

(name shuttle_http)

(generate_opam_files true)

(source
 (github anuragsoni/shuttle_http))

(maintainers "Anurag Soni <anurag@sonianurag.com>")

(authors "Anurag Soni")

(license MIT)

(package
 (name shuttle_http)
 (tags
  (http-server http-client http http1.1 async))
 (synopsis "Async library for HTTP/1.1 servers and clients")
 (description
  "Shuttle_http is a low level library for implementing HTTP/1.1 web services and clients in OCaml.")
 (depends
  (async
   (>= v0.17.0))
  (async_log
   (>= v0.17.0))
  (async_ssl
   (>= v0.17.0))
  (core
   (>= v0.17.0))
  (jane_rope
   (>= v0.17.0))
  (ocaml
   (>= 5.1.0))
  (ppx_jane
   (>= v0.17.0))
  (re2
   (>= v0.17.0))
  (core_unix :with-test)))

(package
 (name shuttle_websocket)
 (tags
  (http-server websocket))
 (synopsis "Websocket support for HTTP/1.1 servers using Async")
 (description
  "Shuttle_websocket is a companion library for shuttle_http that provides a HTTP service that performs websocket negotiation for HTTP/1.1 servers.")
 (depends
  (shuttle_http
   (= :version))
  (async_websocket
    (>= v0.17.0))))
