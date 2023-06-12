(lang dune 2.8)

(name shuttle_http)

(generate_opam_files true)

(source
 (github anuragsoni/shuttle))

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
  (async_ssl (>= v0.15.0))
  (re2
   (>= v0.15.0))
  (ppx_jane :with-test)
  (core_unix :with-test)))
