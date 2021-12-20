(lang dune 2.8)

(name shuttle)

(generate_opam_files true)

(source
 (github anuragsoni/shuttle))

(maintainers "Anurag Soni <anurag@sonianurag.com>")

(authors "Anurag Soni")

(license MIT)

(package
 (name shuttle_ssl)
 (tags
  (async reader writer ssl))
 (synopsis "Async_ssl support for shuttle")
 (depends
  (ocaml
   (>= 4.11.0))
  (shuttle
   (= :version))
  (ppx_jane
   (and
    (>= v0.14)
    (< v0.15)))
  (async_ssl
   (and
    (>= v0.14)
    (< v0.15)))))

(package
  (name shuttle_http)
  (tags
    (async http))
  (synopsis "HTTP codec for shuttle")
  (depends
    (ocaml
      (>= 4.11.0))
    sexplib0))

(package
 (name shuttle)
 (tags
  (async reader writer))
 (synopsis "Reasonably performant non-blocking channels for async")
 (depends
  (ocaml
   (>= 4.11.0))
  (async
   (and
    (>= v0.14)
    (< v0.15)))
  (core
   (and
    (>= v0.14)
    (< v0.15)))
  (ppx_jane
   (and
    (>= v0.14)
    (< v0.15)))))
