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
   (>= v0.14))
  (async_ssl
   (>= v0.14))))

(package
 (name shuttle)
 (tags
  (async reader writer))
 (synopsis "Reasonably performant non-blocking channels for async")
 (depends
  (ocaml
   (>= 4.11.0))
  (async
   (>= v0.14))
  (core
   (>= v0.14))
  (core_unix
   (>= v0.14))
  (ppx_jane
   (>= v0.14))))
