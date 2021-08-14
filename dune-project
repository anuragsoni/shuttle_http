(lang dune 2.8)

(name shuttle)

(generate_opam_files true)

(source
 (github anuragsoni/shuttle))

(maintainers "Anurag Soni <anurag@sonianurag.com>")

(license MIT)

(package
 (name shuttle)
 (tags
  (async reader writer))
 (synopsis "Buffered reader/writer for async")
 (depends
  (ocaml
   (>= 4.11.0))
  core
  async
  faraday
  ppx_jane))
