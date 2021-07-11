(lang dune 2.8)

(name shuttle)

(generate_opam_files true)

(package
 (name shuttle)
 (depends
  (ocaml
   (>= 4.11.0))
  core
  async
  ppx_jane))
