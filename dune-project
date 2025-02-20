(lang dune 3.17)

(name fpp_personal_parsers)

(generate_opam_files true)

(source
 (github pixilcode/fpp_personal_parsers))

(authors "Brendon Bown <brendon.bown@gmail.com>")

(license MIT)

(package
 (name fpp_personal_parsers)
 (synopsis
  "A set of personal parsers that use the fixed-point parsing framework")
 (depends ocaml cmdliner core ounit2 ppx_jane))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
