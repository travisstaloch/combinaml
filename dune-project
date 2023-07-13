(lang dune 3.8)

(name combinaml)

(generate_opam_files true)

(source
 (github travisstaloch/combinaml))

(authors "Travis Staloch")

(maintainers "Travis Staloch")

(license MIT)

(package
 (name combinaml)
 (synopsis "Customizable, dependency free parser combinator library")
 (description
  "An angstrom clone with no dependencies designed to be customizable")
 (depends
  (ocaml
   (>= 4.08))
  dune
  ppx_expect)
 (tags
  ("parser-combinator" "parser-library" "ocaml")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
