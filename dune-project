(lang dune 3.16)

(name hellocaml)

(generate_opam_files true)

(source
 (github 27theo/hellocaml))

(authors "Theo Addis")

(maintainers "Theo Addis")

(license LICENSE)

(package
 (name hellocaml)
 (synopsis "A simple TODO app")
 (description "TODO app with persistence and export")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
