(lang dune 3.19)

(name tempo)

(version "0.1.0")

(generate_opam_files true)

(source
 (github DabrowskiFr/tempo))

(authors "Frederic Dabrowski <frederic.dabrowski@univ-orleans.fr>")

(maintainers "Frederic Dabrowski <frederic.dabrowski@univ-orleans.fr>")

(license "GPL-3.0-or-later")

(documentation https://url/to/documentation)

(package
 (name tempo)
 (synopsis "A library for reactive programming in OCaml")
 (description "Tempo is a library for reactive programming in OCaml, based on algebraic effects. It provides constructs for defining and composing reactive processes that can respond to events over time.")
 (depends (ocaml (>= 5.3.0)) (dune (>= 3.0))))
