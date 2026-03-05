(lang dune 3.19)

(name tempo)

(version "0.2.0")

(generate_opam_files true)

(source
 (github DabrowskiFr/tempo))

(authors "Frederic Dabrowski <frederic.dabrowski@univ-orleans.fr>")

(maintainers "Frederic Dabrowski <frederic.dabrowski@univ-orleans.fr>")

(license "GPL-3.0-or-later")

(documentation https://dabrowskifr.github.io/tempo/)

(package
 (name tempo)
 (synopsis "A library for reactive programming in OCaml")
 (description "Tempo is a library for reactive programming in OCaml, based on algebraic effects. It provides constructs for defining and composing reactive processes that can respond to events over time.")
 (depends (ocaml (>= 5.3.0)) (dune (>= 3.0)) (logs (>= 0.10.0)) (mtime (>= 2.1.0))))

(package
 (name tempo-raylib)
 (synopsis "Raylib helpers for Tempo demos and graphical applications")
 (description "Optional Raylib integration layer for Tempo. It provides lightweight UI/HUD/FX/audio helpers and rendering backends for graphical Tempo applications.")
 (depends
  (ocaml (>= 5.3.0))
  (dune (>= 3.0))
  (tempo (= :version))
  raylib))

(package
 (name tempo-frp)
 (synopsis "FRP and signal-function encoding on top of Tempo")
 (description "Optional FRP/SF encoding built on top of Tempo. This package is specialized and not the main recommended entry point to the runtime.")
 (depends
  (ocaml (>= 5.3.0))
  (dune (>= 3.0))
  (tempo (= :version))))

(package
 (name tempo-app)
 (synopsis "Application-oriented helpers for Tempo")
 (description "tempo-app provides App, Loop and Scene helpers built on top of the Tempo runtime.")
 (depends
  (ocaml (>= 5.3.0))
  (dune (>= 3.0))
  (tempo (= :version))))

(package
 (name tempo-jobs)
 (synopsis "Parallel external jobs bridged into Tempo")
 (description "tempo-jobs runs external jobs on OCaml domains and imports their progress and completion into Tempo at inter-instant boundaries.")
 (depends
  (ocaml (>= 5.3.0))
  (dune (>= 3.0))
  (tempo (= :version))
  base-threads))

(package
 (name tempo-score)
 (synopsis "Tempo-native musical score model and MIDI conversion")
 (description "Converts MIDI files into a Tempo logical score representation (units, note spans, voices) for scheduling notes with Tempo processes.")
 (depends
  (ocaml (>= 5.3.0))
  (dune (>= 3.0))
  (tempo (= :version))
  dune-configurator
  ctypes
  ctypes-foreign
  base-unix))
