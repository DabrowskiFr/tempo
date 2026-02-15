# Cheat Detector Game (Tempo + Raylib)

Jeu 2D OCaml où le joueur incarne un professeur qui détecte les étudiants qui trichent.

## Contrôles

- Flèches : déplacement du professeur
- E : interroger l'étudiant le plus proche
- Échap / fermeture fenêtre : quitter

## Build

```bash
opam install dune raylib
opam pin add tempo /Users/fredericdabrowski/Repos/tempo
cd /Users/fredericdabrowski/Repos/tempo/game-univ
dune build
dune exec cheat_detector_game
```

## Architecture

- `Professor.process`
- `Student.process`
- `Suspicion.process`
- `Question.process`
- `Render.flush`

Un instant Tempo correspond à une frame Raylib.
