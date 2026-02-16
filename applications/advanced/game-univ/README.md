# game-univ

## Overview

Jeu 2D Tempo + Raylib: vous incarnez un professeur qui doit détecter les étudiants en train de tricher.

## Controls / Inputs

- Clavier:
- `Fleches`: déplacement
- `E`: interroger l'étudiant le plus proche
- `P`: pause/reprise
- `R`: recommencer
- `C`: interaction café
- `Esc`: quitter
- Souris:
- boutons HUD (`Start`, `Pause`, `Recommencer`, etc.)

## Launch Commands

Depuis `/Users/fredericdabrowski/Repos/tempo`:

```bash
dune exec ./applications/run -- game-univ
```

Lancement direct:

```bash
dune exec ./applications/advanced/game-univ/run
```

## CLI Options

- Pas d'options CLI documentées pour le mode interactif.

## Dependencies

- OCaml 5.x, dune
- `raylib` (bindings OCaml) pour le rendu/audio

## Headless / Reproducible Mode

- Pas de mode headless public standardisé.

## Troubleshooting

- Si la fenêtre ne s'ouvre pas au premier lancement: relancer après `dune build`.
- Si le son est absent: vérifier sortie audio système et mute application.
- Si capture vidéo macOS: vérifier permission Screen Recording.

## Release Status

- Category: `advanced`
- Quality status: `ready`
- Last validation date: 2026-02-16
- Validated commands:
- `dune exec ./applications/run -- game-univ`
- `dune exec ./applications/advanced/game-univ/run`
