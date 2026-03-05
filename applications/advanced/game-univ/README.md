# game-univ

## Overview

Jeu 2D Tempo + Raylib: vous incarnez un professeur qui doit détecter les étudiants en train de tricher.

## Pourquoi cette application est une vitrine Tempo

`game-univ` met l'accent sur les fonctionnalités du coeur Tempo plutôt que sur
`tempo-app`.

- orchestration principale avec `Tempo.parallel`
- supervision avec `Tempo.Constructs.supervise_until`
- runtime interactif via `Tempo.run_interactive`
- sous-systèmes séparés et synchronisés par signaux agrégés :
  - contrôle
  - horloge de manche
  - agents étudiants
  - suspense / suspicion
  - feedback visuel
  - logique audio
  - rendu de frame
- usage explicite de `Tempo.Constructs.every_n` et `Tempo.Constructs.timeout`
  pour des patterns temporels lisibles

`Tempo_jobs` n'est volontairement pas utilisé ici : l'application n'a pas de
vrai besoin de travail externe parallèle. Le garder hors du chemin principal
évite d'ajouter une complexité qui ne sert pas le gameplay.

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

Runner headless disponible :

```bash
dune exec ./applications/advanced/game-univ/src/headless_runner.exe
```

Il sert à vérifier rapidement que l'orchestration Tempo produit bien des frames,
des commandes HUD et des événements audio sans dépendre de Raylib.

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
