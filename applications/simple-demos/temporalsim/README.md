# temporalsim

## Overview

Démo simple d’automate temporel avec visualisation et monitoring logique.

## Controls / Inputs

- Contrôles affichés dans l’interface (Run/Pause/Step/Reset, cycle de formules).
- `ESC`: quitter.

## Launch Commands

Depuis `/Users/fredericdabrowski/Repos/tempo`:

```bash
dune exec ./applications/run -- temporalsim
```

Direct:

```bash
dune exec ./applications/simple-demos/temporalsim/run
```

## CLI Options

- Pas d'options CLI publiques documentées.

## Dependencies

- OCaml 5.x, dune, raylib

## Headless / Reproducible Mode

- Pas de mode headless public standardisé.

## Troubleshooting

- Si le HUD est vide: relancer et vérifier que la fenêtre a le focus.

## Release Status

- Category: `simple-demos`
- Quality status: `ready`
- Last validation date: 2026-02-16
- Validated commands:
- `dune exec ./applications/run -- temporalsim`
