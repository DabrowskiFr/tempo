# logicgroove

## Overview

Démo simple de séquenceur logique (monitoring temporel) avec interface Raylib.

## Controls / Inputs

- Contrôles affichés dans l’application (monitor/assist/strict + transport).
- `ESC`: quitter.

## Launch Commands

Depuis `/Users/fredericdabrowski/Repos/tempo`:

```bash
dune exec ./applications/run -- logicgroove
```

Direct:

```bash
dune exec ./applications/simple-demos/logicgroove/run
```

## CLI Options

- Pas d'options CLI publiques documentées.

## Dependencies

- OCaml 5.x, dune, raylib

## Headless / Reproducible Mode

- Pas de mode headless public standardisé.

## Troubleshooting

- Si la fenêtre ne répond plus: relancer après fermeture propre (`ESC`).

## Release Status

- Category: `simple-demos`
- Quality status: `ready`
- Last validation date: 2026-02-16
- Validated commands:
- `dune exec ./applications/run -- logicgroove`
