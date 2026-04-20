# boids-raylib

## Overview

Démo simple Tempo + Raylib de simulation de boids (flocking).

## Controls / Inputs

- `SPACE`: pause/reprise
- `R`: reset
- `ESC`: quitter

## Launch Commands

Depuis `/Users/fredericdabrowski/Repos/tempo`:

```bash
dune exec ./applications/run -- boids-raylib
```

Direct:

```bash
dune exec ./applications/simple-demos/boids-raylib/run
```

## CLI Options

- Pas d'options CLI publiques documentées.

## Dependencies

- OCaml 5.x, dune, raylib

## Headless / Reproducible Mode

- Pas de mode headless public standardisé.

## Troubleshooting

- Si écran noir au premier run, relancer après `dune build`.

## Release Status

- Category: `simple-demos`
- Quality status: `ready`
- Last validation date: 2026-02-16
- Validated commands:
- `dune exec ./applications/run -- boids-raylib`
