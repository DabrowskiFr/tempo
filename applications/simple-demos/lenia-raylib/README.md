# lenia-raylib

## Overview

Démo simple Lenia (automate cellulaire continu) avec rendu Raylib et comparaison moteur matriciel vs moteur processus.

## Controls / Inputs

- `SPACE`: pause/reprise
- `R`: reset
- `Q`/`E`: preset précédent/suivant
- `TAB` (ou `M`): basculer `matriciel` <-> `processus`
- `ESC`: quitter

## Launch Commands

Depuis `/Users/fredericdabrowski/Repos/tempo`:

```bash
dune exec ./applications/run -- lenia-raylib
```

Direct:

```bash
dune exec ./applications/simple-demos/lenia-raylib/run
```

## CLI Options

- Pas d'options CLI publiques documentées.

## Dependencies

- OCaml 5.x, dune, raylib

## Headless / Reproducible Mode

- Pas de mode headless public standardisé.

## Troubleshooting

- Si mode `processus` devient lent: relancer et rester en mode `matriciel` pour machine limitée.

## Release Status

- Category: `simple-demos`
- Quality status: `ready`
- Last validation date: 2026-02-16
- Validated commands:
- `dune exec ./applications/run -- lenia-raylib`
