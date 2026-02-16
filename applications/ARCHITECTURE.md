# Applications Architecture

This document describes the architecture of applications under `/Users/fredericdabrowski/Repos/tempo/applications`.

## Directory model

- `advanced/`
  - applications with higher product/research expectations (`game-univ`, `refactor`).
- `simple-demos/`
  - focused demonstrations of one concept/runtime pattern.

## Runtime execution model

All applications follow the same synchronous loop shape:

1. input acquisition (`input : unit -> 'a option`)
2. Tempo instant execution (`execute`)
3. output emission (`output : 'b -> unit`)

For graphical apps, rendering adapters convert platform events to Tempo input and project Tempo output to draw/audio calls.

## Integration boundaries

- Tempo core:
  - synchronous semantics, scheduling, signals, guards, watch/parallel.
- App logic:
  - domain state transitions and gameplay/simulation rules.
- Platform adapters:
  - raylib event polling, frame presentation, audio side effects.

Rule: application behavior should remain defined by Tempo processes; adapters must stay translation layers.

## Launch architecture

- Unified router: `dune exec ./applications/run -- <app>`
- App-local launchers:
  - `dune exec ./applications/advanced/<app>/run`
  - `dune exec ./applications/simple-demos/<app>/run`

## Quality gates

Release validation uses:

- `/Users/fredericdabrowski/Repos/tempo/tools/release/validate_apps.sh`
- `/Users/fredericdabrowski/Repos/tempo/applications/RELEASE_CHECKLIST.md`
