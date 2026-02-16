# refactor

## Overview

Application avancée de reconfiguration réactive (style Concerto-D simplifié) basée sur Tempo.  
Le moteur charge un système et des transactions, puis exécute orchestration, vérification, commit/rollback et génération de traces.

## Controls / Inputs

- Mode interactif (fenêtre Raylib):
- `T`: enqueue transaction suivante
- `R`: recharger les specs
- `P`: toggle partition
- `L` / `K`: loss +/-
- `Y`: cycle policy
- `U`: cycle scenario
- `Esc`: quitter

## Launch Commands

Depuis `/Users/fredericdabrowski/Repos/tempo`:

```bash
dune exec ./applications/run -- refactor
```

Compat:

```bash
dune exec ./applications/reconfiguration/run -- refactor
```

Lancement direct:

```bash
dune exec ./applications/advanced/refactor/run
```

## CLI Options

- `--headless`
- `--steps <n>`
- `--seed <n>`
- `--policy conservative|balanced|aggressive`
- `--scenario baseline|lossy|partitioned`
- `--system-file <path>`
- `--reconfig-file <path>`
- `--trace-dir <path>`

Exemple:

```bash
dune exec ./applications/run -- refactor -- --headless --steps 2400 --seed 7 --policy balanced --scenario baseline
```

## Dependencies

- OCaml 5.x, dune
- Raylib (mode interactif)

## Headless / Reproducible Mode

- Mode headless supporté via `--headless`.
- Reproductibilité via `--seed`.
- Traces CSV/JSON dans `applications/advanced/refactor/traces/` (ou `--trace-dir`).

## Troubleshooting

- Vérifier la validité des fichiers `spec/system.concerto` et `spec/reconfig.concerto`.
- Si lancement interactif échoue: tester d’abord en `--headless`.

## Release Status

- Category: `advanced`
- Quality status: `ready`
- Last validation date: 2026-02-16
- Validated commands:
- `dune exec ./applications/run -- refactor`
- `dune exec ./applications/run -- refactor -- --headless --steps 120 --seed 1`
