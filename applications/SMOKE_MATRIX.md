# Applications Smoke Matrix

This matrix defines release smoke checks for all app slugs exposed by:

`dune exec ./applications/run -- help`

## Automated by `validate_apps.sh`

| App | Build target | `run --help` | Headless smoke | Status |
|---|---|---|---|---|
| `game-univ` | yes | yes | no | automated (partial) |
| `refactor` | yes | yes | yes (`--headless --steps 120 --seed 1`) | automated |
| `tempo-core-studio` | yes | yes | yes (`--headless --instants 12`) | automated |
| `snake-raylib` | yes | yes | no | automated (partial) |
| `boids-raylib` | yes | yes | no | automated (partial) |
| `ca-continuous-raylib` | yes | yes | no | automated (partial) |
| `lenia-raylib` | yes | yes | no | automated (partial) |
| `solar-system-raylib` | yes | yes | no | automated (partial) |
| `logicgroove` | yes | yes | no | automated (partial) |
| `temporalsim` | yes | yes | no | automated (partial) |

## Manual release checks

Run interactively before release:

1. `dune exec ./applications/run -- game-univ`
2. `dune exec ./applications/run -- refactor`
3. `dune exec ./applications/run -- tempo-core-studio`
4. at least one demo per visual family:
   - `dune exec ./applications/run -- boids-raylib`
   - `dune exec ./applications/run -- lenia-raylib`
   - `dune exec ./applications/run -- logicgroove`

Manual acceptance criteria:

- app window opens and closes cleanly;
- controls react as documented;
- no immediate runtime exception in nominal path;
- audio/render side effects are coherent for advanced apps.
