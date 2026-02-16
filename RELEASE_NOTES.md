# Release Notes (Draft)

## Migration

### Applications tree reorganization

Applications are now split into:

- `applications/advanced`
- `applications/simple-demos`

Moved advanced apps:

- `applications/advanced/game-univ`
- `applications/advanced/refactor`

Simple demos are under `applications/simple-demos/*`.

### Launch commands

Preferred command:

```bash
dune exec ./applications/run -- <app>
```

Examples:

```bash
dune exec ./applications/run -- game-univ
dune exec ./applications/run -- refactor
dune exec ./applications/run -- lenia-raylib
```

Compatibility alias kept:

- `reactive-reconfiguration-engine` maps to `refactor` in launcher scripts.

### Release validation

Run the release gate before tagging:

```bash
./tools/release/validate_apps.sh
```
