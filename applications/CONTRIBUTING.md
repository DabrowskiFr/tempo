# Applications Contributing Guide

This guide defines contribution rules for `/Users/fredericdabrowski/Repos/tempo/applications`.

## Directory policy

- Advanced apps: `/Users/fredericdabrowski/Repos/tempo/applications/advanced`
- Simple demos: `/Users/fredericdabrowski/Repos/tempo/applications/simple-demos`

Place new apps in the category that matches their quality target.

## Required app contract

Each app must provide:

1. `run` launcher script with:
   - `help` / `--help` / `-h`
   - argument passthrough
   - compatibility with `DUNE_SOURCEROOT`
2. `README.md` following:
   - `/Users/fredericdabrowski/Repos/tempo/applications/README_TEMPLATE.md`
3. launch compatibility with unified router:
   - `dune exec ./applications/run -- <app>`

## Quality gate before merge

Run:

```bash
./tools/release/validate_apps.sh
```

The script checks launchers, README structure, app builds, and a headless smoke path.

## Documentation expectations

- Commands must be runnable from repository root.
- Paths must use `advanced`/`simple-demos` tree.
- Mention headless/seed behavior when available.

## Determinism and testing

- Prefer deterministic app behavior for non-interactive paths.
- If app supports simulation mode, expose `--seed` and document it.
