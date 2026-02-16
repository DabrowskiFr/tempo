# Applications Release Checklist

This checklist is the release gate for all applications under `/Users/fredericdabrowski/Repos/tempo/applications`.

## A. Packaging and Routing

- [ ] Architecture and category rules respected:
  - `/Users/fredericdabrowski/Repos/tempo/applications/ARCHITECTURE.md`
- [ ] App is placed in the correct category:
  - `advanced`: production-grade or research-grade applications (`game-univ`, `refactor`).
  - `simple-demos`: focused demos and experiments.
- [ ] App is launchable from unified router:
  - `dune exec ./applications/run -- <app>`
- [ ] App-local launcher works:
  - `dune exec ./applications/<category>/<app>/run`

## B. Run Script Contract

- [ ] `run` script supports `help`, `--help`, `-h`.
- [ ] `run` script supports extra CLI args passthrough.
- [ ] `run` script works both from repository and from dune context (`DUNE_SOURCEROOT`).
- [ ] Exit code is non-zero on invalid usage.

## C. README Quality (homogeneous format)

- [ ] README includes these sections:
  - Overview
  - Controls / Inputs
  - Launch Commands
  - CLI options (if any)
  - Dependencies
  - Headless / reproducible mode (if available)
  - Troubleshooting
  - Release status
- [ ] Paths use the current tree (`advanced`, `simple-demos`).
- [ ] Commands are copy/paste-ready from repo root.

## D. Runtime Quality

- [ ] App starts and closes cleanly.
- [ ] No obvious runtime errors in normal path.
- [ ] If graphical: window close button works.
- [ ] If audio: audio start/pause/stop behavior is coherent.

## E. Determinism / Automation

- [ ] If applicable, app supports deterministic seed (`--seed`) and/or headless mode.
- [ ] App has at least one smoke command suitable for CI/local validation.

## F. Repository Validation

- [ ] Smoke matrix reviewed:
  - `/Users/fredericdabrowski/Repos/tempo/applications/SMOKE_MATRIX.md`
- [ ] Documentation freshness gate:
  - `./tools/release/check_doc_freshness.sh`
- [ ] Run the release gate script:
  - `./tools/release/validate_apps.sh`
- [ ] `dune build`
- [ ] `dune runtest`
- [ ] App smoke run (`dune exec ./applications/run -- <app>`)

## G. Release Notes

- [ ] App entry updated in release notes with:
  - category
  - launch command
  - known limitations
