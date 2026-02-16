# Compatibility Policy

This policy defines compatibility expectations for Tempo runtime and application surfaces.

## 1. Stable surface (default expectation)

- Core synchronous API in `/Users/fredericdabrowski/Repos/tempo/lib/tempo.mli`:
  - `execute`, signals, `await`, `await_immediate`, `pause`, `when_`, `watch`, `parallel`.
- Deterministic observable semantics validated by `dune runtest`.
- Unified app launcher contract:
  - `dune exec ./applications/run -- <app>`

Changes to this surface require:

1. release note entry,
2. migration note,
3. test and benchmark validation.

## 2. Evolving surface (may change across minor releases)

- Layer2 convenience modules and helper ergonomics.
- App-internal structures and visual assets.
- Tooling scripts under `tools/` (unless explicitly marked stable).

Breaking changes are allowed only with migration instructions.

## 3. Deprecation policy

- Keep compatibility aliases for at least one release cycle when feasible.
- Example:
  - `reactive-reconfiguration-engine` launcher alias kept for `refactor`.

## 4. Path/layout policy

Current canonical app layout:

- `/Users/fredericdabrowski/Repos/tempo/applications/advanced`
- `/Users/fredericdabrowski/Repos/tempo/applications/simple-demos`

Legacy paths are non-canonical and may disappear after migration period.

## 5. Release gate for compatibility

Before release:

1. `dune build`
2. `dune runtest`
3. `./tools/release/validate_apps.sh`
4. update `RELEASE_NOTES.md` migration section
