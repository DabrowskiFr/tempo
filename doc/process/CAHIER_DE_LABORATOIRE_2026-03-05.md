# Cahier de laboratoire - 2026-03-05

## Theme: CI GitHub failing on release/0.2.0

### Objective
Restore green CI on GitHub for branch `release/0.2.0` and associated PR.

### Observations
- Failing run identified: `22708160769` (workflow `CI`).
- Failure occurs on both `ubuntu-latest` and `macos-latest`.
- Failing step: `opam install . --deps-only --with-doc --with-test`.
- Reported root cause: missing package `threads` via `deps-of-tempo-jobs`.

### Attempts and outcomes
- Attempt 1: inspect workflow-level error only.
Result: insufficient detail.
- Attempt 2: inspect failed logs with `gh run view --log-failed`.
Result: success, exact dependency mismatch identified.
- Attempt 3: patch dependency from `threads` to `base-threads` in package metadata.
Result: local checks pass (`opam lint tempo-jobs.opam`, `dune build @all`).
- Attempt 4: rerun CI after patch 3.
Result: new conflict surfaced (`deps-of-tempo-jobs -> deps-of-tempo >= 0.2.0`), revealing unpushed opam version alignment.
- Attempt 5: push opam version alignment to `0.2.0`.
Result: conflict on `tempo-jobs` resolved, but a new dependency issue surfaced (`tempo-fluidsynth -> unix` unknown package).
- Attempt 6: patch dependency from `unix` to `base-unix` in `dune-project` and `tempo-fluidsynth.opam`.
Result: local metadata corrected; CI rerun required.

### Root cause analysis
`threads` is not an opam package dependency name for CI solver use in this context.
The expected dependency is `base-threads`.
Additionally, `unix` must be declared as `base-unix` in opam package dependencies.

### Applied fix
- Updated package declaration in `dune-project` for `tempo-jobs`.
- Updated generated opam file `tempo-jobs.opam` accordingly.

### Validation
- `opam lint tempo-jobs.opam`: passed.
- `dune build @all`: passed.

### Risks / follow-up
- CI must be re-run remotely to confirm both matrix jobs now resolve dependencies.
- If more stale opam deps exist, re-run `opam install . --deps-only --with-doc --with-test` in clean switch.
