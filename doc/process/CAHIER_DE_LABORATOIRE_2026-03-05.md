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

### Root cause analysis
`threads` is not an opam package dependency name for CI solver use in this context.
The expected dependency is `base-threads`.

### Applied fix
- Updated package declaration in `dune-project` for `tempo-jobs`.
- Updated generated opam file `tempo-jobs.opam` accordingly.

### Validation
- `opam lint tempo-jobs.opam`: passed.
- `dune build @all`: passed.

### Risks / follow-up
- CI must be re-run remotely to confirm both matrix jobs now resolve dependencies.
- If more stale opam deps exist, re-run `opam install . --deps-only --with-doc --with-test` in clean switch.

