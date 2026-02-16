# Tempo Runtime Source of Truth (Phase 1)

## Decision

The runtime source of truth is:

- `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`
- `/Users/fredericdabrowski/Repos/tempo/docs/runtime_perf_invariants.md` (hot-path rules)

Public runtime behavior, scheduler semantics, signal handling, and performance work must be implemented there.

## Legacy modules

The following modules remain in the repository for reference only:

- `/Users/fredericdabrowski/Repos/tempo/lib/tempo_engine.ml`
- `/Users/fredericdabrowski/Repos/tempo/lib/tempo_signal.ml`
- `/Users/fredericdabrowski/Repos/tempo/lib/tempo_task.ml`

They are no longer part of the `tempo` library build.

## Contributor rule

For runtime changes:

1. Change `tempo.ml` (and `tempo.mli` when API changes).
2. Do not add new behavior in legacy modules.
3. Validate with:
   - `dune build`
   - `dune runtest`
   - core benchmark run + comparison report.
4. Confirm observable runtime invariants via:
   - `/Users/fredericdabrowski/Repos/tempo/tests/ok/runtime_instant_observable_order.ml`
   - `/Users/fredericdabrowski/Repos/tempo/tests/ok/runtime_instant_current_empty.ml`

## Internal structure (phase 2)

Inside `tempo.ml`, runtime internals are now organized around dedicated modules:

- `Thread_store`: dense thread-state storage and logical thread id allocation.
- `Task_fastpath`: hot-path checks for kills/guards.
- `Task_builder`: task construction and guard-single derivation.
- `Join_many`: specialized join orchestration for small and general fanouts.
- `Guard_waiters`: guard-missing registration and wakeup/blocking transitions.
- `Instant_rollover`: end-of-instant transition helpers (blocked wake-for-next + survivor drain).
- `Effects_handler`: effect interpretation and continuation scheduling.
- `Scheduler_step`: instant/step orchestration and queue progression.
- `Runtime_bootstrap`: scheduler-state initialization and shared input/output hook wiring.

## Rationale

This avoids duplicated scheduler logic, reduces maintenance overhead, and makes performance/safety reviews easier for new contributors.

## Anti-drift checklist

Before merging runtime refactors:

1. No semantic behavior added in legacy modules.
2. Deterministic tests assert values/signals, not print order.
3. New helper/module has an explicit ownership boundary.
4. Runtime docs list the new helper in the internal module map.
5. Hot-path changes respect `/Users/fredericdabrowski/Repos/tempo/docs/runtime_perf_invariants.md`.
