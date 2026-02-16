# Tempo Runtime Internals (for contributors)

This note explains where to make changes in the runtime and which invariants must stay true.

## Source of truth

- Runtime semantics are implemented in:
  - `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`
- Hot-path/performance invariants are documented in:
  - `/Users/fredericdabrowski/Repos/tempo/docs/runtime_perf_invariants.md`

Legacy modules (`tempo_engine`, `tempo_signal`, `tempo_task`) are kept as references only and are not part of the `tempo` build.

## Internal module map in `tempo.ml`

- `Thread_store`:
  - manages dense thread-state storage (`thread_dense`),
  - allocates logical thread ids.
- `Task_fastpath`:
  - centralizes cheap checks used on hot paths (`task_kills_alive`, `task_guard_ok`).
- `Task_builder`:
  - centralizes task construction (`mk_task`, `mk_task_with_state`),
  - owns guard fast-path derivation (`guard_single`) and thread-state registration.
- `Join_many`:
  - handles join orchestration with specialized paths (`0/1/2`) and general fallback.
- `Guard_waiters`:
  - registers tasks on missing guards,
  - centralizes wakeup and re-blocking transitions when guards become present.
- `Instant_rollover`:
  - handles end-of-instant queue transition logic,
  - centralizes blocked-task carryover and killed-task survivor filtering.
- `Effects_handler`:
  - interprets runtime effects (`Emit`, `Await`, `Fork`, `Join`, ...),
  - schedules continuations and guarded tasks.
- `Scheduler_step`:
  - controls per-step execution and per-instant rollover.
- `Runtime_bootstrap`:
  - creates scheduler state consistently,
  - centralizes host I/O hook wiring for `execute` and `execute_inspect`.

## Key invariants

1. A task finishing must call `finish_task_state` exactly once.
2. `Join_many` must reject joining the current thread.
3. Signal waiters must preserve existing observable ordering (LIFO in current runtime).
4. Hot-path logging/metrics must remain fully gated when disabled.
5. Deterministic semantics depend on signals and explicit communication only (no shared mutable cross-process protocol).

## Invariants mapped to tests

- Instant-end ordering and snapshot contract:
  - `/Users/fredericdabrowski/Repos/tempo/tests/ok/runtime_instant_observable_order.ml`
- `current` queue drained before `on_instant` callback:
  - `/Users/fredericdabrowski/Repos/tempo/tests/ok/runtime_instant_current_empty.ml`
- Commutative deterministic observable behavior for core synchronization:
  - `/Users/fredericdabrowski/Repos/tempo/tests/ok/await_immediate_parallel_order.ml`
  - `/Users/fredericdabrowski/Repos/tempo/tests/ok/parallel_fanout_barrier.ml`

When changing scheduler internals, update or extend these tests first, then modify runtime code.

## Runtime anti-patterns

- Updating legacy runtime modules (`tempo_engine`, `tempo_signal`, `tempo_task`) instead of `tempo.ml`.
- Introducing side effects in deterministic tests that depend on task scheduling order.
- Adding debug/metrics work on hot paths without guard checks.
- Coupling runtime semantics to mutable shared references across processes.
- Hiding lifecycle transitions (`blocked`, `next_instant`, task completion) across unrelated helpers.
- Growing hot-path logic without benchmark evidence and guard checks.

Preferred pattern:

- Keep one owner for each queue transition (`Scheduler_step`, `Instant_rollover`, `Guard_waiters`).
- Encode communication via signals/events and assert observable outcomes in tests.

## Change workflow

For any runtime modification:

1. `dune build`
2. `dune runtest`
3. run core bench with baseline comparison
4. record:
   - cumulative gain vs baseline
   - incremental gain vs previous phase

## Review checklist (runtime PR)

- Semantics unchanged for deterministic tests.
- No hidden logging/metrics costs added in hot path.
- New fast-path has a readable fallback path.
- Benchmark deltas documented with command outputs.
