# Runtime Performance Invariants

This document defines mandatory rules for modifying hot paths in `/Users/fredericdabrowski/Repos/tempo/lib/tempo.ml`.

## Scope

Hot paths include, at minimum:

- `Scheduler_step.step`
- `Effects_handler.run`
- `Guard_waiters.block_on_guards`
- `enqueue_now` / `enqueue_next`
- signal waiter push/pop paths
- task creation in continuation-heavy branches

## Invariants

1. No unguarded debug work in hot paths.
2. No metrics collection unless explicitly enabled.
3. No new allocations in tight loops unless measured and justified.
4. Keep queue transition ownership clear (`Scheduler_step`, `Instant_rollover`, `Guard_waiters`).
5. Any change in fast-path branching must have a readable fallback path.

## Required validation for hot-path changes

1. `dune build`
2. `dune runtest`
3. benchmark run with baseline comparison:
   - `./tools/bench/run_core_bench.sh --out-dir <dir> --baseline-csv bench/results/baseline-20260216-163831/core/core_bench.csv`
4. record:
   - average deltas per benchmark
   - worst-point delta
   - maintainability impact (code complexity, branching, ownership clarity)

## Decision policy

- Keep a performance optimization only if the ratio `performance gain / maintainability cost` is favorable.
- If gains are in measurement noise and code complexity increases, prefer simplification.
