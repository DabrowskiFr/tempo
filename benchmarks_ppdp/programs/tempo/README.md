# Tempo Benchmarks

Build and run:

```sh
cd /path/to/tempo-dev
opam exec --switch=5.4.1+options -- dune build ./benchmarks_ppdp/programs/tempo/tempo_bench.exe
opam exec --switch=5.4.1+options -- \
  ./_build/default/benchmarks_ppdp/programs/tempo/tempo_bench.exe \
  --benchmark propagation_chains --size 5000 --run 1
```

The benchmark is executed via the built native binary (not `dune exec`).

Output CSV schema:

`impl,benchmark,size,run,time_ms,instants,peak_mb`

`instants` is normalized with the same benchmark-level convention as the
ReactiveML harness:
- `propagation_chains`: `1`
- `propagation_chains_multi`: `effective_multi_rounds(n)` (`max(2, floor(log2 n))`)
- `broadcast_expansion`: `2`
- `fork_explosion`: `effective_fork_depth(n)`
- `guarded_cascades`: `1`
- `guarded_cascades_multi`: `effective_multi_rounds(n)` (`max(2, floor(log2 n))`)
- `nested_preemption`: `2`

## Diagnostic mode

To record per-instant runtime counters (queues, signals, thread slots) and GC
stats, use `--diag-csv`:

```sh
opam exec --switch=5.4.1+options -- \
  ./_build/default/benchmarks_ppdp/programs/tempo/tempo_bench.exe \
  --benchmark guarded_cascades --size 1000 --run 1 \
  --diag-csv /tmp/tempo_diag_b4_n1000.csv
```

Diagnostic rows include:
- scheduler state (`current_q`, `blocked_q`, `next_q`)
- signal pressure (`tracked_signals`, `awaiters`, `guard_waiters`, `kill_watchers`)
- task/kill-context pressure (`live_tasks`, `kill_context_refs`, `kill_context_nodes`, `kill_context_max_depth`)
- GC counters (`gc_*`)
- OS RSS sample per snapshot (`rss_mb`) with elapsed time (`timestamp_ms`)

The optional `--diag-compact` flag runs `Gc.compact ()` before the final
benchmark row, which is useful to compare reserved heap vs compacted live
footprint at the end of a run.

The diagnostic CSV also includes cumulative churn counters (tasks/signals
created/disposed, awaiters and kill-watchers registrations/fires/prunes),
which are useful to explain memory scaling.

## Memtrace mode

For allocation-site profiling (when `memtrace` is available in the switch):

```sh
opam exec --switch=5.4.1+options -- \
  ./_build/default/benchmarks_ppdp/programs/tempo/tempo_bench.exe \
  --benchmark nested_preemption --size 5000 --run 1 \
  --memtrace-file /tmp/tempo-b5-n5000.ctf --memtrace-rate 2e-5
opam exec --switch=5.4.1+options -- memtrace_hotspots /tmp/tempo-b5-n5000.ctf
```
