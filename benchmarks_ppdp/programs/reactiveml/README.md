# ReactiveML Benchmarks

Build and run:

```sh
make clean all
./rml_bench --benchmark propagation_chains --size 5000 --run 1
```

`rml_bench` is compiled as a native executable (`ocamlopt`).

Output CSV schema:

`impl,benchmark,size,run,time_ms,instants,peak_mb`

`instants` follows the same normalized benchmark-level convention as the Tempo
harness:
- `propagation_chains`: `1`
- `broadcast_expansion`: `2`
- `fork_explosion`: `effective_fork_depth(n)`
- `guarded_cascades`: `1`
- `nested_preemption`: `2`
