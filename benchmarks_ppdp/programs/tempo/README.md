# Tempo Benchmarks

Build and run:

```sh
dune build ./tempo_bench.exe
dune exec ./tempo_bench.exe -- --benchmark propagation_chains --size 5000 --run 1
```

Output CSV schema:

`impl,benchmark,size,run,time_ms,instants,peak_mb`
