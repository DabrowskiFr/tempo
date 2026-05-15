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
