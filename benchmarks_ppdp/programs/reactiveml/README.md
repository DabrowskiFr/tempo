# ReactiveML Benchmarks

Build and run:

```sh
make clean all
./rml_bench --benchmark propagation_chains --size 5000 --run 1
```

`rml_bench` is compiled as a native executable (`ocamlopt`).

Output CSV schema:

`impl,benchmark,size,run,time_ms,instants,peak_mb`
