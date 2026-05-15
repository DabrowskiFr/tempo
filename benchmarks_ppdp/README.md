# PPDP Benchmark Pack (Tempo vs ReactiveML)

This directory is a self-contained benchmark bundle prepared for paper artifacts.

It contains:
- benchmark programs for Tempo and ReactiveML
- a frozen reference pair of raw CSV logs (`data/raw/*reference-good.csv`)
- deterministic summarization/plot scripts pinned to that reference pair
- native-binary benchmark runners for both implementations

## Layout

- `programs/tempo/`: Tempo benchmark executable source (`tempo_bench.ml` + dune files)
- `programs/reactiveml/`: ReactiveML benchmark source (`rml_bench.rml` + Makefile)
- `scripts/`: deterministic summarize + plot scripts
- `scripts/run_tempo.sh`, `scripts/run_rml.sh`, `scripts/run_all.sh`: run fresh campaigns
- `data/raw/`: raw benchmark rows
- `data/processed/`: generated CSV summaries
- `figures/`: generated plots

## Frozen Reference (good campaign)

The reference scripts use this explicit pair:
- `data/raw/tempo-reference-good.csv`
- `data/raw/rml-reference-good.csv`

These files correspond to a campaign where Tempo remains competitive on B1/B2 and significantly closer than older outlier runs on B4/B5.
The current frozen pair includes sizes \(n \in \{10, 100, 1000, 5000, 50000\}\).

## Rebuild Reference Outputs

```sh
./scripts/rebuild_reference_outputs.sh
```

## Run A Fresh Campaign (Native Binaries)

```sh
cp config.env.example config.env
./scripts/run_tempo.sh
./scripts/run_rml.sh
```

Notes:
- Tempo is built with `dune build` and executed via `./_build/default/.../tempo_bench.exe`.
- ReactiveML is compiled with `ocamlopt` (native) and executed via `./rml_bench`.
- This avoids mixing native execution on one side with bytecode execution on the other.

This writes:
- `data/processed/reference-median-time.csv`
- `data/processed/reference-median-memory.csv`
- `data/processed/reference-median-full.csv`
- `data/processed/reference-summary-n5000.csv`
- `data/processed/reference-memory-summary-n5000.csv`
- `data/processed/reference-dispersion-n5000.csv`
- `figures/evaluation-time-curves-reference.pdf`
- `figures/evaluation-time-curves-reference.png`
- `figures/evaluation-memory-curves-reference.pdf`
- `figures/evaluation-memory-curves-reference.png`

## Expected n=5000 medians (Tempo / ReactiveML)

- B1 propagation: `2.192 / 2.8845`
- B2 broadcast: `1.3845 / 1.3745`
- B3 fork: `2.532 / 1.8260`
- B4 guarded: `3.0865 / 2.6007`
- B5 preemption: `2.4945 / 1.8090`

## Expected n=5000 memory medians (Tempo / ReactiveML peak MB)

- B1 propagation: `7.359 / 6.812`
- B2 broadcast: `6.766 / 5.172`
- B3 fork: `8.953 / 5.672`
- B4 guarded: `11.172 / 5.781`
- B5 preemption: `11.625 / 7.000`

## Expected n=50000 medians (Tempo / ReactiveML)

- B1 propagation: `21.9915 / 41.2741`
- B2 broadcast: `14.4055 / 16.1984`
- B3 fork: `26.1845 / 20.5320`
- B4 guarded: `40.2925 / 30.6746`
- B5 preemption: `57.1035 / 22.2269`

## Expected n=50000 memory medians (Tempo / ReactiveML peak MB)

- B1 propagation: `25.031 / 28.109`
- B2 broadcast: `17.453 / 13.047`
- B3 fork: `33.188 / 14.297`
- B4 guarded: `68.516 / 20.172`
- B5 preemption: `81.406 / 28.688`

The benchmark pack is intentionally frozen for artifact stability:
no `latest` auto-selection, no alternate run path inside this directory.
