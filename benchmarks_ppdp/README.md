# PPDP Benchmark Pack (Tempo vs ReactiveML)

This directory is a self-contained benchmark bundle prepared for paper artifacts.

It contains:
- benchmark programs for Tempo and ReactiveML
- a frozen reference pair of raw CSV logs (`data/raw/*reference-good.csv`)
- deterministic summarization/plot scripts pinned to that reference pair
- native-binary benchmark runners for both implementations
- a policy file (`policy.env`) that locks canonical baselines and safe size defaults

## Layout

- `programs/tempo/`: Tempo benchmark executable source (`tempo_bench.ml` + dune files)
- `programs/reactiveml/`: ReactiveML benchmark source (`rml_bench.rml` + Makefile)
- `scripts/`: deterministic summarize + plot scripts
- `scripts/run_tempo.sh`, `scripts/run_rml.sh`, `scripts/run_rml_ocaml5.sh`, `scripts/run_all.sh`: run fresh campaigns
- `scripts/run_tempo_diag.sh`, `scripts/analyze_tempo_diag.py`: targeted Tempo memory diagnostics (per-snapshot counters + RSS)
- `scripts/compare_rml_toolchains.py`: compare legacy ReactiveML vs OCaml-5 ReactiveML campaigns
- `scripts/run_locked_comparison.sh`: run current vs locked baseline with guardrails
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
./scripts/run_rml_ocaml5.sh   # optional second RML run on OCaml 5 toolchain
```

Notes:
- Tempo is built with `dune build` and executed via `./_build/default/.../tempo_bench.exe`.
- ReactiveML is compiled with `ocamlopt` (native) and executed via `./rml_bench`.
- The optional OCaml-5 ReactiveML run writes its own CSV (`rml_ocaml5-*.csv`) and does not interfere with `rml-*.csv`.
- This avoids mixing native execution on one side with bytecode execution on the other.
- `peak_mb` is always overwritten from OS-level peak RSS (`/usr/bin/time`) for every run.
- Campaign scripts now fail fast if peak RSS cannot be extracted (no fallback to runtime-internal counters).
- Each run now writes a metadata sidecar (`*.meta`) with commit/switch/grid details.
- By default, size values above `MAX_REASONABLE_SIZE` (policy default: 5000) are refused.

To enable the OCaml-5 ReactiveML target in `run_all.sh`, set in `config.env`:
- `RML_OCAML5_ENABLED=1`
- `RML_OCAML5_SWITCH=$TEMPO_SWITCH` (or another OCaml-5 switch)
- `RML_OCAML5_RMLC=/abs/path/to/patched/rmlc` when `rmlc` is not installed in that switch
- `RML_OCAML5_RMLLIB=/abs/path/to/rml/lib/lco` only if auto-detection fails

To compare both ReactiveML toolchains after runs:
```sh
python3 ./scripts/compare_rml_toolchains.py \
  --rml-legacy ./data/raw/rml-<timestamp>.csv \
  --rml-ocaml5 ./data/raw/rml_ocaml5-<timestamp>.csv
```

## Targeted Tempo Memory Diagnostics

To inspect runtime pressure per instant and correlate with RSS:

```sh
./scripts/run_tempo_diag.sh
python3 ./scripts/analyze_tempo_diag.py \
  --summary ./data/processed/tempo-diag-<timestamp>/summary.csv \
  --out-dir ./data/processed/tempo-diag-analysis-<timestamp>
```

The diagnostic traces contain:
- per-snapshot scheduler/signal/task counters
- `live_tasks` and `kill_context_*` pressure indicators
- sampled process RSS (`rss_mb`) per snapshot phase

## Locked Current-vs-Baseline Comparison (Recommended)

Use this instead of ad-hoc commit choices:

```sh
cp config.env.example config.env
./scripts/run_locked_comparison.sh
```

This workflow enforces:
- baseline selected by policy id (default: `v0.2.0`)
- baseline commit pinned in `policy.env`
- same benchmark harness copied into the baseline worktree
- comparison outputs with explicit baseline id/commit in CSV rows

To run archaeology against the very old baseline, you must explicitly opt in by
editing `policy.env` (`ALLOW_LEGACY_BASELINE=1`).

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
