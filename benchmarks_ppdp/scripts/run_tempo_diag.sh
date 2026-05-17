#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

DIAG_BENCHMARKS="${DIAG_BENCHMARKS:-fork_explosion guarded_cascades nested_preemption}"
DIAG_RUNS="${DIAG_RUNS:-1}"
DIAG_COMPACT="${DIAG_COMPACT:-0}"
OUT_DIR="${1:-$PROC_DIR/tempo-diag-$(timestamp)}"
TRACE_DIR="$OUT_DIR/traces"
SUMMARY_CSV="$OUT_DIR/summary.csv"

mkdir -p "$TRACE_DIR"
echo "benchmark,size,run,time_ms,instants,peak_mb,diag_csv" > "$SUMMARY_CSV"

echo "[tempo-diag] output directory: $OUT_DIR"
echo "[tempo-diag] building native benchmark executable with switch=$TEMPO_SWITCH"
opam exec --switch="$TEMPO_SWITCH" -- \
  dune build ./benchmarks_ppdp/programs/tempo/tempo_bench.exe \
  --root "$TEMPO_ROOT" >> "$LOG_DIR/tempo.log" 2>&1

TEMPO_BIN="$TEMPO_ROOT/_build/default/benchmarks_ppdp/programs/tempo/tempo_bench.exe"
if [[ ! -x "$TEMPO_BIN" ]]; then
  echo "[tempo-diag] expected executable not found: $TEMPO_BIN" >&2
  exit 1
fi

validate_positive_integer "DIAG_RUNS" "$DIAG_RUNS"

for bench in $DIAG_BENCHMARKS; do
  bench_sizes="$(sizes_for_benchmark "$bench")"
  for size in $bench_sizes; do
    for run in $(seq 1 "$DIAG_RUNS"); do
      diag_csv="$TRACE_DIR/tempo-${bench}-n${size}-run${run}.csv"
      cmd=(
        opam exec --switch="$TEMPO_SWITCH" -- "$TEMPO_BIN"
        --benchmark "$bench" --size "$size" --run "$run"
        --diag-csv "$diag_csv"
      )
      if [[ "$DIAG_COMPACT" == "1" ]]; then
        cmd+=(--diag-compact)
      fi

      echo "[tempo-diag] bench=$bench size=$size run=$run"
      tmp_stdout="$(mktemp)"
      tmp_stderr="$(mktemp)"
      (
        RML_LOG_LEVEL=off RML_TRACE_GUARDS=0 RML_LOG_COLOR=0 \
          "${TIME_CMD[@]}" "${cmd[@]}" >"$tmp_stdout" 2>"$tmp_stderr"
      )

      cat "$tmp_stdout" >> "$LOG_DIR/tempo.log"
      cat "$tmp_stderr" >> "$LOG_DIR/tempo.log"

      row="$(awk -F, '$1=="tempo" && NF>=7 {line=$0} END{print line}' "$tmp_stdout")"
      if [[ -z "$row" ]]; then
        echo "[tempo-diag] failed to parse benchmark CSV output (bench=$bench size=$size run=$run)" >&2
        rm -f "$tmp_stdout" "$tmp_stderr"
        exit 1
      fi

      peak_mb="$(require_peak_rss_mb "$tmp_stderr" "tempo-diag bench=$bench size=$size run=$run")"
      row="$(set_csv_peak_mb "$row" "$peak_mb")"
      awk -F, -v OFS=, -v d="$diag_csv" '{print $2,$3,$4,$5,$6,$7,d}' <<<"$row" >> "$SUMMARY_CSV"

      rm -f "$tmp_stdout" "$tmp_stderr"
    done
  done
done

echo "[tempo-diag] summary: $SUMMARY_CSV"
echo "[tempo-diag] traces:  $TRACE_DIR"
