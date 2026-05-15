#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

OUT_FILE="$RAW_DIR/rml-$(timestamp).csv"
echo "impl,benchmark,size,run,time_ms,instants,peak_mb" > "$OUT_FILE"

echo "Running ReactiveML benchmarks (native binary) -> $OUT_FILE"

echo "[rml] building native benchmark executable with switch=$RML_SWITCH"
(
  cd "$RML_BENCH_DIR"
  opam exec --switch="$RML_SWITCH" -- make clean all
) >> "$LOG_DIR/rml.log" 2>&1

RML_BIN="$RML_BENCH_DIR/rml_bench"
if [[ ! -x "$RML_BIN" ]]; then
  echo "[rml] expected executable not found: $RML_BIN" >&2
  exit 1
fi

for bench in $BENCHMARKS; do
  bench_sizes="$(sizes_for_benchmark "$bench")"
  for size in $bench_sizes; do
    for run in $(seq 1 "$RUNS"); do
      cmd=(
        opam exec --switch="$RML_SWITCH" -- "$RML_BIN"
        --benchmark "$bench" --size "$size" --run "$run"
      )
      echo "[rml] bench=$bench size=$size run=$run"
      tmp_stdout="$(mktemp)"
      tmp_stderr="$(mktemp)"
      (
        "${TIME_CMD[@]}" "${cmd[@]}" >"$tmp_stdout" 2>"$tmp_stderr"
      )
      cat "$tmp_stdout" >> "$LOG_DIR/rml.log"
      cat "$tmp_stderr" >> "$LOG_DIR/rml.log"

      row="$(awk -F, '$1=="rml" && NF>=7 {line=$0} END{print line}' "$tmp_stdout")"
      if [[ -z "$row" ]]; then
        echo "[rml] failed to parse benchmark CSV output (bench=$bench size=$size run=$run)" >&2
        rm -f "$tmp_stdout" "$tmp_stderr"
        exit 1
      fi
      if peak_mb="$(extract_peak_rss_mb "$tmp_stderr")"; then
        row="$(set_csv_peak_mb "$row" "$peak_mb")"
      else
        echo "[rml] warning: could not extract peak RSS, keeping executable-reported peak_mb" >> "$LOG_DIR/rml.log"
      fi
      echo "$row" >> "$OUT_FILE"
      rm -f "$tmp_stdout" "$tmp_stderr"
    done
  done
done

echo "Done: $OUT_FILE"
