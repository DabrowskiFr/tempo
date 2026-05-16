#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

OUT_FILE="$RAW_DIR/tempo-$(timestamp).csv"
echo "impl,benchmark,size,run,time_ms,instants,peak_mb" > "$OUT_FILE"

echo "Running Tempo benchmarks (native binary) -> $OUT_FILE"
echo "[tempo] repo=$TEMPO_ROOT commit=$(git_head_or_unknown "$TEMPO_ROOT")"

if ! opam exec --switch="$TEMPO_SWITCH" -- ocamlfind query tempo >/dev/null 2>&1; then
  if [[ -n "${TEMPO_REPO:-}" && -d "${TEMPO_REPO:-}" ]]; then
    echo "[tempo] library 'tempo' not found in switch=$TEMPO_SWITCH, pinning from $TEMPO_REPO"
    opam pin add --switch="$TEMPO_SWITCH" --yes tempo "$TEMPO_REPO" >> "$LOG_DIR/tempo.log" 2>&1
  else
    echo "[tempo] library 'tempo' not found in switch=$TEMPO_SWITCH and TEMPO_REPO is unavailable." >&2
    echo "[tempo] Set TEMPO_REPO in benchmarks_ppdp/config.env or install package 'tempo' in that switch." >&2
    exit 1
  fi
fi

echo "[tempo] building native benchmark executable with switch=$TEMPO_SWITCH"
opam exec --switch="$TEMPO_SWITCH" -- \
  dune build ./benchmarks_ppdp/programs/tempo/tempo_bench.exe \
  --root "$TEMPO_ROOT" >> "$LOG_DIR/tempo.log" 2>&1

TEMPO_BIN="$TEMPO_ROOT/_build/default/benchmarks_ppdp/programs/tempo/tempo_bench.exe"
if [[ ! -x "$TEMPO_BIN" ]]; then
  echo "[tempo] expected executable not found: $TEMPO_BIN" >&2
  exit 1
fi

for bench in $BENCHMARKS; do
  bench_sizes="$(sizes_for_benchmark "$bench")"
  for size in $bench_sizes; do
    for run in $(seq 1 "$RUNS"); do
      cmd=(
        opam exec --switch="$TEMPO_SWITCH" -- "$TEMPO_BIN"
        --benchmark "$bench" --size "$size" --run "$run"
      )
      echo "[tempo] bench=$bench size=$size run=$run"
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
        echo "[tempo] failed to parse benchmark CSV output (bench=$bench size=$size run=$run)" >&2
        rm -f "$tmp_stdout" "$tmp_stderr"
        exit 1
      fi
      if peak_mb="$(extract_peak_rss_mb "$tmp_stderr")"; then
        row="$(set_csv_peak_mb "$row" "$peak_mb")"
      else
        echo "[tempo] warning: could not extract peak RSS, keeping executable-reported peak_mb" >> "$LOG_DIR/tempo.log"
      fi
      echo "$row" >> "$OUT_FILE"
      rm -f "$tmp_stdout" "$tmp_stderr"
    done
  done
done

write_run_metadata "tempo" "$OUT_FILE" "$TEMPO_ROOT" "$TEMPO_SWITCH"
echo "Done: $OUT_FILE"
