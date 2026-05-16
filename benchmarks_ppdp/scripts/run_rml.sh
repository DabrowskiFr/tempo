#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

RML_RUN_NAME="${RML_RUN_NAME:-rml}"
RML_IMPL_ID="${RML_IMPL_ID:-rml}"
RML_RUN_SWITCH="${RML_RUN_SWITCH:-$RML_SWITCH}"
RML_RUN_BENCH_DIR="${RML_RUN_BENCH_DIR:-$RML_BENCH_DIR}"
RML_RUN_RMLC="${RML_RUN_RMLC:-}"
RML_RUN_RMLLIB="${RML_RUN_RMLLIB:-}"
RML_RUN_EXTRA_INCLUDE="${RML_RUN_EXTRA_INCLUDE:-}"
RML_RUN_OCAMLOPT="${RML_RUN_OCAMLOPT:-ocamlopt}"
RML_LOG_TAG="${RML_LOG_TAG:-$RML_IMPL_ID}"

ensure_switch_exists "$RML_RUN_SWITCH"

if [[ -z "$RML_RUN_RMLC" ]]; then
  if ! opam exec --switch="$RML_RUN_SWITCH" -- sh -lc 'command -v rmlc >/dev/null 2>&1'; then
    cat >&2 <<EOF
[$RML_LOG_TAG] cannot find 'rmlc' in switch=$RML_RUN_SWITCH.
Provide RML_RUN_RMLC=/absolute/path/to/rmlc (or set up rmlc in that switch).
EOF
    exit 1
  fi
elif [[ "$RML_RUN_RMLC" == */* ]]; then
  if [[ ! -x "$RML_RUN_RMLC" ]]; then
    echo "[$RML_LOG_TAG] RML_RUN_RMLC is not executable: $RML_RUN_RMLC" >&2
    exit 1
  fi
else
  if ! opam exec --switch="$RML_RUN_SWITCH" -- sh -lc "command -v \"$RML_RUN_RMLC\" >/dev/null 2>&1"; then
    echo "[$RML_LOG_TAG] compiler '$RML_RUN_RMLC' not found in switch=$RML_RUN_SWITCH" >&2
    exit 1
  fi
fi

OUT_FILE="$RAW_DIR/${RML_RUN_NAME}-$(timestamp).csv"
LOG_FILE="$LOG_DIR/${RML_RUN_NAME}.log"
echo "impl,benchmark,size,run,time_ms,instants,peak_mb" > "$OUT_FILE"

echo "Running ReactiveML benchmarks ($RML_IMPL_ID, native binary) -> $OUT_FILE"
echo "[$RML_LOG_TAG] repo=$RML_RUN_BENCH_DIR commit=$(git_head_or_unknown "$RML_RUN_BENCH_DIR")"

make_args=(make clean all "OCAMLOPT=$RML_RUN_OCAMLOPT")
if [[ -n "$RML_RUN_RMLC" ]]; then
  make_args+=("RMLC=$RML_RUN_RMLC")
fi
if [[ -n "$RML_RUN_EXTRA_INCLUDE" ]]; then
  make_args+=("RML_EXTRA_INCLUDE=$RML_RUN_EXTRA_INCLUDE")
fi

echo "[$RML_LOG_TAG] building native benchmark executable with switch=$RML_RUN_SWITCH"
(
  cd "$RML_RUN_BENCH_DIR"
  if [[ -n "$RML_RUN_RMLLIB" ]]; then
    RMLLIB="$RML_RUN_RMLLIB" opam exec --switch="$RML_RUN_SWITCH" -- "${make_args[@]}"
  else
    opam exec --switch="$RML_RUN_SWITCH" -- "${make_args[@]}"
  fi
) >> "$LOG_FILE" 2>&1

RML_BIN="$RML_RUN_BENCH_DIR/rml_bench"
if [[ ! -x "$RML_BIN" ]]; then
  echo "[$RML_LOG_TAG] expected executable not found: $RML_BIN" >&2
  exit 1
fi

for bench in $BENCHMARKS; do
  bench_sizes="$(sizes_for_benchmark "$bench")"
  for size in $bench_sizes; do
    for run in $(seq 1 "$RUNS"); do
      cmd=(
        opam exec --switch="$RML_RUN_SWITCH" --
      )
      if [[ -n "$RML_RUN_RMLLIB" ]]; then
        cmd+=(env "RMLLIB=$RML_RUN_RMLLIB")
      fi
      cmd+=("$RML_BIN" --benchmark "$bench" --size "$size" --run "$run")
      echo "[$RML_LOG_TAG] bench=$bench size=$size run=$run"
      tmp_stdout="$(mktemp)"
      tmp_stderr="$(mktemp)"
      (
        "${TIME_CMD[@]}" "${cmd[@]}" >"$tmp_stdout" 2>"$tmp_stderr"
      )
      cat "$tmp_stdout" >> "$LOG_FILE"
      cat "$tmp_stderr" >> "$LOG_FILE"

      row="$(awk -F, '$1=="rml" && NF>=7 {line=$0} END{print line}' "$tmp_stdout")"
      if [[ -z "$row" ]]; then
        echo "[$RML_LOG_TAG] failed to parse benchmark CSV output (bench=$bench size=$size run=$run)" >&2
        rm -f "$tmp_stdout" "$tmp_stderr"
        exit 1
      fi
      if [[ "$RML_IMPL_ID" != "rml" ]]; then
        row="$(set_csv_impl "$row" "$RML_IMPL_ID")"
      fi
      peak_mb="$(require_peak_rss_mb "$tmp_stderr" "$RML_LOG_TAG bench=$bench size=$size run=$run")"
      row="$(set_csv_peak_mb "$row" "$peak_mb")"
      echo "$row" >> "$OUT_FILE"
      rm -f "$tmp_stdout" "$tmp_stderr"
    done
  done
done

write_run_metadata "$RML_IMPL_ID" "$OUT_FILE" "$RML_RUN_BENCH_DIR" "$RML_RUN_SWITCH"
{
  printf 'bench_dir=%s\n' "$RML_RUN_BENCH_DIR"
  printf 'rmlc=%s\n' "${RML_RUN_RMLC:-rmlc}"
  printf 'rmllib=%s\n' "${RML_RUN_RMLLIB:-}"
  printf 'extra_include=%s\n' "${RML_RUN_EXTRA_INCLUDE:-}"
  printf 'ocamlopt=%s\n' "$RML_RUN_OCAMLOPT"
} >> "${OUT_FILE%.csv}.meta"

echo "Done: $OUT_FILE"
