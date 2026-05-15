#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BENCH_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
TEMPO_ROOT="$(cd "$BENCH_ROOT/.." && pwd)"
CONFIG_FILE="$BENCH_ROOT/config.env"

if [[ ! -f "$CONFIG_FILE" ]]; then
  echo "Missing $CONFIG_FILE (copy config.env.example first)." >&2
  exit 1
fi

# shellcheck disable=SC1090
source "$CONFIG_FILE"

RAW_DIR="$BENCH_ROOT/data/raw"
PROC_DIR="$BENCH_ROOT/data/processed"
LOG_DIR="$BENCH_ROOT/logs"

TEMPO_BENCH_DIR="$BENCH_ROOT/programs/tempo"
RML_BENCH_DIR="$BENCH_ROOT/programs/reactiveml"

mkdir -p "$RAW_DIR" "$PROC_DIR" "$LOG_DIR"

REQUIRED_TEMPO_SWITCH="5.4.1+options"
REQUIRED_RML_SWITCH="rml-4.14"

ensure_required_switches() {
  if [[ "${TEMPO_SWITCH:-}" != "$REQUIRED_TEMPO_SWITCH" ]]; then
    echo "Invalid TEMPO_SWITCH='$TEMPO_SWITCH' in $CONFIG_FILE." >&2
    echo "Expected TEMPO_SWITCH='$REQUIRED_TEMPO_SWITCH'." >&2
    exit 1
  fi
  if [[ "${RML_SWITCH:-}" != "$REQUIRED_RML_SWITCH" ]]; then
    echo "Invalid RML_SWITCH='$RML_SWITCH' in $CONFIG_FILE." >&2
    echo "Expected RML_SWITCH='$REQUIRED_RML_SWITCH'." >&2
    exit 1
  fi
}

ensure_switch_exists() {
  local switch_name="$1"
  if ! opam switch list --short | grep -Fx "$switch_name" >/dev/null 2>&1; then
    echo "Missing opam switch '$switch_name'." >&2
    echo "Create it before running benchmarks." >&2
    exit 1
  fi
}

ensure_required_switches
ensure_switch_exists "$TEMPO_SWITCH"
ensure_switch_exists "$RML_SWITCH"

if /usr/bin/time -l true >/dev/null 2>&1; then
  TIME_CMD=(/usr/bin/time -l)
elif /usr/bin/time -v true >/dev/null 2>&1; then
  TIME_CMD=(/usr/bin/time -v)
else
  echo "Could not find a compatible '/usr/bin/time' mode for peak RSS measurement." >&2
  exit 1
fi

timestamp() {
  date "+%Y%m%d-%H%M%S"
}

extract_peak_rss_mb() {
  local stderr_file="$1"
  local rss_raw=""

  # macOS / BSD time -l: value is bytes.
  rss_raw="$(awk '/maximum resident set size/{print $1; exit}' "$stderr_file" || true)"
  if [[ -n "$rss_raw" ]]; then
    awk -v b="$rss_raw" 'BEGIN { printf "%.3f", b / (1024.0 * 1024.0) }'
    return 0
  fi

  # GNU time -v: value is kbytes.
  rss_raw="$(awk -F: '/Maximum resident set size \(kbytes\)/{gsub(/^[ \t]+/, "", $2); print $2; exit}' "$stderr_file" || true)"
  if [[ -n "$rss_raw" ]]; then
    awk -v kb="$rss_raw" 'BEGIN { printf "%.3f", kb / 1024.0 }'
    return 0
  fi

  return 1
}

set_csv_peak_mb() {
  local csv_line="$1"
  local peak_mb="$2"
  awk -F, -v OFS=, -v peak="$peak_mb" 'NF >= 7 { $7 = peak; print }' <<<"$csv_line"
}

sizes_for_benchmark() {
  local bench="$1"
  local var_name=""
  local sizes=""

  case "$bench" in
    propagation_chains) var_name="SIZES_PROPAGATION_CHAINS" ;;
    broadcast_expansion) var_name="SIZES_BROADCAST_EXPANSION" ;;
    fork_explosion) var_name="SIZES_FORK_EXPLOSION" ;;
    guarded_cascades) var_name="SIZES_GUARDED_CASCADES" ;;
    nested_preemption) var_name="SIZES_NESTED_PREEMPTION" ;;
  esac

  if [[ -n "$var_name" ]]; then
    # shellcheck disable=SC2086
    sizes="${!var_name:-}"
  fi
  if [[ -z "$sizes" ]]; then
    sizes="${SIZES:-}"
  fi
  if [[ -z "$sizes" ]]; then
    echo "No size grid configured for benchmark '$bench'." >&2
    exit 1
  fi

  printf '%s\n' "$sizes"
}
