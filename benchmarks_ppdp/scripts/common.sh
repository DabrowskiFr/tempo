#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BENCH_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
TEMPO_ROOT="$(cd "$BENCH_ROOT/.." && pwd)"
CONFIG_FILE="$BENCH_ROOT/config.env"
POLICY_FILE="$BENCH_ROOT/policy.env"

if [[ ! -f "$CONFIG_FILE" ]]; then
  echo "Missing $CONFIG_FILE (copy config.env.example first)." >&2
  exit 1
fi

# Policy first (defaults), then local config (explicit overrides).
if [[ -f "$POLICY_FILE" ]]; then
  # shellcheck disable=SC1090
  source "$POLICY_FILE"
fi

# shellcheck disable=SC1090
source "$CONFIG_FILE"

MAX_REASONABLE_SIZE="${MAX_REASONABLE_SIZE:-5000}"
ALLOW_LARGE_N="${ALLOW_LARGE_N:-0}"
RML_OCAML5_ENABLED="${RML_OCAML5_ENABLED:-0}"
RML_OCAML5_SWITCH="${RML_OCAML5_SWITCH:-$TEMPO_SWITCH}"
RML_OCAML5_IMPL="${RML_OCAML5_IMPL:-rml_ocaml5}"
RML_OCAML5_RUN_NAME="${RML_OCAML5_RUN_NAME:-rml_ocaml5}"
RML_OCAML5_RMLC="${RML_OCAML5_RMLC:-}"
RML_OCAML5_RMLLIB="${RML_OCAML5_RMLLIB:-}"

RAW_DIR="$BENCH_ROOT/data/raw"
PROC_DIR="$BENCH_ROOT/data/processed"
LOG_DIR="$BENCH_ROOT/logs"

TEMPO_BENCH_DIR="$BENCH_ROOT/programs/tempo"
RML_BENCH_DIR="$BENCH_ROOT/programs/reactiveml"
RML_OCAML5_BENCH_DIR="${RML_OCAML5_BENCH_DIR:-$RML_BENCH_DIR}"

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
  if [[ "$RML_OCAML5_ENABLED" != "0" && "$RML_OCAML5_ENABLED" != "1" ]]; then
    echo "Invalid RML_OCAML5_ENABLED='$RML_OCAML5_ENABLED' in $CONFIG_FILE." >&2
    echo "Expected RML_OCAML5_ENABLED='0' or '1'." >&2
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
if [[ "$RML_OCAML5_ENABLED" == "1" ]]; then
  ensure_switch_exists "$RML_OCAML5_SWITCH"
fi

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

require_peak_rss_mb() {
  local stderr_file="$1"
  local context="${2:-benchmark run}"
  local peak_mb=""
  if ! peak_mb="$(extract_peak_rss_mb "$stderr_file")"; then
    cat >&2 <<EOF
Failed to extract OS peak RSS for $context.
Benchmark campaigns are configured to use OS RSS only (system time output).
Check availability/format of '/usr/bin/time' and stderr capture.
EOF
    return 1
  fi
  printf '%s\n' "$peak_mb"
}

set_csv_peak_mb() {
  local csv_line="$1"
  local peak_mb="$2"
  awk -F, -v OFS=, -v peak="$peak_mb" 'NF >= 7 { $7 = peak; print }' <<<"$csv_line"
}

set_csv_impl() {
  local csv_line="$1"
  local impl="$2"
  awk -F, -v OFS=, -v impl="$impl" 'NF >= 7 { $1 = impl; print }' <<<"$csv_line"
}

validate_positive_integer() {
  local name="$1"
  local value="$2"
  if [[ ! "$value" =~ ^[0-9]+$ ]] || (( value <= 0 )); then
    echo "Invalid $name='$value'. Expected a strictly positive integer." >&2
    exit 1
  fi
}

sizes_for_benchmark() {
  local bench="$1"
  local var_name=""
  local sizes=""

  case "$bench" in
    propagation_chains) var_name="SIZES_PROPAGATION_CHAINS" ;;
    propagation_chains_multi) var_name="SIZES_PROPAGATION_CHAINS_MULTI" ;;
    broadcast_expansion) var_name="SIZES_BROADCAST_EXPANSION" ;;
    fork_explosion) var_name="SIZES_FORK_EXPLOSION" ;;
    guarded_cascades) var_name="SIZES_GUARDED_CASCADES" ;;
    guarded_cascades_multi) var_name="SIZES_GUARDED_CASCADES_MULTI" ;;
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

validate_campaign_policy() {
  validate_positive_integer "RUNS" "${RUNS:-}"

  for bench in $BENCHMARKS; do
    case "$bench" in
      propagation_chains|propagation_chains_multi|broadcast_expansion|fork_explosion|guarded_cascades|guarded_cascades_multi|nested_preemption) ;;
      *)
        echo "Unknown benchmark '$bench' in BENCHMARKS." >&2
        exit 1
        ;;
    esac

    local bench_sizes
    bench_sizes="$(sizes_for_benchmark "$bench")"
    for n in $bench_sizes; do
      validate_positive_integer "size($bench)" "$n"
      if (( n > MAX_REASONABLE_SIZE )) && [[ "$ALLOW_LARGE_N" != "1" ]]; then
        cat >&2 <<EOF
Refusing size $n for benchmark '$bench': MAX_REASONABLE_SIZE=$MAX_REASONABLE_SIZE.
Set ALLOW_LARGE_N=1 in config.env (or policy.env) only for explicit stress runs.
EOF
        exit 1
      fi
    done
  done
}

git_head_or_unknown() {
  local repo="$1"
  if git -C "$repo" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    git -C "$repo" rev-parse HEAD
  else
    echo "unknown"
  fi
}

write_run_metadata() {
  local impl="$1"
  local out_csv="$2"
  local repo_root="$3"
  local switch_name="$4"

  local meta_file="${out_csv%.csv}.meta"
  local git_head git_describe
  git_head="$(git_head_or_unknown "$repo_root")"
  git_describe="$(git -C "$repo_root" describe --tags --always --dirty 2>/dev/null || echo unknown)"

  {
    printf 'impl=%s\n' "$impl"
    printf 'timestamp_utc=%s\n' "$(date -u '+%Y-%m-%dT%H:%M:%SZ')"
    printf 'repo_root=%s\n' "$repo_root"
    printf 'git_head=%s\n' "$git_head"
    printf 'git_describe=%s\n' "$git_describe"
    printf 'opam_switch=%s\n' "$switch_name"
    printf 'runs=%s\n' "$RUNS"
    printf 'benchmarks=%s\n' "$BENCHMARKS"
    printf 'sizes_global=%s\n' "${SIZES:-}"
    printf 'sizes_propagation_chains=%s\n' "${SIZES_PROPAGATION_CHAINS:-}"
    printf 'sizes_propagation_chains_multi=%s\n' "${SIZES_PROPAGATION_CHAINS_MULTI:-}"
    printf 'sizes_broadcast_expansion=%s\n' "${SIZES_BROADCAST_EXPANSION:-}"
    printf 'sizes_fork_explosion=%s\n' "${SIZES_FORK_EXPLOSION:-}"
    printf 'sizes_guarded_cascades=%s\n' "${SIZES_GUARDED_CASCADES:-}"
    printf 'sizes_guarded_cascades_multi=%s\n' "${SIZES_GUARDED_CASCADES_MULTI:-}"
    printf 'sizes_nested_preemption=%s\n' "${SIZES_NESTED_PREEMPTION:-}"
    printf 'max_reasonable_size=%s\n' "$MAX_REASONABLE_SIZE"
    printf 'allow_large_n=%s\n' "$ALLOW_LARGE_N"
    printf 'peak_mb_source=%s\n' "os_rss_time"
  } > "$meta_file"

  echo "Metadata: $meta_file"
}

validate_campaign_policy
