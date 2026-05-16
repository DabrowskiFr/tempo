#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

usage() {
  cat <<'EOF'
Usage: run_locked_comparison.sh [options]

Runs a reproducible "current vs baseline" benchmark comparison with guardrails:
- baseline is selected by logical id (not ad-hoc commit)
- canonical baseline is pinned in benchmarks_ppdp/policy.env
- same harness/config is copied into baseline worktree before execution

Options:
  --baseline-id ID      Baseline id from policy.env (default: CANONICAL_BASELINE_ID)
  --tag TAG             Optional tag appended to output filenames
  --out-dir DIR         Output directory for comparison CSVs (default: data/processed)
  --worktree-dir DIR    Directory used for baseline worktrees (default: <repo>/.bench-worktrees)
  -h, --help            Show this help
EOF
}

BASELINE_ID="${CANONICAL_BASELINE_ID:-}"
OUT_DIR="$PROC_DIR"
WORKTREE_DIR="$TEMPO_ROOT/.bench-worktrees"
TAG=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --baseline-id)
      BASELINE_ID="$2"
      shift 2
      ;;
    --tag)
      TAG="$2"
      shift 2
      ;;
    --out-dir)
      OUT_DIR="$2"
      shift 2
      ;;
    --worktree-dir)
      WORKTREE_DIR="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [[ -z "${CANONICAL_BASELINE_ID:-}" || -z "${CANONICAL_BASELINE_COMMIT:-}" ]]; then
  echo "Missing canonical baseline in $POLICY_FILE." >&2
  exit 1
fi

resolve_baseline_commit() {
  local baseline_id="$1"
  if [[ "$baseline_id" == "$CANONICAL_BASELINE_ID" ]]; then
    printf '%s\n' "$CANONICAL_BASELINE_COMMIT"
    return 0
  fi
  if [[ "$baseline_id" == "${LEGACY_BASELINE_ID:-}" ]]; then
    if [[ "${ALLOW_LEGACY_BASELINE:-0}" != "1" ]]; then
      cat >&2 <<EOF
Legacy baseline '$baseline_id' is disabled by policy.
Set ALLOW_LEGACY_BASELINE=1 in policy.env for exceptional archaeology runs.
EOF
      exit 1
    fi
    printf '%s\n' "${LEGACY_BASELINE_COMMIT:-}"
    return 0
  fi

  echo "Unknown baseline id '$baseline_id'." >&2
  echo "Known ids: $CANONICAL_BASELINE_ID ${LEGACY_BASELINE_ID:-}" >&2
  exit 1
}

latest_csv() {
  local root="$1"
  local stem="$2"
  ls -1t "$root"/"$stem"-*.csv 2>/dev/null | head -n 1 || true
}

require_new_csv() {
  local before="$1"
  local after="$2"
  local label="$3"
  if [[ -z "$after" ]]; then
    echo "No output CSV found for $label." >&2
    exit 1
  fi
  if [[ "$before" == "$after" ]]; then
    echo "No new CSV produced for $label." >&2
    exit 1
  fi
}

BASELINE_COMMIT="$(resolve_baseline_commit "$BASELINE_ID")"
BASELINE_COMMIT_FULL="$(git -C "$TEMPO_ROOT" rev-parse "$BASELINE_COMMIT")"
BASELINE_KEY="${BASELINE_ID}-$(git -C "$TEMPO_ROOT" rev-parse --short "$BASELINE_COMMIT_FULL")"
BASELINE_WT="$WORKTREE_DIR/$BASELINE_KEY"
BASELINE_BENCH_ROOT="$BASELINE_WT/benchmarks_ppdp"

mkdir -p "$WORKTREE_DIR"
if [[ ! -e "$BASELINE_WT/.git" ]]; then
  echo "[compare] creating baseline worktree: $BASELINE_WT ($BASELINE_COMMIT_FULL)"
  git -C "$TEMPO_ROOT" worktree add --detach "$BASELINE_WT" "$BASELINE_COMMIT_FULL" >/dev/null
fi

RESOLVED_WT_HEAD="$(git -C "$BASELINE_WT" rev-parse HEAD)"
if [[ "$RESOLVED_WT_HEAD" != "$BASELINE_COMMIT_FULL" ]]; then
  echo "Baseline worktree head mismatch: expected $BASELINE_COMMIT_FULL, got $RESOLVED_WT_HEAD." >&2
  echo "Delete $BASELINE_WT and rerun." >&2
  exit 1
fi

mkdir -p "$BASELINE_BENCH_ROOT"
rsync -a --delete \
  --exclude '.worktrees' \
  --exclude 'data/raw' \
  --exclude 'data/processed' \
  --exclude 'figures' \
  --exclude 'logs' \
  "$BENCH_ROOT/" "$BASELINE_BENCH_ROOT/"
cp "$CONFIG_FILE" "$BASELINE_BENCH_ROOT/config.env"

echo "[compare] current commit : $(git -C "$TEMPO_ROOT" rev-parse HEAD)"
echo "[compare] baseline id    : $BASELINE_ID"
echo "[compare] baseline commit: $BASELINE_COMMIT_FULL"

before_current_tempo="$(latest_csv "$RAW_DIR" "tempo")"
before_current_rml="$(latest_csv "$RAW_DIR" "rml")"
"$SCRIPT_DIR/run_tempo.sh"
"$SCRIPT_DIR/run_rml.sh"
current_tempo_csv="$(latest_csv "$RAW_DIR" "tempo")"
current_rml_csv="$(latest_csv "$RAW_DIR" "rml")"
require_new_csv "$before_current_tempo" "$current_tempo_csv" "current tempo"
require_new_csv "$before_current_rml" "$current_rml_csv" "current rml"

before_base_tempo="$(latest_csv "$BASELINE_BENCH_ROOT/data/raw" "tempo")"
before_base_rml="$(latest_csv "$BASELINE_BENCH_ROOT/data/raw" "rml")"
(
  cd "$BASELINE_WT"
  "$BASELINE_BENCH_ROOT/scripts/run_tempo.sh"
  "$BASELINE_BENCH_ROOT/scripts/run_rml.sh"
)
baseline_tempo_csv="$(latest_csv "$BASELINE_BENCH_ROOT/data/raw" "tempo")"
baseline_rml_csv="$(latest_csv "$BASELINE_BENCH_ROOT/data/raw" "rml")"
require_new_csv "$before_base_tempo" "$baseline_tempo_csv" "baseline tempo"
require_new_csv "$before_base_rml" "$baseline_rml_csv" "baseline rml"

mkdir -p "$OUT_DIR"
compare_args=(
  --current-tempo "$current_tempo_csv"
  --current-rml "$current_rml_csv"
  --baseline-tempo "$baseline_tempo_csv"
  --baseline-rml "$baseline_rml_csv"
  --baseline-id "$BASELINE_ID"
  --baseline-commit "$BASELINE_COMMIT_FULL"
  --out-dir "$OUT_DIR"
)
if [[ -n "$TAG" ]]; then
  compare_args+=(--tag "$TAG")
fi
python3 "$SCRIPT_DIR/compare_tempo_versions.py" "${compare_args[@]}"

MANIFEST="$OUT_DIR/locked-compare-manifest-$(timestamp).txt"
{
  echo "baseline_id=$BASELINE_ID"
  echo "baseline_commit=$BASELINE_COMMIT_FULL"
  echo "current_commit=$(git -C "$TEMPO_ROOT" rev-parse HEAD)"
  echo "current_tempo_csv=$current_tempo_csv"
  echo "current_rml_csv=$current_rml_csv"
  echo "baseline_tempo_csv=$baseline_tempo_csv"
  echo "baseline_rml_csv=$baseline_rml_csv"
  echo "policy_file=$POLICY_FILE"
  echo "config_file=$CONFIG_FILE"
} > "$MANIFEST"

echo "Manifest: $MANIFEST"
