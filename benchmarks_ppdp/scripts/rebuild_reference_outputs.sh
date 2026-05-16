#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

python3 "$SCRIPT_DIR/summarize_reference.py" \
  --tempo "$ROOT/data/raw/tempo-reference-good.csv" \
  --rml "$ROOT/data/raw/rml-reference-good.csv" \
  --size "${SUMMARY_SIZE:-4642}"

python3 "$SCRIPT_DIR/plot_reference.py" \
  --tempo "$ROOT/data/raw/tempo-reference-good.csv" \
  --rml "$ROOT/data/raw/rml-reference-good.csv"

python3 "$SCRIPT_DIR/plot_reference_memory.py" \
  --tempo "$ROOT/data/raw/tempo-reference-good.csv" \
  --rml "$ROOT/data/raw/rml-reference-good.csv"
