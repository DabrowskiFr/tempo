#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

"$SCRIPT_DIR/run_tempo.sh"
"$SCRIPT_DIR/run_rml.sh"
if [[ "$RML_OCAML5_ENABLED" == "1" ]]; then
  "$SCRIPT_DIR/run_rml_ocaml5.sh"
fi
