#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

RML_OCAML5_SWITCH="${RML_OCAML5_SWITCH:-$TEMPO_SWITCH}"
RML_OCAML5_BENCH_DIR="${RML_OCAML5_BENCH_DIR:-$RML_BENCH_DIR}"
RML_OCAML5_IMPL="${RML_OCAML5_IMPL:-rml_ocaml5}"
RML_OCAML5_RUN_NAME="${RML_OCAML5_RUN_NAME:-rml_ocaml5}"
RML_OCAML5_RMLC="${RML_OCAML5_RMLC:-}"
RML_OCAML5_RMLLIB="${RML_OCAML5_RMLLIB:-}"

ensure_switch_exists "$RML_OCAML5_SWITCH"

resolve_rmlc() {
  if [[ -n "$RML_OCAML5_RMLC" ]]; then
    if [[ "$RML_OCAML5_RMLC" == */* ]]; then
      if [[ ! -x "$RML_OCAML5_RMLC" ]]; then
        echo "[rml-ocaml5] RML_OCAML5_RMLC is not executable: $RML_OCAML5_RMLC" >&2
        exit 1
      fi
      printf '%s\n' "$RML_OCAML5_RMLC"
      return 0
    fi
    if ! opam exec --switch="$RML_OCAML5_SWITCH" -- sh -lc "command -v \"$RML_OCAML5_RMLC\" >/dev/null 2>&1"; then
      echo "[rml-ocaml5] compiler '$RML_OCAML5_RMLC' not found in switch=$RML_OCAML5_SWITCH" >&2
      exit 1
    fi
    printf '%s\n' "$RML_OCAML5_RMLC"
    return 0
  fi

  if opam exec --switch="$RML_OCAML5_SWITCH" -- sh -lc 'command -v rmlc >/dev/null 2>&1'; then
    printf '%s\n' "rmlc"
    return 0
  fi

  cat >&2 <<EOF
[rml-ocaml5] no ReactiveML compiler found for OCaml-5 run.
Set RML_OCAML5_RMLC to your patched compiler path, for example:
  RML_OCAML5_RMLC=/absolute/path/to/rml-*/compiler/rmlc
EOF
  exit 1
}

rmlc_cmd="$(resolve_rmlc)"

prepare_rmllib_bundle() {
  local source_root="$1"
  local runtime_dir="$source_root/interpreter/lco"
  local stdlib_dir="$source_root/stdlib"
  local bundle_dir="$source_root/.bench-rmllib/lco"

  if [[ ! -d "$runtime_dir" || ! -d "$stdlib_dir" ]]; then
    return 1
  fi
  if [[ ! -f "$runtime_dir/rmllib.cmxa" || ! -f "$stdlib_dir/stdlib.rzi" ]]; then
    return 1
  fi

  mkdir -p "$bundle_dir"
  rsync -a --delete "$runtime_dir/" "$bundle_dir/"
  rsync -a "$stdlib_dir/" "$bundle_dir/"
  printf '%s\n' "$bundle_dir"
}

rmllib_dir="$RML_OCAML5_RMLLIB"
source_root=""
if [[ -z "$rmllib_dir" && "$rmlc_cmd" == */* ]]; then
  source_root="$(cd "$(dirname "$rmlc_cmd")/.." && pwd)"
  if bundle="$(prepare_rmllib_bundle "$source_root")"; then
    rmllib_dir="$bundle"
  fi
fi

extra_include=""
if [[ -n "$source_root" && -d "$source_root/interpreter" ]]; then
  extra_include="-I $source_root/interpreter"
fi

echo "[rml-ocaml5] switch=$RML_OCAML5_SWITCH impl=$RML_OCAML5_IMPL rmlc=$rmlc_cmd rmllib=${rmllib_dir:-<default>} extra_include=${extra_include:-<none>}"

RML_RUN_NAME="$RML_OCAML5_RUN_NAME" \
RML_IMPL_ID="$RML_OCAML5_IMPL" \
RML_RUN_SWITCH="$RML_OCAML5_SWITCH" \
RML_RUN_BENCH_DIR="$RML_OCAML5_BENCH_DIR" \
RML_RUN_RMLC="$rmlc_cmd" \
RML_RUN_RMLLIB="$rmllib_dir" \
RML_RUN_EXTRA_INCLUDE="$extra_include" \
RML_RUN_OCAMLOPT="ocamlopt" \
RML_LOG_TAG="rml-ocaml5" \
"$SCRIPT_DIR/run_rml.sh"
