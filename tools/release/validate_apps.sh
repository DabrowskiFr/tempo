#!/bin/sh
set -eu

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"

PROFILE="${DUNE_PROFILE:-release}"

ADVANCED_APPS="game-univ refactor tempo-core-studio"
SIMPLE_APPS="snake-raylib boids-raylib ca-continuous-raylib lenia-raylib solar-system-raylib logicgroove temporalsim"
ALL_APPS="$ADVANCED_APPS $SIMPLE_APPS"

log() {
  printf '%s\n' "$*"
}

fail() {
  printf 'ERROR: %s\n' "$*" >&2
  exit 1
}

usage() {
  cat <<'EOF'
Usage:
  ./tools/release/validate_apps.sh

What it validates:
  1) Router compatibility and help commands.
  2) README presence + required section headers for all apps.
  3) Build of all app executables.
  4) Launcher help contract for all app run scripts.
  5) Headless smoke runs for advanced apps with deterministic modes.
EOF
}

if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
  usage
  exit 0
fi

check_readme_sections() {
  app_dir="$1"
  readme="$app_dir/README.md"
  [ -f "$readme" ] || fail "Missing README: $readme"

  for section in \
    "## Overview" \
    "## Controls / Inputs" \
    "## Launch Commands" \
    "## CLI Options" \
    "## Dependencies" \
    "## Headless / Reproducible Mode" \
    "## Troubleshooting" \
    "## Release Status"
  do
    rg -q "^${section}$" "$readme" || fail "README missing section '${section}' in $readme"
  done
}

resolve_app_dir() {
  app="$1"
  if [ -d "$ROOT/applications/advanced/$app" ]; then
    printf '%s\n' "applications/advanced/$app"
    return 0
  fi
  if [ -d "$ROOT/applications/simple-demos/$app" ]; then
    printf '%s\n' "applications/simple-demos/$app"
    return 0
  fi
  return 1
}

log "[1/5] Router help checks"
rm -f "$ROOT/_build/.lock" || true
dune exec ./applications/run -- help >/dev/null
dune exec ./applications/reconfiguration/run -- help >/dev/null

log "[2/5] README quality checks"
check_readme_sections "$ROOT/applications/advanced/game-univ"
check_readme_sections "$ROOT/applications/advanced/refactor"
for app in $SIMPLE_APPS; do
  check_readme_sections "$ROOT/applications/simple-demos/$app"
done

log "[3/5] Build all app executables (profile=$PROFILE)"
TARGETS=""
for app in $ALL_APPS; do
  app_dir="$(resolve_app_dir "$app")" || fail "Unknown app dir for $app"
  [ -f "$ROOT/$app_dir/src/main.ml" ] || fail "Missing main.ml in $app_dir"
  TARGETS="$TARGETS ./$app_dir/src/main.exe"
done

rm -f "$ROOT/_build/.lock" || true
dune build --profile "$PROFILE" $TARGETS

log "[4/5] Run-script help contract checks"
for app in $ALL_APPS; do
  app_dir="$(resolve_app_dir "$app")" || fail "Unknown app dir for $app"
  run_script="$ROOT/$app_dir/run"
  [ -x "$run_script" ] || fail "Run script missing or not executable: $run_script"
  "$run_script" --help >/dev/null
done

log "[5/5] Headless smoke checks"
# Keep smoke deterministic and short.
dune exec ./applications/run -- refactor -- --headless --steps 120 --seed 1 >/dev/null
dune exec ./applications/run -- tempo-core-studio -- --headless --instants 12 >/dev/null

log "Validation OK"
