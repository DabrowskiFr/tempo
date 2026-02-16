#!/bin/sh
set -eu

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"

fail() {
  printf 'ERROR: %s\n' "$*" >&2
  exit 1
}

require_file() {
  file="$1"
  [ -f "$ROOT/$file" ] || fail "missing required file: $file"
}

require_contains() {
  file="$1"
  needle="$2"
  desc="$3"
  grep -Fq "$needle" "$ROOT/$file" || fail "$desc (expected '$needle' in $file)"
}

log() {
  printf '%s\n' "$*"
}

log "[doc-freshness] checking required release documentation files"
require_file "applications/ARCHITECTURE.md"
require_file "applications/SMOKE_MATRIX.md"
require_file "applications/RELEASE_CHECKLIST.md"
require_file "docs/COMPATIBILITY_POLICY.md"
require_file "Tempo.md"
require_file "RELEASE_NOTES.md"

log "[doc-freshness] checking cross-references in Tempo.md"
require_contains "Tempo.md" "applications/ARCHITECTURE.md" "Tempo.md must reference architecture documentation"
require_contains "Tempo.md" "applications/SMOKE_MATRIX.md" "Tempo.md must reference smoke matrix documentation"
require_contains "Tempo.md" "docs/COMPATIBILITY_POLICY.md" "Tempo.md must reference compatibility policy"
require_contains "Tempo.md" "RELEASE_NOTES.md" "Tempo.md must reference release notes"

log "[doc-freshness] checking release checklist gate entries"
require_contains "applications/RELEASE_CHECKLIST.md" "applications/ARCHITECTURE.md" "release checklist must include architecture gate"
require_contains "applications/RELEASE_CHECKLIST.md" "applications/SMOKE_MATRIX.md" "release checklist must include smoke matrix gate"
require_contains "applications/RELEASE_CHECKLIST.md" "./tools/release/validate_apps.sh" "release checklist must include app validator command"
require_contains "applications/RELEASE_CHECKLIST.md" "dune build" "release checklist must include build command"
require_contains "applications/RELEASE_CHECKLIST.md" "dune runtest" "release checklist must include test command"

log "[doc-freshness] checking architecture and compatibility policy anchors"
require_contains "applications/ARCHITECTURE.md" "applications/advanced" "architecture doc must describe advanced category"
require_contains "applications/ARCHITECTURE.md" "applications/simple-demos" "architecture doc must describe simple-demos category"
require_contains "docs/COMPATIBILITY_POLICY.md" "Core synchronous API" "compatibility policy must define core API scope"
require_contains "docs/COMPATIBILITY_POLICY.md" "Layer2" "compatibility policy must define layer2 scope"

log "[doc-freshness] checking release notes migration section"
if ! grep -Eq '^## +Migration|^### +Migration' "$ROOT/RELEASE_NOTES.md"; then
  fail "release notes must include a Migration section"
fi

log "[doc-freshness] OK"
