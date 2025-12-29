#!/bin/sh
set -eu

if [ "$(uname -s)" != "Darwin" ]; then
  echo "This script is for macOS (Darwin) only." >&2
  exit 1
fi

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
OUT_DIR="${OUT_DIR:-$ROOT/dist/macos-dmg}"
PROFILE="${DUNE_PROFILE:-release}"

usage() {
  cat <<USAGE
Usage:
  ./tools/release/build_games_dmg_macos.sh [game-slug ...]

Examples:
  ./tools/release/build_games_dmg_macos.sh
  ./tools/release/build_games_dmg_macos.sh game-univ snake-raylib

Environment variables:
  OUT_DIR            Output directory for .app/.dmg (default: dist/macos-dmg)
  DUNE_PROFILE       Dune profile (default: release)
  CODESIGN_IDENTITY  Optional codesign identity (Developer ID Application: ...)
USAGE
}

if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
  usage
  exit 0
fi

cd "$ROOT"

if [ "$#" -gt 0 ]; then
  GAMES="$*"
else
  GAMES=""
  for d in applications/game/*; do
    [ -d "$d/src" ] || continue
    [ -f "$d/src/main.ml" ] || continue
    slug="$(basename "$d")"
    GAMES="$GAMES $slug"
  done
fi

if [ -z "$(echo "$GAMES" | tr -d '[:space:]')" ]; then
  echo "No games found under applications/game/* with src/main.ml" >&2
  exit 1
fi

BUILD_TARGETS=""
for g in $GAMES; do
  target="./applications/game/$g/src/main.exe"
  if [ ! -f "$ROOT/applications/game/$g/src/main.ml" ]; then
    echo "Unknown game '$g' (expected $ROOT/applications/game/$g/src/main.ml)" >&2
    exit 1
  fi
  BUILD_TARGETS="$BUILD_TARGETS $target"
done

echo "Building games (profile=$PROFILE):$BUILD_TARGETS"
rm -f "$ROOT/_build/.lock" || true
dune build --profile "$PROFILE" $BUILD_TARGETS

mkdir -p "$OUT_DIR"

for g in $GAMES; do
  exe_rel="_build/default/applications/game/$g/src/main.exe"
  exe="$ROOT/$exe_rel"
  if [ ! -x "$exe" ]; then
    echo "Executable not found after build: $exe" >&2
    exit 1
  fi

  bundle_name="$g"
  app="$OUT_DIR/$bundle_name.app"
  macos_dir="$app/Contents/MacOS"
  resources_dir="$app/Contents/Resources"
  bin_name="$g"

  rm -rf "$app"
  mkdir -p "$macos_dir" "$resources_dir"
  cp "$exe" "$macos_dir/$bin_name"
  chmod +x "$macos_dir/$bin_name"

  cat > "$app/Contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key><string>$bundle_name</string>
  <key>CFBundleDisplayName</key><string>$bundle_name</string>
  <key>CFBundleIdentifier</key><string>com.tempo.$g</string>
  <key>CFBundleVersion</key><string>1.0.0</string>
  <key>CFBundleShortVersionString</key><string>1.0.0</string>
  <key>CFBundleExecutable</key><string>$bin_name</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>LSMinimumSystemVersion</key><string>12.0</string>
</dict>
</plist>
PLIST

  if [ -n "${CODESIGN_IDENTITY:-}" ]; then
    echo "Signing $app"
    codesign --deep --force --verify --verbose --sign "$CODESIGN_IDENTITY" "$app"
  fi

  dmg_root="$OUT_DIR/.dmg-root-$g"
  dmg="$OUT_DIR/$g-macos.dmg"
  rm -rf "$dmg_root"
  mkdir -p "$dmg_root"
  cp -R "$app" "$dmg_root/"
  ln -s /Applications "$dmg_root/Applications"

  rm -f "$dmg"
  hdiutil create -volname "$bundle_name" -srcfolder "$dmg_root" -ov -format UDZO "$dmg" >/dev/null
  rm -rf "$dmg_root"

  echo "Created: $app"
  echo "Created: $dmg"
done

echo "All artifacts are in: $OUT_DIR"
