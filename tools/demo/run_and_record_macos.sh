#!/bin/sh
set -eu

if [ "${1:-}" = "" ] || [ "${2:-}" = "" ] || [ "${3:-}" = "" ]; then
  cat <<'EOF'
Usage:
  tools/demo/run_and_record_macos.sh <app> <duration_sec> <output_mp4> [video_device] [fps] [startup_delay_sec]

Example:
  tools/demo/run_and_record_macos.sh game-univ 30 demos/game-univ-demo.mp4 "1:none" 30 8

Notes:
  - app: one of dune exec ./applications/run -- help
  - video_device defaults to: 1:none
  - fps defaults to: 30
  - startup_delay_sec defaults to: 8
  - Use tools/demo/list_avfoundation_devices_macos.sh to inspect devices.
EOF
  exit 2
fi

if ! command -v ffmpeg >/dev/null 2>&1; then
  echo "ffmpeg is required. Install it first (e.g. brew install ffmpeg)." >&2
  exit 1
fi

APP="$1"
DURATION="$2"
OUT="$3"
DEVICE="${4:-1:none}"
FPS="${5:-30}"
STARTUP_DELAY="${6:-8}"

APP_DIR=""
case "$APP" in
  game-univ|snake-raylib|temporal-arena|time-echo-jumper)
    APP_DIR="applications/game/$APP"
    ;;
  boids-raylib|ca-continuous-raylib|emergent-city-lab|solar-system-raylib|temporal-physics-sandbox)
    APP_DIR="applications/simulation/$APP"
    ;;
  logicgroove|logicgroove-evolution|temporalsim)
    APP_DIR="applications/logic/$APP"
    ;;
  *)
    echo "Unknown app: $APP" >&2
    echo "Run: dune exec ./applications/run -- help" >&2
    exit 2
    ;;
esac

OUT_DIR="$(dirname "$OUT")"
mkdir -p "$OUT_DIR"

GAME_PID=""
cleanup() {
  if [ -n "$GAME_PID" ] && kill -0 "$GAME_PID" 2>/dev/null; then
    kill "$GAME_PID" 2>/dev/null || true
    wait "$GAME_PID" 2>/dev/null || true
  fi
}
trap cleanup EXIT INT TERM

echo "Starting game: $APP"
echo "Prebuilding target app (avoids black recording on first run)..."
dune build "./$APP_DIR/src/main.exe" >/dev/null

dune exec ./applications/run -- "$APP" &
GAME_PID=$!

# Give the game time to open its window before capture starts.
sleep "$STARTUP_DELAY"

echo "Recording $DURATION seconds to $OUT"
if ! ffmpeg -y \
  -f avfoundation \
  -framerate "$FPS" \
  -i "$DEVICE" \
  -t "$DURATION" \
  -c:v libx264 \
  -preset veryfast \
  -crf 20 \
  -pix_fmt yuv420p \
  "$OUT"; then
  echo "ffmpeg capture failed for device '$DEVICE'." >&2
  echo "Run './tools/demo/list_avfoundation_devices_macos.sh' and verify Screen Recording permission." >&2
  exit 1
fi

echo "Done: $OUT"
