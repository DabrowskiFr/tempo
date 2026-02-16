#!/bin/sh
set -eu

if [ "${1:-}" = "" ] || [ "${2:-}" = "" ]; then
  cat <<'EOF'
Usage:
  tools/demo/compress_mp4.sh <input_mp4> <output_mp4>

Example:
  tools/demo/compress_mp4.sh demos/raw.mp4 demos/final.mp4
EOF
  exit 2
fi

if ! command -v ffmpeg >/dev/null 2>&1; then
  echo "ffmpeg is required. Install it first (e.g. brew install ffmpeg)." >&2
  exit 1
fi

IN="$1"
OUT="$2"
OUT_DIR="$(dirname "$OUT")"
mkdir -p "$OUT_DIR"

ffmpeg -y \
  -i "$IN" \
  -c:v libx264 \
  -preset slow \
  -crf 23 \
  -pix_fmt yuv420p \
  -movflags +faststart \
  "$OUT"

echo "Compressed video written to: $OUT"
