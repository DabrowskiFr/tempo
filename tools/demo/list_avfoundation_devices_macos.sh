#!/bin/sh
set -eu

if ! command -v ffmpeg >/dev/null 2>&1; then
  echo "ffmpeg is required. Install it first (e.g. brew install ffmpeg)." >&2
  exit 1
fi

echo "Listing AVFoundation devices (video/audio) ..."
echo "Use the indices shown by ffmpeg in the [AVFoundation input device] lines."
ffmpeg -hide_banner -f avfoundation -list_devices true -i "" 2>&1 || true
cat <<'EOF'

If no video device appears:
  1) System Settings -> Privacy & Security -> Screen Recording
  2) Enable permission for your terminal app (Terminal/iTerm/Codex)
  3) Restart the terminal app, then run this command again.
EOF
