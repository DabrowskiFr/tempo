#!/bin/sh
set -eu

if [ -n "${DUNE_SOURCEROOT:-}" ]; then
  cd "${DUNE_SOURCEROOT}"
else
  cd "$(dirname "$0")/../.."
fi

OUT_DIR="bench/results/core"

parse_out_dir() {
  PREV=""
  for ARG in "$@"; do
    if [ "$PREV" = "--out-dir" ]; then
      OUT_DIR="$ARG"
      PREV=""
      continue
    fi
    case "$ARG" in
      --out-dir=*)
        OUT_DIR="${ARG#--out-dir=}"
        PREV=""
        ;;
      --out-dir)
        PREV="--out-dir"
        ;;
      *)
        PREV=""
        ;;
    esac
  done
}

convert_svg_to_pdf() {
  if command -v rsvg-convert >/dev/null 2>&1; then
    FIND_CONVERTER="rsvg"
  elif command -v inkscape >/dev/null 2>&1; then
    FIND_CONVERTER="inkscape"
  elif command -v cairosvg >/dev/null 2>&1; then
    FIND_CONVERTER="cairosvg"
  else
    echo "No SVG->PDF converter found (rsvg-convert/inkscape/cairosvg)." >&2
    echo "Install one, e.g. on macOS: brew install librsvg" >&2
    exit 2
  fi

  COUNT=0
  find "$OUT_DIR" -type f -name '*.svg' | while IFS= read -r SVG; do
    PDF="${SVG%.svg}.pdf"
    case "$FIND_CONVERTER" in
      rsvg)
        rsvg-convert -f pdf -o "$PDF" "$SVG"
        ;;
      inkscape)
        inkscape "$SVG" --export-type=pdf --export-filename="$PDF" >/dev/null 2>&1
        ;;
      cairosvg)
        cairosvg "$SVG" -f pdf -o "$PDF"
        ;;
    esac
    COUNT=$((COUNT + 1))
  done
  echo "Converted SVG to PDF in: $OUT_DIR"
}

parse_out_dir "$@"
dune exec ./bench/core_bench.exe -- "$@"
convert_svg_to_pdf
