#!/usr/bin/env sh
# Run the Rust native layer's smoke example under Xvfb with lavapipe, and
# screenshot it mid-run. Usage: rust-smoke.sh [outfile.png] [frames]
# The same traps as headless-shot.sh: force X11, and keep the host's
# libraries out -- here keeping winit's dlopened X11/xkbcommon libraries too.
set -e
OUT=${1:-build/rust-smoke.png}
FRAMES=${2:-600}
ROOT=$(cd "$(dirname "$0")/.." && pwd)

unset WAYLAND_DISPLAY
export DISPLAY=:98

LD_LIBRARY_PATH=$(printf '%s' "${LD_LIBRARY_PATH:-}" | tr ':' '\n' \
  | grep -E 'vulkan-loader|libxkbcommon|libx11|libxcursor|libxrandr|libxi|wayland|alsa-lib|systemd' | paste -sd: -)
export LD_LIBRARY_PATH
if [ -z "${VK_ICD_FILENAMES:-}" ]; then
  for d in $(printf '%s' "$PATH" | tr ':' ' '); do
    f=$(ls "$d"/../share/vulkan/icd.d/lvp_icd.*.json 2>/dev/null | grep -v i686 | head -1)
    if [ -n "$f" ]; then VK_ICD_FILENAMES=$f; break; fi
  done
  VK_ICD_FILENAMES=${VK_ICD_FILENAMES:-$(ls /run/opengl-driver/share/vulkan/icd.d/lvp_icd.*.json 2>/dev/null | head -1)}
fi
export VK_ICD_FILENAMES

(cd "$ROOT/rust" && cargo build --release --example smoke)

Xvfb :98 -screen 0 1280x800x24 &
XVFB_PID=$!
trap 'kill $XVFB_PID 2>/dev/null || true' EXIT
sleep 1

mkdir -p "$(dirname "$OUT")"
SMOKE_SHADERS=${SMOKE_SHADERS:-} SMOKE_FRAMES=$FRAMES "$ROOT/rust/target/release/examples/smoke" &
APP_PID=$!
sleep 2
import -window root "$OUT"
wait $APP_PID
echo "wrote $OUT"
