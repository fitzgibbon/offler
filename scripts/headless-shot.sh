#!/usr/bin/env sh
# Screenshot a native example under Xvfb with lavapipe as the Vulkan device.
# Usage: headless-shot.sh <example> <outfile.png> [seconds]
#
# The traps this script exists to remember: SDL3 prefers Wayland whenever
# WAYLAND_DISPLAY is set, regardless of DISPLAY, so a run that only sets
# DISPLAY silently opens a window on the real desktop and screenshots an
# empty Xvfb root. And wgpu dlopens libvulkan.so.1, which `nix develop`
# already puts on LD_LIBRARY_PATH.
set -e
EXAMPLE=$1
OUT=$2
WAIT=${3:-6}

unset WAYLAND_DISPLAY
export SDL_VIDEODRIVER=x11
export DISPLAY=:99

# A third: the rig must not borrow the host's libraries. A host session's
# LD_LIBRARY_PATH can carry libraries built against a newer glibc than the
# dev shell's (alsa-lib, say), and then the shim fails to load; and the
# host's lavapipe ICD can be one the shell's processes cannot dlopen, which
# surfaces as a validation error from wgpuInstanceCreateSurface rather than
# anything about drivers. So keep only the shell's Vulkan loader, and take
# lavapipe from the shell's own mesa when it has one.
LD_LIBRARY_PATH=$(printf '%s' "${LD_LIBRARY_PATH:-}" | tr ':' '\n' | grep vulkan-loader | paste -sd: -)
export LD_LIBRARY_PATH
if [ -z "${VK_ICD_FILENAMES:-}" ]; then
  for d in $(printf '%s' "$PATH" | tr ':' ' '); do
    f=$(ls "$d"/../share/vulkan/icd.d/lvp_icd.*.json 2>/dev/null | grep -v i686 | head -1)
    if [ -n "$f" ]; then VK_ICD_FILENAMES=$f; break; fi
  done
  VK_ICD_FILENAMES=${VK_ICD_FILENAMES:-$(ls /run/opengl-driver/share/vulkan/icd.d/lvp_icd.*.json 2>/dev/null | head -1)}
fi
export VK_ICD_FILENAMES

Xvfb :99 -screen 0 1280x800x24 &
XVFB_PID=$!
trap 'kill $XVFB_PID 2>/dev/null || true' EXIT
sleep 1

"./build/exec/$EXAMPLE" &
APP_PID=$!
sleep "$WAIT"

WIN=$(xdotool search --name "$EXAMPLE" | head -1 || true)
if [ -z "$WIN" ]; then
  # Title may be capitalised or already carry status slots.
  WIN=$(xdotool search --onlyvisible --class '' | head -1 || true)
fi
import -window "${WIN:-root}" "$OUT"

kill $APP_PID 2>/dev/null || true
wait $APP_PID 2>/dev/null || true
echo "wrote $OUT"
