#!/usr/bin/env bash
set -euo pipefail

X_SHIFT=$1
Y_SHIFT=$2

# sets WINODW, X, Y, WIDTH, HEIGHT, SCREEN (always 0)
eval $(xdotool getactivewindow getwindowgeometry -shell)

# Position excludes window decorations, so for me they're off by 2 every time
xdotool getactivewindow windowmove $((X + X_SHIFT - 2)) $((Y + Y_SHIFT - 2))
