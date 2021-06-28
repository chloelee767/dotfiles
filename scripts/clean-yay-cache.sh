#!/usr/bin/env bash
set -euo pipefail

yaycache="$(find /home/chloe/.cache/yay -maxdepth 1 -type d | awk '{ print "-c " $1 }' | tail -n +2)"
echo "==> Keep last 3 installed versions"
/usr/bin/paccache -rvk3 $yaycache
echo "==> Clear cache for packages no longer installed"
/usr/bin/paccache -ruvk0 $yaycache
