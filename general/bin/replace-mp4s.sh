#!/usr/bin/env bash
set -euo pipefail

for mp4_file in **/*.mp4; do
    fileSize=$(wc -c "$mp4_file" | sed 's/\s.*$//')
    if [[ fileSize -gt 0 ]]; then
        echo $mp4_file
        trash "$mp4_file"
        touch "$mp4_file"
    fi
done
