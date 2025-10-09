#!/usr/bin/env bash

# Performs setup + creates symlinks for the specified modules.
#
# Usage:
# ./sync-dir.sh <DIR1> [DIR2] [DIR3] ...
# Example: ./sync-dir.sh shell general doom

set -eou pipefail

if [ $# -eq 0 ]; then
    echo "Error: No directories specified"
    echo "Usage: ./sync-dir.sh <DIR1> [DIR2] [DIR3] ..."
    exit 1
fi

# Execute global setup.sh if it exists
if [ -f "./setup.sh" ]; then
    echo "Executing ./setup.sh"
    bash "./setup.sh"
fi

for DIR in "$@"; do
    echo "Syncing directory: $DIR"

    # Directory specific setup.sh
    if [ -f "$DIR/setup.sh" ]; then
        echo "Executing $DIR/setup.sh"
        bash "$DIR/setup.sh"
    fi

    stow -R -t ~ --ignore="^setup.sh$" -v "$DIR" 2>&1 | grep -v "BUG in find_stowed_path? Absolute/relative mismatch"
done
