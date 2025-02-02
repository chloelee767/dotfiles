#!/usr/bin/env bash
# Usage:
# ./sync-dir.sh <DIR>
# Example: ./sync-dir.sh shell

set -eou pipefail

DIR=$1

# Execute setup.sh if it exists.
# It is normally used to create the directories that I don't wish to be folded by stow.
# Global setup.sh
if [ -f "./setup.sh" ]; then
    echo "Executing ./setup.sh"
    bash "./setup.sh"
fi
# Directory specific setup.sh
if [ -f "$DIR/setup.sh" ]; then
    echo "Executing $DIR/setup.sh"
    bash "$DIR/setup.sh"
fi

stow -R -t ~ --ignore="^setup.sh$" -v "$DIR"
