#!/usr/bin/env bash
# run.sh — helper subcommands for the agents-sandbox.
#
# Subcommands:
#   --image-name   Print the Docker image name used by the sandbox.

set -euo pipefail

AGENTS_SANDBOX_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case "${1:-}" in
  --image-name)
    tr -d '[:space:]' < "$AGENTS_SANDBOX_DIR/image"
    ;;
  *)
    echo "Usage: run.sh <subcommand>" >&2
    echo "  --image-name   Print the Docker image name" >&2
    exit 1
    ;;
esac
