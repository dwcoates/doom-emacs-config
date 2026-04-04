#!/usr/bin/env bash
# install.sh — set up the Docker sandbox for this repo.
#
# - Builds the Docker image if not already present locally.
# - Installs claude-sandbox to INSTALL_DIR (default: /usr/local/bin).
#
# Usage: bash .claude/install.sh [install-dir]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GIT_ROOT="$(dirname "$SCRIPT_DIR")"
INSTALL_DIR="${1:-/usr/local/bin}"

IMAGE=$(tr -d '[:space:]' < "$SCRIPT_DIR/sandbox-image")

echo "==> Checking Docker image: $IMAGE"
if docker image inspect "$IMAGE" &>/dev/null; then
  echo "    Image already present, skipping build."
else
  echo "==> Building $IMAGE ..."
  docker build -f "$SCRIPT_DIR/Dockerfile" -t "$IMAGE" "$GIT_ROOT"
  echo "    Build complete."
fi

echo "==> Installing claude-sandbox to $INSTALL_DIR"
if [[ ! -w "$INSTALL_DIR" ]]; then
  if [[ $EUID -ne 0 ]]; then
    echo "    $INSTALL_DIR is not writable. Re-running with sudo..."
    exec sudo "$0" "$INSTALL_DIR"
  fi
fi
install -m 755 "$SCRIPT_DIR/claude-sandbox" "$INSTALL_DIR/claude-sandbox"
echo "    Done. Run 'claude-sandbox' from any worktree of this repo."
