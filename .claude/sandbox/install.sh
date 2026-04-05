#!/usr/bin/env bash
# install.sh — set up the Docker sandbox for this repo.
#
# - Builds the Docker image if Dockerfile has changed (or image is missing).
# - Installs claude-sandbox to INSTALL_DIR (default: /usr/local/bin).
#
# Usage: bash .claude/sandbox/install.sh [install-dir]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GIT_ROOT="$(dirname "$(dirname "$SCRIPT_DIR")")"
FORCE=false
INSTALL_DIR=/usr/local/bin
for arg in "$@"; do
  case "$arg" in
    --force) FORCE=true ;;
    --*)     echo "Unknown option: $arg" >&2; exit 1 ;;
    *)       INSTALL_DIR="$arg" ;;
  esac
done

if ! command -v jq &>/dev/null; then
  echo "==> Installing jq..."
  case "$(uname -s)" in
    Darwin) brew install jq ;;
    Linux)
      if command -v apt-get &>/dev/null; then
        sudo apt-get install -y jq
      elif command -v yum &>/dev/null; then
        sudo yum install -y jq
      else
        echo "    Cannot install jq: no known package manager found. Install it manually." >&2
        exit 1
      fi
      ;;
    *) echo "    Unsupported platform for auto-install. Install jq manually." >&2; exit 1 ;;
  esac
fi

IMAGE=$(tr -d '[:space:]' < "$SCRIPT_DIR/image")
DOCKERFILE_HASH=$(sha256sum "$SCRIPT_DIR/Dockerfile" | awk '{print $1}')

echo "==> Checking Docker image: $IMAGE"
needs_build=false
if [[ "$FORCE" == "true" ]]; then
  echo "    --force specified, rebuilding..."
  needs_build=true
elif ! docker image inspect "$IMAGE" &>/dev/null; then
  needs_build=true
else
  BUILT_HASH=$(docker image inspect "$IMAGE" --format '{{index .Config.Labels "dockerfile.sha256"}}' 2>/dev/null || true)
  if [[ "$BUILT_HASH" != "$DOCKERFILE_HASH" ]]; then
    echo "    Dockerfile has changed (${BUILT_HASH:0:12} → ${DOCKERFILE_HASH:0:12}), rebuilding..."
    needs_build=true
  else
    echo "    Image is up to date."
  fi
fi

if [[ "$needs_build" == "true" ]]; then
  echo "==> Building $IMAGE ..."
  docker build --platform linux/amd64 \
    --label "dockerfile.sha256=$DOCKERFILE_HASH" \
    -f "$SCRIPT_DIR/Dockerfile" -t "$IMAGE" "$GIT_ROOT"
  echo "    Build complete."
fi

echo "==> Updating sandbox mounts"
MOUNTS_FILE="$SCRIPT_DIR/mounts"
SAMPLE_FILE="$SCRIPT_DIR/mounts.sample"
if [[ -f "$SAMPLE_FILE" ]]; then
  if [[ ! -f "$MOUNTS_FILE" ]]; then
    jq '{defaults: ., custom: []}' "$SAMPLE_FILE" > "$MOUNTS_FILE"
    echo "    Created $MOUNTS_FILE"
  else
    jq --slurpfile sample "$SAMPLE_FILE" '.defaults = $sample[0]' "$MOUNTS_FILE" \
      > "$MOUNTS_FILE.tmp" && mv "$MOUNTS_FILE.tmp" "$MOUNTS_FILE"
    echo "    Updated defaults in $MOUNTS_FILE"
  fi
else
  echo "    No mounts.sample found, skipping."
fi

echo "==> Installing claude-sandbox to $INSTALL_DIR"
if [[ ! -w "$INSTALL_DIR" ]]; then
  if [[ $EUID -ne 0 ]]; then
    echo "    $INSTALL_DIR is not writable. Re-running with sudo..."
    exec sudo "$0" "$@"
  fi
fi
install -m 755 "$SCRIPT_DIR/claude-sandbox" "$INSTALL_DIR/claude-sandbox"
echo "    Done. Run 'claude-sandbox' from any worktree of this repo."
