#!/usr/bin/env bash
# install.sh — shared sandbox installation library for .agents-sandbox/.
#
# Source this file and call: install_sandbox <tool> [--force] [install-dir]
# Tools: claude, codex
#
# Example (from a thin wrapper):
#   source "$(git rev-parse --show-toplevel)/.agents-sandbox/install.sh"
#   install_sandbox claude "$@"

set -euo pipefail

AGENTS_SANDBOX_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GIT_ROOT="$(dirname "$AGENTS_SANDBOX_DIR")"

_ensure_jq() {
  command -v jq &>/dev/null && return
  echo "==> Installing jq..."
  case "$(uname -s)" in
    Darwin) brew install jq ;;
    Linux)
      if command -v apt-get &>/dev/null;   then sudo apt-get install -y jq
      elif command -v yum &>/dev/null;     then sudo yum install -y jq
      else echo "    Cannot install jq automatically. Install it manually." >&2; exit 1
      fi ;;
    *) echo "    Unsupported platform. Install jq manually." >&2; exit 1 ;;
  esac
}

_build_image_if_needed() {
  local force="$1"
  local image dockerfile_hash built_hash
  image=$("$AGENTS_SANDBOX_DIR/run.sh" --image-name)
  dockerfile_hash=$(sha256sum "$AGENTS_SANDBOX_DIR/Dockerfile" | awk '{print $1}')

  echo "==> Checking Docker image: $image"
  local needs_build=false
  if [[ "$force" == "true" ]]; then
    echo "    --force specified, rebuilding..."
    needs_build=true
  elif ! docker image inspect "$image" &>/dev/null; then
    needs_build=true
  else
    built_hash=$(docker image inspect "$image" --format '{{index .Config.Labels "dockerfile.sha256"}}' 2>/dev/null || true)
    if [[ "$built_hash" != "$dockerfile_hash" ]]; then
      echo "    Dockerfile has changed (${built_hash:0:12} → ${dockerfile_hash:0:12}), rebuilding..."
      needs_build=true
    else
      echo "    Image is up to date."
    fi
  fi

  if [[ "$needs_build" == "true" ]]; then
    echo "==> Building $image ..."
    docker build --platform linux/amd64 \
      --label "dockerfile.sha256=$dockerfile_hash" \
      -f "$AGENTS_SANDBOX_DIR/Dockerfile" -t "$image" "$GIT_ROOT"
    echo "    Build complete."
  fi
}

_update_mounts() {
  local mounts_file="$AGENTS_SANDBOX_DIR/mounts"
  local sample_file="$AGENTS_SANDBOX_DIR/mounts.sample"
  echo "==> Updating sandbox mounts"
  if [[ ! -f "$sample_file" ]]; then
    echo "    No mounts.sample found, skipping."
    return
  fi
  if [[ ! -f "$mounts_file" ]]; then
    jq '{defaults: ., custom: []}' "$sample_file" > "$mounts_file"
    echo "    Created $mounts_file"
  else
    jq --slurpfile sample "$sample_file" '.defaults = $sample[0]' "$mounts_file" \
      > "$mounts_file.tmp" && mv "$mounts_file.tmp" "$mounts_file"
    echo "    Updated defaults in $mounts_file"
  fi
}

# _install_launcher <tool> <install_dir> [orig_args...]
# orig_args are forwarded to sudo re-exec of the calling wrapper script.
_install_launcher() {
  local tool="$1" install_dir="$2"; shift 2
  local orig_args=("$@")
  echo "==> Installing ${tool}-sandbox to $install_dir"
  if [[ ! -w "$install_dir" ]] && [[ $EUID -ne 0 ]]; then
    echo "    $install_dir is not writable. Re-running with sudo..."
    exec sudo "$0" "${orig_args[@]}"
  fi
  install -m 755 "$AGENTS_SANDBOX_DIR/sandbox" "$install_dir/${tool}-sandbox"
  echo "    Done. Run '${tool}-sandbox' from any worktree of this repo."
}

install_sandbox() {
  local tool="$1"; shift
  local orig_args=("$@")
  local force=false install_dir=/usr/local/bin
  for arg in "$@"; do
    case "$arg" in
      --force) force=true ;;
      --*)     echo "Unknown option: $arg" >&2; exit 1 ;;
      *)       install_dir="$arg" ;;
    esac
  done

  _ensure_jq
  _build_image_if_needed "$force"
  _update_mounts
  _install_launcher "$tool" "$install_dir" "${orig_args[@]}"
}
