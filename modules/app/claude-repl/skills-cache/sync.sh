#!/usr/bin/env bash
# sync.sh — mirror each cached workspace-* skill's impl source into the
# in-repo cache so the persisted copy stays current.
#
# Driven by manifest.sh.  For each entry:
#   - If the impl source exists on the host, diff it against the cache.
#     Different → copy impl into the cache (rsync --delete for dirs,
#     cp for files).  Same → no-op.
#   - If the impl source is missing (e.g. running on a machine without
#     the host worktrees), leave the cache as-is.  The persisted cache
#     remains the install fallback.
#
# Exit codes:
#   0  always on success — both "no changes" and "changes synced" are
#      success.  Pre-commit hook checks `git diff` itself.
#   2  hard error (manifest parse, missing tools, etc.)
#
# Usage:
#   bash modules/app/claude-repl/skills-cache/sync.sh
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CACHE_DIR="$SCRIPT_DIR"

if [ ! -f "$CACHE_DIR/manifest.sh" ]; then
  echo "[sync] ERROR: manifest.sh not found at $CACHE_DIR" >&2
  exit 2
fi
# shellcheck source=./manifest.sh
source "$CACHE_DIR/manifest.sh"

if ! command -v rsync >/dev/null 2>&1; then
  echo "[sync] ERROR: rsync is required but not found in PATH" >&2
  exit 2
fi

CHANGED=0
UNCHANGED=0
MISSING=0

for entry in "${CACHED_SKILLS[@]}"; do
  IFS='|' read -r name impl <<< "$entry"
  cache="$CACHE_DIR/$name"

  if [ ! -e "$impl" ]; then
    echo "[sync] $name: impl source missing ($impl) — cache preserved"
    MISSING=$((MISSING + 1))
    continue
  fi

  if [ -d "$impl" ]; then
    if [ -d "$cache" ] && diff -qr "$impl/" "$cache/" >/dev/null 2>&1; then
      echo "[sync] $name: unchanged"
      UNCHANGED=$((UNCHANGED + 1))
      continue
    fi
    mkdir -p "$cache"
    # -c forces content-checksum comparison; without it rsync skips
    # files with matching size+mtime even when content differs (real
    # case: two files created seconds apart with same byte count).
    rsync -ac --delete "$impl/" "$cache/"
    echo "[sync] $name: synced impl -> cache"
    CHANGED=$((CHANGED + 1))
  elif [ -f "$impl" ]; then
    if [ -f "$cache" ] && cmp -s "$impl" "$cache"; then
      echo "[sync] $name: unchanged"
      UNCHANGED=$((UNCHANGED + 1))
      continue
    fi
    mkdir -p "$(dirname "$cache")"
    cp -p "$impl" "$cache"
    echo "[sync] $name: synced impl -> cache"
    CHANGED=$((CHANGED + 1))
  else
    echo "[sync] WARNING: $name: impl is neither file nor directory ($impl)" >&2
  fi
done

echo "[sync] Summary: ${CHANGED} updated, ${UNCHANGED} unchanged, ${MISSING} missing-impl."
