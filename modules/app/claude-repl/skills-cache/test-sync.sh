#!/usr/bin/env bash
# test-sync.sh — tests for skills-cache/sync.sh.
#
# Each test sets up an isolated cache root with a synthetic manifest
# pointing at a synthetic impl, then runs a copy of sync.sh inside
# that root and asserts on the resulting state.  No host paths are
# touched.
#
# Run with:   bash modules/app/claude-repl/skills-cache/test-sync.sh
set -euo pipefail

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
SRC_SYNC="$THIS_DIR/sync.sh"

PASS=0
FAIL=0
pass() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); shift; if [ $# -gt 0 ]; then printf '%s\n' "$@" | sed 's/^/        /'; fi; }

# Build an isolated sync.sh root with a single-entry manifest.
mkroot() {
  local impl="$1" name="$2"
  local root; root="$(mktemp -d)"
  cp "$SRC_SYNC" "$root/sync.sh"
  chmod +x "$root/sync.sh"
  cat > "$root/manifest.sh" <<EOF
CACHED_SKILLS=("$name|$impl")
EOF
  echo "$root"
}

test_unchanged_dir() {
  local impl; impl="$(mktemp -d)"
  echo "same" > "$impl/SKILL.md"
  local root; root="$(mkroot "$impl" "foo")"
  cp -R "$impl" "$root/foo"
  local out
  if out="$(bash "$root/sync.sh" 2>&1)" \
     && grep -q "foo: unchanged" <<< "$out" \
     && grep -q "0 updated" <<< "$out"; then
    pass "unchanged dir reports 'unchanged' and 0 updated"
  else
    fail "unchanged dir" "$out"
  fi
  rm -rf "$impl" "$root"
}

test_dir_synced_when_different() {
  local impl; impl="$(mktemp -d)"
  echo "new" > "$impl/SKILL.md"
  local root; root="$(mkroot "$impl" "foo")"
  mkdir -p "$root/foo"
  echo "old" > "$root/foo/SKILL.md"
  local out; out="$(bash "$root/sync.sh" 2>&1)"
  if [ "$(cat "$root/foo/SKILL.md")" = "new" ] && grep -q "synced impl -> cache" <<< "$out"; then
    pass "differing dir is mirrored impl -> cache"
  else
    fail "differing dir mirror" "$out" "cache: $(cat "$root/foo/SKILL.md")"
  fi
  rm -rf "$impl" "$root"
}

test_dir_removes_stale_files() {
  local impl; impl="$(mktemp -d)"
  echo "kept" > "$impl/SKILL.md"
  local root; root="$(mkroot "$impl" "foo")"
  mkdir -p "$root/foo"
  echo "kept" > "$root/foo/SKILL.md"
  echo "stale" > "$root/foo/stale-file"
  bash "$root/sync.sh" >/dev/null
  if [ ! -e "$root/foo/stale-file" ]; then
    pass "stale files in cache are removed (rsync --delete)"
  else
    fail "rsync --delete behavior"
  fi
  rm -rf "$impl" "$root"
}

test_missing_impl_preserves_cache() {
  local root; root="$(mkroot "/nonexistent/path-$$" "foo")"
  mkdir -p "$root/foo"
  echo "kept" > "$root/foo/SKILL.md"
  local out; out="$(bash "$root/sync.sh" 2>&1)"
  if grep -q "impl source missing" <<< "$out" \
     && [ "$(cat "$root/foo/SKILL.md")" = "kept" ]; then
    pass "missing impl preserves existing cache"
  else
    fail "missing impl preserves cache" "$out"
  fi
  rm -rf "$root"
}

test_file_synced_when_different() {
  local impl_dir; impl_dir="$(mktemp -d)"
  local impl="$impl_dir/script.sh"
  printf 'new\n' > "$impl"
  local root; root="$(mkroot "$impl" "script.sh")"
  printf 'old\n' > "$root/script.sh"
  bash "$root/sync.sh" >/dev/null
  if [ "$(cat "$root/script.sh")" = "new" ]; then
    pass "differing file is mirrored impl -> cache"
  else
    fail "differing file mirror"
  fi
  rm -rf "$impl_dir" "$root"
}

test_cache_created_when_absent() {
  local impl; impl="$(mktemp -d)"
  echo "x" > "$impl/SKILL.md"
  local root; root="$(mkroot "$impl" "foo")"
  # Note: no $root/foo yet.
  bash "$root/sync.sh" >/dev/null
  if [ -d "$root/foo" ] && [ "$(cat "$root/foo/SKILL.md")" = "x" ]; then
    pass "absent cache dir is created from impl"
  else
    fail "absent cache dir creation"
  fi
  rm -rf "$impl" "$root"
}

echo "=== test-sync.sh ==="
test_unchanged_dir
test_dir_synced_when_different
test_dir_removes_stale_files
test_missing_impl_preserves_cache
test_file_synced_when_different
test_cache_created_when_absent

echo
echo "Passed: $PASS  Failed: $FAIL"
exit $((FAIL > 0))
