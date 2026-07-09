#!/usr/bin/env bash
# test-mounts.sh — validate the mounts.sample schema.
#
# Checks that mounts.sample is valid JSON with the expected structure
# (matching the format consumed by the sandbox harness).
#
# Run with:   bash .agents-sandbox/test-mounts.sh
set -euo pipefail

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
SAMPLE="$THIS_DIR/mounts.sample"

PASS=0
FAIL=0
pass() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); shift; if [ $# -gt 0 ]; then printf '%s\n' "$@" | sed 's/^/        /'; fi; }

# --- mounts.sample exists ---
test_sample_exists() {
  if [ -f "$SAMPLE" ]; then
    pass "mounts.sample exists"
  else
    fail "mounts.sample exists" "file not found: $SAMPLE"
  fi
}

# --- mounts.sample is valid JSON ---
test_sample_is_valid_json() {
  if python3 -c "import json, sys; json.load(open(sys.argv[1]))" "$SAMPLE" 2>/dev/null; then
    pass "mounts.sample is valid JSON"
  else
    fail "mounts.sample is valid JSON" "$(python3 -c "import json, sys; json.load(open(sys.argv[1]))" "$SAMPLE" 2>&1)"
  fi
}

# --- top-level has 'defaults' and 'custom' keys ---
test_sample_has_required_keys() {
  local keys
  keys="$(python3 -c "
import json, sys
d = json.load(open(sys.argv[1]))
print(' '.join(sorted(d.keys())))
" "$SAMPLE" 2>/dev/null)"
  if [ "$keys" = "custom defaults" ]; then
    pass "mounts.sample has 'defaults' and 'custom' keys"
  else
    fail "mounts.sample has 'defaults' and 'custom' keys" "got keys: $keys"
  fi
}

# --- 'defaults' and 'custom' are arrays ---
test_sample_values_are_arrays() {
  local result
  result="$(python3 -c "
import json, sys
d = json.load(open(sys.argv[1]))
ok = isinstance(d['defaults'], list) and isinstance(d['custom'], list)
print('ok' if ok else 'fail')
" "$SAMPLE" 2>/dev/null)"
  if [ "$result" = "ok" ]; then
    pass "'defaults' and 'custom' are arrays"
  else
    fail "'defaults' and 'custom' are arrays"
  fi
}

# --- each mount entry has required fields ---
test_mount_entries_have_required_fields() {
  local result
  result="$(python3 -c "
import json, sys
d = json.load(open(sys.argv[1]))
required = {'source', 'target', 'type', 'readonly'}
errors = []
for section in ('defaults', 'custom'):
    for i, entry in enumerate(d[section]):
        missing = required - set(entry.keys())
        if missing:
            errors.append(f'{section}[{i}] missing: {missing}')
if errors:
    print('\n'.join(errors))
else:
    print('ok')
" "$SAMPLE" 2>/dev/null)"
  if [ "$result" = "ok" ]; then
    pass "mount entries have required fields (source, target, type, readonly)"
  else
    fail "mount entries have required fields" "$result"
  fi
}

# --- home mount is present in defaults ---
test_workspace_mount_present() {
  local result
  result="$(python3 -c "
import json, sys
d = json.load(open(sys.argv[1]))
found = any(
    e.get('source') == '~' and e.get('target') == '/home/claude/workspace'
    for e in d['defaults']
)
print('ok' if found else 'fail')
" "$SAMPLE" 2>/dev/null)"
  if [ "$result" = "ok" ]; then
    pass "~ mount is present in defaults"
  else
    fail "~ mount is present in defaults"
  fi
}

# --- home mount is read+write ---
test_workspace_mount_is_rw() {
  local result
  result="$(python3 -c "
import json, sys
d = json.load(open(sys.argv[1]))
for e in d['defaults']:
    if e.get('source') == '~':
        print('ok' if e.get('readonly') is False else 'fail')
        sys.exit(0)
print('not-found')
" "$SAMPLE" 2>/dev/null)"
  if [ "$result" = "ok" ]; then
    pass "home mount is read+write (readonly: false)"
  else
    fail "home mount is read+write" "readonly is not false"
  fi
}

# --- mount type is 'bind' ---
test_workspace_mount_type_is_bind() {
  local result
  result="$(python3 -c "
import json, sys
d = json.load(open(sys.argv[1]))
for e in d['defaults']:
    if e.get('source') == '~':
        print('ok' if e.get('type') == 'bind' else 'fail')
        sys.exit(0)
print('not-found')
" "$SAMPLE" 2>/dev/null)"
  if [ "$result" = "ok" ]; then
    pass "home mount type is 'bind'"
  else
    fail "home mount type" "expected 'bind'"
  fi
}

echo "=== test-mounts.sh ==="
test_sample_exists
test_sample_is_valid_json
test_sample_has_required_keys
test_sample_values_are_arrays
test_mount_entries_have_required_fields
test_workspace_mount_present
test_workspace_mount_is_rw
test_workspace_mount_type_is_bind

echo
echo "Passed: $PASS  Failed: $FAIL"
exit $((FAIL > 0))
