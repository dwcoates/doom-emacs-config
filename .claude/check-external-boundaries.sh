#!/usr/bin/env bash
# check-external-boundaries.sh — fail if non-wrapper production code
# invokes an external binary directly.
#
# Implements the static half of AGENTS.md "No External Processes or
# External State in Tests".  The runtime half (per-test guards) lives
# in `modules/app/claude-repl/test-helpers.el'.
#
# Rule:
#   Production code (modules/app/claude-repl/*.el, excluding test-*.el)
#   may invoke external binaries ONLY through a function listed in
#   `claude-repl--external-boundary-functions' (the registry in
#   core.el).  Any other code path that names `git', `gh', or other
#   external CLIs as the first argument to `call-process',
#   `shell-command-to-string', `start-process', `make-process', or
#   `process-file' is a violation.
#
# Exit code:
#   0 — clean (or only ALLOW-listed lines remain)
#   1 — violations found; offending lines printed to stderr
#
# Wrappers (the registry's defining file) are exempted by path-prefix.

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
MODULE_DIR="$REPO_ROOT/modules/app/claude-repl"

# Wrapper definitions live in core.el (the registry's home file).
# Every external invocation inside core.el IS the wrapper itself,
# so the whole file is exempt from the lint.  Test files are also
# exempt — tests are governed by the runtime guards instead.
EXEMPT_FILES=(
  "$MODULE_DIR/core.el"
)

# Production files = *.el under $MODULE_DIR excluding test-*.el and
# any path in EXEMPT_FILES.
mapfile -t prod_files < <(
  find "$MODULE_DIR" -maxdepth 1 -name "*.el" \
    ! -name "test-*.el" \
    ! -name "test-helpers.el" \
    -print
)

violations=0
while IFS= read -r file; do
  case " ${EXEMPT_FILES[*]} " in
    *" $file "*) continue ;;
  esac

  # Patterns that name an external binary as the first argument to a
  # subprocess primitive.  Limited to the binaries the project actually
  # shells out to today (`git`, `gh`); add new ones here as new
  # external wrappers are introduced (and remember to also extend the
  # registry in `claude-repl--external-boundary-functions').
  # Skip lines tagged with ;; ALLOW-EXTERNAL-BOUNDARY — used on the
  # source line of a wrapper definition (e.g. inside the body of
  # `claude-repl--git-exit-code') so the wrapper itself is not flagged
  # by its own lint.
  hits="$(
    grep -nE '(call-process|shell-command|shell-command-to-string|start-process|make-process|process-file|async-shell-command)[^)]*"(git|gh)"' \
      "$file" 2>/dev/null \
      | grep -vE 'ALLOW-EXTERNAL-BOUNDARY' \
      || true
  )"
  if [ -n "$hits" ]; then
    if [ "$violations" -eq 0 ]; then
      echo "[check-external-boundaries] VIOLATIONS — non-wrapper production code names an external binary directly:" >&2
      echo "" >&2
    fi
    while IFS= read -r line; do
      echo "  $file:$line" >&2
      violations=$((violations + 1))
    done <<<"$hits"
  fi
done < <(printf '%s\n' "${prod_files[@]}")

if [ "$violations" -gt 0 ]; then
  echo "" >&2
  echo "[check-external-boundaries] $violations violation(s)." >&2
  echo "[check-external-boundaries] Fix: route each call through a wrapper named" >&2
  echo "    claude-repl--<resource>-* (e.g. claude-repl--git-string, claude-repl--gh-string-quiet)" >&2
  echo "[check-external-boundaries] then add the new wrapper symbol to" >&2
  echo "    \`claude-repl--external-boundary-functions' in core.el so the test-time guards see it." >&2
  echo "[check-external-boundaries] See AGENTS.md \"No External Processes or External State in Tests\"." >&2
  exit 1
fi

echo "[check-external-boundaries] OK"
exit 0
