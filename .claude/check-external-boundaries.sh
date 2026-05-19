#!/usr/bin/env bash
# check-external-boundaries.sh — fail if non-wrapper production code
# invokes an external binary directly.
#
# Implements the static half of AGENTS.md "No External Processes or
# External State in Tests".  The runtime half (per-test guards) lives
# in `modules/app/claude-repl/test-helpers.el'.
#
# Rule:
#   Production code (modules/app/claude-repl/*.el, excluding test-*.el
#   and the wrapper-defining `core.el') may invoke external binaries
#   ONLY through a function listed in
#   `claude-repl--external-boundary-functions' (the registry in
#   core.el).  Any other code path that names an external CLI as an
#   argument to `call-process', `shell-command(-to-string)',
#   `start-process', `make-process', `process-file', or their
#   `*-shell-command' siblings is a violation.
#
# Detection:
#   - Subprocess primitive must appear in call position — preceded by
#     `(' or `#''.  Avoids false positives on docstring / comment
#     references to the symbol.
#   - Comment-only lines (whitespace then `;') are skipped.
#   - Binary names are matched WITHIN a sliding window of 8 lines
#     after the primitive line so multi-line subprocess forms like
#     `(make-process ... :command (list "git" ...))' are caught.
#   - Binary list: git, gh, docker, claude, osascript, pbcopy,
#     pbpaste, curl, terminal-notifier.  Add new ones here as new
#     wrappers land (and remember to extend the registry in
#     `claude-repl--external-boundary-functions').
#   - Match requires the binary name to be followed by a quote,
#     whitespace, or `/' so `"github"' / `"gitea-cli"' don't fire.
#
# Exemptions:
#   - `EXEMPT_FILES' (whole-file): core.el (the wrapper-defining file).
#   - `;; ALLOW-EXTERNAL-BOUNDARY' on EITHER the primitive line or
#     anywhere in the 8-line window — used for wrapper-definition
#     lines that legitimately invoke the external (the wrapper IS
#     the boundary).
#
# Exit code:
#   0 — clean
#   1 — violations found; offending sites printed to stderr

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
MODULE_DIR="$REPO_ROOT/modules/app/claude-repl"

# core.el holds the canonical wrapper family (`claude-repl--git-string',
# etc.) — every external invocation in that file IS a wrapper definition,
# so whole-file exempt.  Per-line ALLOW-EXTERNAL-BOUNDARY covers wrappers
# that live elsewhere.
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

# Regex pieces (extended regex syntax — used both by grep -E and awk).
PRIM_RE='(\(call-process|\(shell-command(-to-string)?|\(start-process(-shell-command)?|\(make-process|\(process-file|\(async-shell-command|\(call-process-shell-command|#'\''(call-process|shell-command(-to-string)?|start-process(-shell-command)?|make-process|process-file|async-shell-command|call-process-shell-command))'

BINARY_RE='"(git|gh|docker|claude|osascript|pbcopy|pbpaste|curl|terminal-notifier)(["[:space:]/]|$)'

WINDOW=8

violations=0
header_emitted=0

emit_header() {
  if [ "$header_emitted" -eq 0 ]; then
    echo "[check-external-boundaries] VIOLATIONS — non-wrapper production code invokes an external binary directly:" >&2
    echo "" >&2
    header_emitted=1
  fi
}

while IFS= read -r file; do
  case " ${EXEMPT_FILES[*]} " in
    *" $file "*) continue ;;
  esac

  # Find primitive lines (in call position; skip comment-only lines).
  # Output is `line_num:line_text'.
  while IFS= read -r prim_hit; do
    [ -z "$prim_hit" ] && continue
    line_num="${prim_hit%%:*}"
    line_text="${prim_hit#*:}"

    # Skip comment-only lines.
    case "$line_text" in
      *[![:space:]]*) ;;  # has non-space, fine
      *) continue ;;
    esac
    trimmed="${line_text#"${line_text%%[![:space:]]*}"}"
    case "$trimmed" in
      ';'*) continue ;;
    esac

    # Capture window: primitive line + next WINDOW lines.
    end_num=$((line_num + WINDOW))
    window=$(awk -v s="$line_num" -v e="$end_num" 'NR>=s && NR<=e' "$file")

    # If ANY line in the window carries the ALLOW tag, exempt the form.
    if printf '%s\n' "$window" | grep -q 'ALLOW-EXTERNAL-BOUNDARY'; then
      continue
    fi

    # Check the window for a binary literal.
    if printf '%s\n' "$window" | grep -qE "$BINARY_RE"; then
      emit_header
      echo "  $file:$line_num:$line_text" >&2
      violations=$((violations + 1))
    fi
  done < <(grep -nE "$PRIM_RE" "$file" 2>/dev/null || true)
done < <(printf '%s\n' "${prod_files[@]}")

if [ "$violations" -gt 0 ]; then
  echo "" >&2
  echo "[check-external-boundaries] $violations violation(s)." >&2
  echo "[check-external-boundaries] Fix: route each call through a wrapper named" >&2
  echo "    claude-repl--<resource>-* (e.g. claude-repl--git-string, claude-repl--gh-string-quiet)" >&2
  echo "[check-external-boundaries] then add the new wrapper symbol to" >&2
  echo "    \`claude-repl--external-boundary-functions' in core.el so the test-time guards see it." >&2
  echo "[check-external-boundaries] If the source line legitimately IS the wrapper definition," >&2
  echo "    append \`;; ALLOW-EXTERNAL-BOUNDARY' to that line so the lint exempts it." >&2
  echo "[check-external-boundaries] See AGENTS.md \"No External Processes or External State in Tests\"." >&2
  exit 1
fi

echo "[check-external-boundaries] OK"
exit 0
