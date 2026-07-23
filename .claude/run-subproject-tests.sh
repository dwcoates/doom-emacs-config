#!/usr/bin/env bash
# run-subproject-tests.sh — PostToolUse(Write|Edit) hook.
#
# Reads the hook payload on stdin, and when the edited file lives inside
# one of the agent-repl subprojects (agent-shim/claude-shim/, webapp/,
# daemon/), runs that subproject's test suite — and only that one. Files
# outside those trees
# exit 0 immediately, so the hook is a no-op for the rest of the repo
# (including the elisp module, which has its own ERT flow).
#
# Exit codes (PostToolUse semantics):
#   0 — not applicable, or tests passed
#   2 — tests failed; stderr is fed back to Claude to drive a fix
set -u

payload=$(cat)
file=$(printf '%s' "$payload" | jq -r '.tool_input.file_path // .tool_response.filePath // empty')
[ -z "$file" ] && exit 0

root="modules/app/agent-repl"
case "$file" in
  *"$root/agent-shim/claude-shim/"*)
    sub="shim"
    subdir="agent-shim/claude-shim"
    ;;
  *"$root/webapp/"*)
    sub="webapp"
    subdir="webapp"
    ;;
  *"$root/daemon/"*)
    sub="daemon"
    subdir="daemon"
    ;;
  *) exit 0 ;;
esac

proj="${CLAUDE_PROJECT_DIR:-$(pwd)}/$root/$subdir"
[ -d "$proj" ] || exit 0
cd "$proj" || exit 0

case "$sub" in
  shim|webapp)
    # Without installed deps there is no runnable suite here; the edit
    # itself is fine, so skip rather than block.
    [ -d node_modules ] || exit 0
    out=$(npx vitest run 2>&1)
    status=$?
    ;;
  daemon)
    command -v go >/dev/null 2>&1 || exit 0
    out=$(go test ./internal/... 2>&1)
    status=$?
    ;;
esac

if [ "$status" -ne 0 ]; then
  printf '%s test suite failed after editing %s:\n%s\n' "$sub" "$file" "$out" >&2
  exit 2
fi
exit 0
