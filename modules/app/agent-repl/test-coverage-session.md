# Test Coverage Analysis: session.el

## Summary

This is a historical snapshot: it predates the removal of the Docker
sandbox surface (`get-sandbox-image`, `resolve-sandbox-config`,
`prompt-sandbox-build`, and friends), whose rows have been dropped along
with the functions themselves. The per-function counts it once carried
no longer describe the file and have been removed rather than guessed
at; re-derive them before relying on a headline number.

## Coverage Status by Function

### Command Building

| Function | Covered | Tests |
|----------|---------|-------|
| `agent-repl--compute-claude-flags` | Yes | resume, fork, perm flag, all nil, combined resume+perm, system-prompt default period, system-prompt nil, system-prompt shell-quoting, system-prompt combined |
| `agent-repl--compute-perm-flag` | Yes | ChessCom, personal, nil dir |
| `agent-repl--assemble-cmd` | Yes | bare metal, no flags |
| `agent-repl--build-start-cmd` | **No** | see edge cases below |

### Session Startup

| Function | Covered | Tests |
|----------|---------|-------|
| `agent-repl--log-session-start` | **No** | pure logging, low value |

(Session startup was merged into `agent-repl--initialize-claude` in panels.el; see
`test-coverage-panels.md`.)

### Session Completion Handling

| Function | Covered | Tests |
|----------|---------|-------|
| `agent-repl--maybe-notify-finished` | Yes | debounce, skips when focused, first call, stores time |
| `agent-repl--mark-done-if-hidden` | Yes | not visible (sets done), visible (no-op) |
| `agent-repl--refresh-vterm-after-finish` | Yes | live buffer, dead buffer |
| `agent-repl--handle-claude-finished` | Yes | hidden/visible/nil-vterm, notifies other ws, no msg for current ws |

### Session ID Management

| Function | Covered | Tests |
|----------|---------|-------|
| `agent-repl--set-session-id` | **No** | trivial setter, tested indirectly via capture-session-id |
| `agent-repl--session-file-matches-p` | Yes | host path, container path, no match, missing cwd, missing sessionId, JSON error |
| `agent-repl--find-session-id-in-dir` | Yes | no sessions dir, matches, no match |
| `agent-repl--capture-session-id` | Yes | no project-dir, found, not-found-clears |

### Readiness and Pending Prompts

| Function | Covered | Tests |
|----------|---------|-------|
| `agent-repl--finalize-ready-state` | Yes | cancels timer + captures session |
| `agent-repl--handle-first-ready` | Yes | idempotent, with owning ws, without owning ws |
| `agent-repl--on-vterm-title-set` | Yes | non-claude buffer, claude buffer |

### Process State Predicates

| Function | Covered | Tests |
|----------|---------|-------|
| `agent-repl--vterm-process-alive-p` | Yes | no buffer, dead buffer, no process |
| `agent-repl--claude-running-p` | **No** | trivial wrapper around vterm-process-alive-p |
| `agent-repl--session-starting-p` | Yes | not running, ready, starting |

### Readiness Timer

| Function | Covered | Tests |
|----------|---------|-------|
| `agent-repl--cancel-ready-timer` | Yes | no timer, with timer |
| `agent-repl--ready-timer-tick` | Yes | timeout, still starting, ready current ws, ready other ws |
| `agent-repl--schedule-ready-timer` | Yes | cancels existing and schedules new |

### Workspace Environment Initialization

| Function | Covered | Tests |
|----------|---------|-------|
| `agent-repl--ensure-ws-env` | Yes | initializes once, preserves existing |

## Untested Functions -- Edge Case Analysis

### `agent-repl--build-start-cmd`

Integration function assembling all parts of the start command.

**Edge cases:**
1. Bare-metal workspace with no session -- should produce `claude --permission-mode auto`
2. Fork session ID set -- should include --fork-session and clear fork-session-id
3. Nil project-dir
4. Empty session-id on instantiation

**Why not tested:** Heavy orchestration across multiple workspace state lookups. Each sub-function is individually tested.

**Recommendation:** Add integration-level tests with comprehensive stubs.

### `agent-repl--log-session-start`

Pure logging function.

**Edge cases:**
1. All fields present in start-info
2. Nil values for optional fields (session-id, fork-session-id)
3. worktree-p true vs false formatting

**Why not tested:** Pure side-effect (message + agent-repl--log). Low risk.

**Recommendation:** Low priority. Could verify message format if desired.

### `agent-repl--set-session-id`

Trivial setter: `(setf (agent-repl-instantiation-session-id (agent-repl--active-inst ws)) id)`.

**Why not tested:** One-liner, fully exercised indirectly by capture-session-id tests.

**Recommendation:** No dedicated test needed.

### `agent-repl--claude-running-p`

Trivial wrapper: `(agent-repl--vterm-process-alive-p (or ws (+workspace-current-name)))`.

**Edge cases:**
1. Explicit ws argument
2. Nil ws (falls back to +workspace-current-name)

**Why not tested:** One-liner delegating to already-tested function.

**Recommendation:** Low priority. Could add a test for the nil-ws default behavior.

## Edge Cases Covered by Existing Tests

### `agent-repl--session-file-matches-p` (6 tests)
- Host path match
- Container path match (Docker scenario)
- No path match
- Missing `cwd` field in JSON
- Missing `sessionId` field in JSON
- JSON parse error (file corrupt or missing)

### `agent-repl--handle-claude-finished` (5 tests)
- Hidden vterm buffer -> sets :done
- Visible vterm buffer -> no :done
- Nil vterm buffer -> no :done
- Non-current workspace -> prints message
- Current workspace -> no message

### `agent-repl--maybe-notify-finished` (4 tests)
- Debounce within 2s window
- Skips when frame focused
- First call with no prior notify time
- Stores :last-notify-time after notifying

### `agent-repl--handle-first-ready` (3 tests)
- Idempotent (second call is no-op)
- With owning workspace (finalizes + opens panels)
- Without owning workspace (skips finalize/panels)

### `agent-repl--ready-timer-tick` (4 tests)
- Timeout after 30s
- Still starting (no-op)
- Ready + current workspace (opens panels)
- Ready + other workspace (no panels)

### `agent-repl--compute-claude-flags` (10 tests)
- Resume only
- Fork only (takes precedence)
- Fork ignores session-id
- Perm flag only
- All nil
- Resume + perm flag combined
- System-prompt default period emits `--system-prompt "."` (literal double quotes)
- System-prompt nil omits the flag entirely
- System-prompt with spaces wrapped in literal double quotes
- System-prompt with embedded `"` backslash-escapes
- System-prompt with `$` backslash-escapes to prevent expansion
- System-prompt combines with `--continue` and perm flag

## Recommended Priority for Remaining Coverage

1. **High:** `agent-repl--build-start-cmd` -- integration point for all command building
2. **Low:** `agent-repl--log-session-start` -- pure logging
3. **Low:** `agent-repl--claude-running-p` -- trivial wrapper
4. **Low:** `agent-repl--set-session-id` -- trivial setter
