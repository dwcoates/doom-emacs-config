# Test Coverage Analysis: session.el

## Summary

- **Total functions in session.el:** 37
- **Functions with direct test coverage:** 30
- **Functions without direct coverage:** 7
- **Coverage by function count:** 81%

## Coverage Status by Function

### Sandbox Configuration

| Function | Covered | Tests |
|----------|---------|-------|
| `claude-repl--docker-image-exists-p` | Yes | true/false cases for docker inspect exit code |
| `claude-repl--find-sandbox-script` | Yes | prefers PATH, falls back to repo, returns nil |
| `claude-repl--query-sandbox-image` | Yes | trimmed output, empty output returns nil |
| `claude-repl--find-install-script` | Yes | exists, missing |
| `claude-repl--resolve-sandbox-config` | Yes | no launcher, image ready, needs build, empty image |
| `claude-repl--prompt-sandbox-build` | **No** | see edge cases below |
| `claude-repl--get-sandbox-image` | **No** | see edge cases below |

### Command Building

| Function | Covered | Tests |
|----------|---------|-------|
| `claude-repl--compute-claude-flags` | Yes | resume, fork, perm flag, all nil, combined resume+perm |
| `claude-repl--compute-perm-flag` | Yes | sandboxed, ChessCom, personal, nil dir |
| `claude-repl--assemble-cmd` | Yes | sandboxed, bare metal, no flags |
| `claude-repl--build-start-cmd` | **No** | see edge cases below |

### Session Startup

| Function | Covered | Tests |
|----------|---------|-------|
| `claude-repl--sandbox-mode-line` | Yes | sandboxed, bare metal |
| `claude-repl--log-session-start` | **No** | pure logging, low value |

(Session startup was merged into `claude-repl--initialize-claude` in panels.el; see
`test-coverage-panels.md`.)

### Loading Placeholder

| Function | Covered | Tests |
|----------|---------|-------|
| `claude-repl--swap-placeholder-into-windows` | Yes | live buf, dead buf |
| `claude-repl--swap-placeholder` | Yes | indirectly via handle-first-ready idempotent test |

### Session Completion Handling

| Function | Covered | Tests |
|----------|---------|-------|
| `claude-repl--maybe-notify-finished` | Yes | debounce, skips when focused, first call, stores time |
| `claude-repl--mark-done-if-hidden` | Yes | not visible (sets done), visible (no-op) |
| `claude-repl--refresh-vterm-after-finish` | Yes | live buffer, dead buffer |
| `claude-repl--handle-claude-finished` | Yes | hidden/visible/nil-vterm, notifies other ws, no msg for current ws |

### Session ID Management

| Function | Covered | Tests |
|----------|---------|-------|
| `claude-repl--set-session-id` | **No** | trivial setter, tested indirectly via capture-session-id |
| `claude-repl--session-file-matches-p` | Yes | host path, container path, no match, missing cwd, missing sessionId, JSON error |
| `claude-repl--find-session-id-in-dir` | Yes | no sessions dir, matches, no match |
| `claude-repl--capture-session-id` | Yes | no project-dir, found, not-found-clears |

### Readiness and Pending Prompts

| Function | Covered | Tests |
|----------|---------|-------|
| `claude-repl--deliver-pending-prompts` | Yes | live buf, dead buf |
| `claude-repl--drain-pending-prompts` | Yes | empty, sends |
| `claude-repl--show-panels-or-defer` | Yes | current ws, other ws |
| `claude-repl--finalize-ready-state` | Yes | cancels timer + captures session |
| `claude-repl--open-panels-after-ready` | Yes | with pending, no pending current, no pending other |
| `claude-repl--handle-first-ready` | Yes | idempotent, with owning ws, without owning ws |
| `claude-repl--on-vterm-title-set` | Yes | non-claude buffer, claude buffer |

### Process State Predicates

| Function | Covered | Tests |
|----------|---------|-------|
| `claude-repl--vterm-process-alive-p` | Yes | no buffer, dead buffer, no process |
| `claude-repl--claude-running-p` | **No** | trivial wrapper around vterm-process-alive-p |
| `claude-repl--session-starting-p` | Yes | not running, ready, starting |

### Readiness Timer

| Function | Covered | Tests |
|----------|---------|-------|
| `claude-repl--cancel-ready-timer` | Yes | no timer, with timer |
| `claude-repl--ready-timer-tick` | Yes | timeout, still starting, ready current ws, ready other ws |
| `claude-repl--schedule-ready-timer` | Yes | cancels existing and schedules new |

### Workspace Environment Initialization

| Function | Covered | Tests |
|----------|---------|-------|
| `claude-repl--ensure-ws-env` | Yes | initializes once, preserves existing |

## Untested Functions -- Edge Case Analysis

### `claude-repl--prompt-sandbox-build`

Interactive function that calls `y-or-n-p` and signals `user-error`.

**Edge cases:**
1. User answers yes to build prompt -- should call `compile` with install script, then signal `user-error`
2. User answers no to build prompt -- should fall through (no explicit handling of "no")
3. No install-script in config -- should signal `user-error` with manual instruction
4. Empty image string in config

**Why not tested:** Signals `user-error` unconditionally, making standard ERT assertions awkward. Could test with `should-error` but the `y-or-n-p` interaction also needs stubbing.

**Recommendation:** Add tests using `cl-letf` to stub `y-or-n-p`, `compile`, and `should-error` to catch the `user-error`.

### `claude-repl--get-sandbox-image`

Orchestrator that combines workspace state lookup with `resolve-sandbox-config` and `prompt-sandbox-build`.

**Edge cases:**
1. Non-worktree workspace -- should return nil without calling resolve
2. Worktree with :active-env not :sandbox -- should return nil
3. Worktree + sandbox + image exists -- should return config plist
4. Worktree + sandbox + needs-build -- should call prompt-sandbox-build
5. :project-dir is nil

**Why not tested:** Depends on workspace state plumbing and calls prompt-sandbox-build (which signals user-error). Testable with appropriate stubs.

**Recommendation:** Add tests with stubbed workspace state and resolve-sandbox-config.

### `claude-repl--build-start-cmd`

Integration function assembling all parts of the start command.

**Edge cases:**
1. Bare-metal workspace with no session -- should produce `claude --dangerously-skip-permissions`
2. Worktree sandbox with session -- should use sandbox script + --resume
3. Fork session ID set -- should include --fork-session and clear fork-session-id
4. Sandbox config needs-build -- docker-image should be nil, sandboxed-p false
5. Nil project-dir
6. Empty session-id on instantiation

**Why not tested:** Heavy orchestration depending on get-sandbox-image (which may signal user-error) and multiple workspace state lookups. Each sub-function is individually tested.

**Recommendation:** Add integration-level tests with comprehensive stubs.

### `claude-repl--log-session-start`

Pure logging function.

**Edge cases:**
1. All fields present in start-info
2. Nil values for optional fields (session-id, fork-session-id)
3. worktree-p true vs false formatting

**Why not tested:** Pure side-effect (message + claude-repl--log). Low risk.

**Recommendation:** Low priority. Could verify message format if desired.

### `claude-repl--set-session-id`

Trivial setter: `(setf (claude-repl-instantiation-session-id (claude-repl--active-inst ws)) id)`.

**Why not tested:** One-liner, fully exercised indirectly by capture-session-id tests.

**Recommendation:** No dedicated test needed.

### `claude-repl--claude-running-p`

Trivial wrapper: `(claude-repl--vterm-process-alive-p (or ws (+workspace-current-name)))`.

**Edge cases:**
1. Explicit ws argument
2. Nil ws (falls back to +workspace-current-name)

**Why not tested:** One-liner delegating to already-tested function.

**Recommendation:** Low priority. Could add a test for the nil-ws default behavior.

## Edge Cases Covered by Existing Tests

### `claude-repl--session-file-matches-p` (6 tests)
- Host path match
- Container path match (Docker scenario)
- No path match
- Missing `cwd` field in JSON
- Missing `sessionId` field in JSON
- JSON parse error (file corrupt or missing)

### `claude-repl--handle-claude-finished` (5 tests)
- Hidden vterm buffer -> sets :done
- Visible vterm buffer -> no :done
- Nil vterm buffer -> no :done
- Non-current workspace -> prints message
- Current workspace -> no message

### `claude-repl--maybe-notify-finished` (4 tests)
- Debounce within 2s window
- Skips when frame focused
- First call with no prior notify time
- Stores :last-notify-time after notifying

### `claude-repl--handle-first-ready` (3 tests)
- Idempotent (second call is no-op)
- With owning workspace (finalizes + opens panels)
- Without owning workspace (skips finalize/panels)

### `claude-repl--ready-timer-tick` (4 tests)
- Timeout after 30s
- Still starting (no-op)
- Ready + current workspace (opens panels)
- Ready + other workspace (no panels)

### `claude-repl--compute-claude-flags` (6 tests)
- Resume only
- Fork only (takes precedence)
- Fork ignores session-id
- Perm flag only
- All nil
- Resume + perm flag combined

## Recommended Priority for Remaining Coverage

1. **High:** `claude-repl--get-sandbox-image` -- orchestration logic with branching
2. **High:** `claude-repl--build-start-cmd` -- integration point for all command building
3. **Medium:** `claude-repl--prompt-sandbox-build` -- user-facing error paths
4. **Low:** `claude-repl--log-session-start` -- pure logging
5. **Low:** `claude-repl--claude-running-p` -- trivial wrapper
6. **Low:** `claude-repl--set-session-id` -- trivial setter
