# Fallback and Default Logic Report: `session.el`

**Total instances: 57**

---

## Section 1: Variable Defaults (2 instances)

| # | Line | Variable | Default | Consequence |
|---|---|---|---|---|
| 1 | 7 | `claude-repl--ready` (local) | `nil` | System assumes Claude not ready until told otherwise |
| 2 | 12-17 | `claude-repl-docker-image` | `""` | Empty = no global fallback image; bare metal when no per-repo image |

---

## Section 2: Sandbox Resolution Chain (7 instances)

### Instance 3: `claude-repl--find-sandbox-script`
- **Line(s):** 29-37
- **Two-tier `or`:** PATH executable -> repo-local `.agents-sandbox/sandbox` -> nil

### Instance 9: `claude-repl--resolve-sandbox-config`
- **Line(s):** 78-89
- **Three-way `if-let`:** Image found + exists -> image found + needs-build -> no image (nil)

### Instance 10: Docker image existence check
- **Line(s):** 79-87
- **Falls back TO:** `:needs-build t` plist triggering build prompt

### Instance 13: `claude-repl--prompt-sandbox-build`
- **Line(s):** 114-118
- **Falls back TO:** Manual instruction `user-error` when no install script found

---

## Section 3: Environment and Permission Defaults

### Instance 11: Default environment
- **Line(s):** 99-105
- **Code:** `(claude-repl--ws-put ws :active-env :bare-metal)`
- **Consequence:** Every new workspace defaults to `:bare-metal`, not `:sandbox`.

### Instance 20: Permission flag heuristic
- **Line(s):** 168-170
- **Code:** `(if (string-match-p "ChessCom" ...) "--permission-mode auto" "--dangerously-skip-permissions")`
- **Consequence:** Work repos get `auto`, all others (personal) get skip-permissions.

### Instance 18: `(or project-dir default-directory)` (appears twice)
- **Lines:** 168, 172
- **Risk:** Wrong directory means wrong permission mode.

---

## Section 4: Command Assembly

### Instance 15: `delq nil` pattern
- **Line(s):** 147-154
- Each flag conditionally nil, stripped by `delq nil`.

### Instance 21: Sandbox vs bare-metal command
- **Line(s):** 182-184
- **Falls back TO:** `"claude "` hardcoded binary name

---

## Section 5: Ready Timer (Fallback Polling System)

**Lines 450-487 -- Explicitly labeled "fallback polling"**

| # | Line | What | Default |
|---|---|---|---|
| 38 | 468-476 | Three-branch `cond` | `t` catch-all = "Claude is ready" |
| 39 | 469 | Timeout | Hardcoded `30.0` seconds |
| 40 | 484-485 | Poll interval | Hardcoded `0.5` seconds |
| 41 | 450-487 | Entire subsystem | Fallback for when hook-based readiness detection fails |

---

## Section 6: Session Lifecycle Guards

| # | Line | Guard | Falls back TO |
|---|---|---|---|
| 26 | 289-292 | `when placeholder` | No-op if loading buffer gone |
| 27 | 274 | `buffer-live-p` in swap | No-op if vterm dead |
| 30 | 323-329 | `buffer-live-p` in refresh | Log (no refresh) |
| 31 | 340-341 | `when vterm-buf` | Skip refresh |
| 43 | 365-368 | `buffer-live-p` in deliver | **Pending prompts silently dropped** |

---

## Section 7: Notification Debounce

| # | Line | What | Default |
|---|---|---|---|
| 28 | 304 | Null last-notify check | nil = "never notified" (allow) |
| 29 | 303-310 | Frame focus check | Only notify when unfocused |
| 55 | 304, 309 | Debounce threshold | Hardcoded `2.0` seconds |
| 56 | 307 | Schedule delay | Hardcoded `0.1` seconds |

---

## Section 8: `&optional ws` with `or` Fallback (2 instances)

| # | Line | Function |
|---|---|---|
| 33 | 435-438 | `claude-repl--claude-running-p` |
| 34 | 440-448 | `claude-repl--session-starting-p` |

Both: `(or ws (+workspace-current-name))` -- could produce incorrect results if called from wrong workspace context.

---

## Section 9: Logging Ternaries (6 instances)

Lines 22, 132, 133, 232, 287, 433, 447 -- `(if x "yes" "no")` or `(if buf (buffer-name buf) "nil")` patterns.

---

## Most Consequential

1. **Instance 11** (default `:bare-metal`) -- determines execution environment for every new workspace
2. **Instance 18** (`or project-dir default-directory`) -- wrong permission flags
3. **Instance 20** (ChessCom heuristic) -- determines whether permissions are skipped entirely
4. **Instance 33-34** (`or ws (+workspace-current-name)`) -- wrong workspace if called from unexpected context
5. **Instance 41** (entire ready-timer) -- the polling mechanism that catches hook failures
6. **Instance 43** (silently dropping pending prompts) -- potential data loss
