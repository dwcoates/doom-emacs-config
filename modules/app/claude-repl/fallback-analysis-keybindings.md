# Fallback and Default Logic Report: `keybindings.el`

**Total instances: 47**

---

## Section 1: Hardcoded Constants (3 instances)

| # | Line | Value | Purpose |
|---|---|---|---|
| 1 | 5 | `~/.claude/output/` | IPC output directory, no override mechanism |
| 11 | 137 | `"DWC/mock-priority-test"` | Debug default branch name |
| 44 | 138 | `format-time-string "%s"` | Timestamp-based unique filenames |

---

## Section 2: `or` Fallback Expressions (8 instances)

| # | Line | Code | Falls back TO |
|---|---|---|---|
| 2 | 14 | `(or (cdr pair) "nil")` | String `"nil"` for display |
| 3 | 20 | `(or (buffer-local-value ...) "nil")` | String `"nil"` for owning-ws |
| 4 | 21 | `(or (claude-repl--workspace-for-buffer ...) "nil")` | String `"nil"` for persp-ws |
| 10 | 128 | `(or names '("DWC/mock-test"))` | Hardcoded mock branch name |
| 17 | 259 | `(or (claude-repl--ws-get ws :prefix-counter) 0)` | Counter defaults to 0 |
| 29 | 342 | `(or (plist-get diag :owning-ws) "nil")` | String `"nil"` for display |
| 33 | 346 | `(or before "nil")` | String `"nil"` for before-state |
| 34 | 346 | `(or after "nil")` | String `"nil"` for after-state |

---

## Section 3: `and` Chain Short-Circuits (8 instances)

| # | Line | Purpose |
|---|---|---|
| 6 | 48-49 | `current` workspace: fboundp + call |
| 7 | 50 | `default` workspace: current + member check |
| 22 | 307-308 | persp validity + buffer list |
| 24 | 313 | Process from vterm buffer |
| 25 | 314 | Process liveness |
| 26 | 315-316 | Owning workspace from buffer |
| 27 | 317 | Window visibility |
| 35 | 340 | Buffer name from vterm buffer |

---

## Section 4: `if`/`when`/`unless` Guards (14 instances)

Key instances:

| # | Line | Guard | Falls back TO |
|---|---|---|---|
| 12 | 149-154 | `file-directory-p` for output dir | "No files found" message |
| 13 | 173 | `if lines` for buffer info | String `"  (none)"` |
| 15 | 227-230 | `pcase` with **no catch-all** | `nil` for label -- **confusing output** |
| 16 | 246-247 | `if path` for log file path | Empty string (no path shown) |
| 28 | 341 | `if proc-alive` | String `"dead/nil"` (ambiguous) |
| 39 | 28-32 | `if vterm-running-p` | No-op before workspace delete |
| 40 | 86-91 | `if vterm-live-p` | Paste silently dropped |

---

## Section 5: `ignore-errors` (1 instance)

### Instance 36: Window deletion
- **Line:** 192
- **Code:** `(ignore-errors (delete-window win))`
- **Consequence:** Last-window deletion error silently swallowed. Window may remain with different buffer.

---

## Section 6: `completing-read` Defaults (3 instances)

| # | Line | Default |
|---|---|---|
| 5 | 39-41 | `(+workspace-current-name)` |
| 8 | 52 | Computed from history + known list |
| 11 | 137 | Pre-filled text `"DWC/mock-priority-test"` |

---

## Section 7: `cond`/`pcase` with Catch-All (2 instances)

### Instance 15: `pcase` with NO default
- **Line(s):** 227-230
- **Missing catch-all.** If `claude-repl-debug` holds an unexpected value, `label` is nil, and the message displays `nil` -- misleading.

### Instance 18: `cond` with `t` default
- **Line(s):** 279-291
- Five branches for buffer/process/timer/struct/other. The `cl-struct-p` branch is redundant with `t`.

---

## Section 8: Other Notable Instances

- **Instance 9:** `make-directory` with `t` flag -- self-bootstrapping directory creation
- **Instance 14:** `&optional verbose` with two-level toggle behavior
- **Instance 37-38:** `when-let` and `when proc` guards during buffer kill
- **Instance 41-43:** Three `if` expressions for priority display (empty -> `"(cleared)"`, empty -> `nil`, empty -> `"cleared"`)
- **Instance 45:** `unless known` guard -- hard abort with `user-error`
- **Instance 47:** `&rest _` -- ignoring all advice arguments

---

## Most Consequential

1. **Instance 15** (`pcase` with no catch-all) -- confusing behavior for unexpected debug values
2. **Instance 17** (prefix counter `or 0`) -- prevents arithmetic errors on uninitialized workspaces
3. **Instance 36** (`ignore-errors` around `delete-window`) -- silently swallows potentially important error
4. **Instance 1** (hardcoded output dir) -- no override mechanism for IPC directory
