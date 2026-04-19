# Fallback and Default Logic Report: `core.el`

**Total instances: 59**

This is the largest and most fallback-dense file in the module.

---

## Section 1: `defvar` / `defvar-local` / `defcustom` Defaults (9 instances)

| # | Line | Variable | Default | Consequence |
|---|---|---|---|---|
| 1 | 6 | `claude-repl--timers` | `nil` | No active timers on load |
| 2 | 29-36 | `claude-repl-debug` | `nil` (Off) | All debug logging suppressed by default |
| 3 | 38-46 | `claude-repl-log-to-file` | `t` | File logging ON even when messages-buffer logging is OFF |
| 4 | 112-114 | `claude-repl--log-format-bug-captured` | `nil` | First non-string FMT triggers full backtrace capture |
| 5 | 211-213 | `claude-repl--project-root` (buffer-local) | `nil` | Buffers without explicit root fall through to `default-directory` |
| 6 | 309 | `claude-repl--workspaces` | Empty hash table | Clean slate; all lookups return nil |
| 7 | 355-358 | `claude-repl--owning-workspace` (buffer-local) | `nil` | No workspace owns the buffer |
| 8 | 252-254 | `claude-repl-git-branch` | Computed at load time | Empty string `""` when not in a git repo |
| 9 | 256-260 | `claude-repl--main-git-root` | Computed at load time | **`"/"`** when loaded outside git (potential bug -- `string-empty-p` doesn't catch `"/"`) |

---

## Section 2: `or` Expressions (8 instances)

| # | Line | Code | Fallback chain |
|---|---|---|---|
| 10 | 56 | `(or (plist-get ... :ws-id) (when-let ...))` | Cached ID -> compute from project-dir -> nil |
| 11 | 95-109 | `(or id "-")` etc. (8 fields) | Metadata value -> `"-"` placeholder |
| 12 | 228 | `(or dir default-directory)` | Explicit dir -> buffer's `default-directory` |
| 13 | 277 | `(or git claude-repl--project-root default-directory)` | **Three-tier chain**: git root -> buffer-local root -> default-directory |
| 14 | 348 | `(or (claude-repl--ws-get ws :active-env) :bare-metal)` | Stored env -> `:bare-metal` default |
| 15 | 349-353 | `(or (claude-repl--ws-get ws env) (make-...))` | Existing struct -> freshly created struct |
| 16 | 387-388 | `(or ws (and (fboundp ...) (+workspace-current-name)))` | Explicit ws -> current workspace |
| 17 | 390 | `(or suffix "")` and `(or safe "default")` | Two fallbacks in one `format` call |

---

## Section 3: Guards (20 instances)

Key instances:

- **Line 14:** `(when (timerp timer) ...)` -- Skips non-timer values in timer list
- **Line 19-20:** `(when (fboundp 'claude-repl--log) ...)` -- Guards against log function not yet defined at load time
- **Line 69:** `(if (or (null ws) (not (boundp 'claude-repl--workspaces))) "")` -- Empty string when no workspace context
- **Line 100-101:** Three-state buffer display: `"live"` / `"dead"` / `"-"`
- **Line 104:** Three-state process display: `"run"` / `"done"` / `"-"`
- **Lines 143-146:** Non-string FMT handling -- substitutes `"[BUG non-string-fmt=...]"` diagnostic
- **Lines 181-190:** Parallel non-string FMT handling in `claude-repl--do-log`
- **Lines 155-158:** Logfile path resolution: static root -> dynamic `(claude-repl--git-root)` -> nil (no logging)
- **Line 342-343:** `(unless (claude-repl--ws-get ws :project-dir) ...)` -- "first write wins" for project-dir
- **Lines 413-417:** Perspective attachment: requires ws + `persp-get-by-name` + `persp-add-buffer` all available

---

## Section 4: `condition-case` / `ignore-errors` (2 instances)

| # | Line | Code | Consequence |
|---|---|---|---|
| 38 | 168 | `(ignore-errors (write-region ...))` | Log file write failures silently swallowed |
| 39 | 123 | `(ignore-errors (backtrace))` | Backtrace capture failure produces empty string |

---

## Section 5: `&optional` Parameters (6 instances)

Lines 225, 382 (x2), 394, 420, 427 -- standard Emacs optional-buffer/ws/suffix patterns.

---

## Section 6: `cl-defstruct` Defaults

**Line(s):** 302-307

```elisp
(cl-defstruct claude-repl-instantiation
  had-session
  session-id
  start-cmd)
```

All three fields default to `nil`.

---

## Section 7: Hardcoded Constants (8 instances)

| Value | Line | Purpose |
|---|---|---|
| `8` | 58, 288 | MD5 hash substring length for workspace IDs |
| `"%H:%M:%S.%3N"` | 127, 147, 183, 188 | Log timestamp format |
| `".claude-repl.log"` | 159 | Logfile name |
| `"*claude-panel"` | 368, 374, 390 | Buffer name prefix |
| `"default"` | 390 | Fallback workspace name |
| `'equal` | 309 | Hash table comparison function |
| `" *Minibuf"` | 442 | Minibuffer detection pattern |
| `'silent` | 169 | write-region visit argument |

---

## Notable Fallback Chains (Multi-Tier)

1. **Workspace root resolution** (deepest): git-root -> project-root -> default-directory (3 tiers)
2. **Buffer naming chain**: explicit ws -> `(+workspace-current-name)` -> `"default"` (3 tiers)
3. **Log file path**: static main-git-root -> dynamic git-root -> nil/no logging (3 tiers)
4. **Active instantiation**: cached struct -> fresh struct; env: explicit -> `:bare-metal` (2 tiers each)

## Potential Concerns

- **Instance 9/30 interaction**: `claude-repl--main-git-root` can be `"/"` when loaded outside git, but `string-empty-p` doesn't catch `"/"`. Could write to `"/.claude-repl.log"`.
- **Instance 17**: `"default"` fallback means unrelated contexts without workspace names collide on `*claude-panel-default*`.
