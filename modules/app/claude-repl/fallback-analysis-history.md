# Fallback and Default Logic Report: `history.el`

**Total instances: 41**

---

## Section 1: Error Swallowing Macro

### Instance 1: `claude-repl--with-error-logging`
- **Line(s):** 13-19
- **Code:** `condition-case` wrapping body, logging errors
- **Consequence:** Caller receives `nil` with no indication an error occurred. Cannot distinguish "BODY returned nil normally" from "BODY threw an error."

---

## Section 2: File I/O Guards (6 instances)

| # | Line | Function | Falls back TO |
|---|---|---|---|
| 2 | 36-41 | `read-sexp-file-if-exists` | `nil` when file missing |
| 5 | 71-74 | `state-file` | `nil` when root is nil |
| 6 | 82-94 | `state-purge` | No-op when root is nil |
| 7 | 92 | `state-purge` inner | Skip nonexistent files |
| 8 | 93 | `state-purge` delete | `ignore-errors` -- silent failure, no log |
| 20 | 188 | `state-restore` `when file` | Redundant nil guard |

---

## Section 3: Serialization Guards (2 instances)

| # | Line | Function | Falls back TO |
|---|---|---|---|
| 3 | 46-52 | `instantiation-to-plist` | `nil` when inst is nil |
| 4 | 54-63 | `instantiation-from-plist` | No-op when either arg is nil |

---

## Section 4: Workspace/Buffer Resolution (3 instances)

### Instance 9: `or` fallback for WS parameter
- **Line:** 98-106
- **Code:** `(or ws (+workspace-current-name))`
- **Falls back TO:** Current workspace name

### Instance 10: `and` short-circuit for WS
- **Line:** 102

### Instance 11: Dead/missing buffer fallback
- **Line(s):** 103-106
- **Falls back TO:** `nil` (stale buffer references treated same as missing)

---

## Section 5: State Save/Restore (7 instances)

| # | Line | Function | Falls back TO |
|---|---|---|---|
| 13 | 114-120 | `history-save` (if-let) | No-op when no live input buffer |
| 14 | 117-119 | `history-save` (nil check) | Skip write; old file remains (stale data) |
| 15 | 122-128 | `history-restore` | Leave history unchanged |
| 16 | 146-156 | `state-save` | Skip when no `:project-dir` |
| 17 | 154-156 | `state-save` (error) | Error swallowed via macro |
| 18 | 170-172 | `apply-restored-state` | Skip project-dir restore if missing |
| 19 | 185-186 | `state-restore` root | `(or ws-project-dir (claude-repl--git-root default-directory))` |

### Instance 21: Multi-branch `cond` for nil handling
- **Line(s):** 189-198
- **Three nil branches:** null root, null file, null data -- each logged and skipped
- **Note:** `(null file)` branch is effectively unreachable

---

## Section 6: `defvar-local` Defaults (4 instances)

| # | Line | Variable | Default | Significance |
|---|---|---|---|---|
| 23 | 202-203 | `claude-repl--input-history` | `nil` | Empty history |
| 24 | 205-206 | `claude-repl--history-index` | `-1` | Sentinel: "not browsing" |
| 25 | 208-209 | `claude-repl--history-stash` | `nil` | No stashed text |
| 26 | 211-212 | `claude-repl--history-navigating` | `nil` | Not navigating |

---

## Section 7: History Navigation (10 instances)

### Instance 27: `or` for TEXT parameter
- **Line:** 214-217
- **Code:** `(or text (buffer-string))`
- **Falls back TO:** Current buffer contents

### Instance 29-30: Empty/duplicate skip guards
- **Line(s):** 218-222
- **Falls back TO:** No mutation of history list

### Instance 31b: Stash-or-empty-string
- **Line:** 245
- **Code:** `(or claude-repl--history-stash "")`
- **Prevents:** inserting literal `"nil"` into buffer

### Instances 32-35: Navigation bounds checks
- Empty history guard, bounds check, stash-on-first-nav, non-negative index guard

### Instance 36: Double guard for reset
- **Line(s):** 268-271
- **Two conditions:** not programmatic AND currently browsing

---

## Section 8: Hardcoded Constants (4 instances)

| # | Line | Value | Purpose |
|---|---|---|---|
| 38 | 7-9 | `'(:bare-metal :sandbox)` | Fixed environment key list |
| 39 | 76-81 | `'(".claude-repl-state" ".claude-repl-history")` | Persistence filenames |
| 40 | 69 | `".claude-repl-history"` | History filename |
| 41 | 73 | `".claude-repl-state"` | State filename |

---

## Summary

| Category | Count |
|---|---|
| `condition-case` / `ignore-errors` | 4 |
| `or` expressions with fallback values | 3 |
| `if`/`when`/`unless` nil guards | 13 |
| `cond` with multiple nil branches | 2 |
| `&optional` parameters | 2 |
| `defvar-local` defaults | 4 |
| `defconst` hardcoded values | 2 |
| Hardcoded strings | 2 |
| Sentinel values | 1 |
| `&rest _` argument ignoring | 1 |
| **Total** | **41** |

**No `defcustom`, `cl-defun` with `&key`, `pcase`, or `cl-case` in this file.**
