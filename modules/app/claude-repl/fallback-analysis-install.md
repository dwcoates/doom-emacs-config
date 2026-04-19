# Fallback and Default Logic Report: `install.el`

**Total instances: 25**

---

## Section 1: `or` Expressions (4 instances)

### Instance 1-2: Load-time file-path resolution
- **Lines:** 34, 42
- **Code:** `(or load-file-name (buffer-file-name))`
- **Falls back TO:** `buffer-file-name` for interactive evaluation. If BOTH are nil, silent wrong-path failure.

### Instance 3: Sandbox detection
- **Line(s):** 63-64
- **Code:** `(or (file-exists-p "/.dockerenv") (equal (getenv "DOOM_SANDBOX") "1"))`
- **Falls back TO:** Env var check when Docker detection fails.

### Instance 13: Severity lookup fallback
- **Line:** 219
- **Code:** `(or (cdr (assq event claude-repl--hook-severity)) 'warn)`
- **Falls back TO:** `'warn` -- a potentially critical missing hook would be reported as merely a warning. **Currently dead code** since all events are covered, but would activate if `claude-repl--managed-hooks` gained a new entry.

---

## Section 2: `ignore-errors` / `condition-case`

### Instance 4: JSON parse error swallowing
- **Line:** 72
- **Code:** `(ignore-errors (json-read-file path))`
- **Consequence:** Malformed JSON, encoding issues, permission errors all silently produce `nil`. No diagnostic about WHY the JSON was unreadable.

---

## Section 3: Guards (7 instances)

| # | Line | Guard | Falls back TO |
|---|---|---|---|
| 5 | 71-72 | `file-exists-p` for settings.json | `nil` (skip parse) |
| 10 | 105-107 | `file-exists-p` for install script | Hard `error` signal |
| 11 | 126-127 | `claude-repl--in-sandbox-p` | No-op message (skip install) |
| 12 | 131-136 | Exit code check | `display-buffer` + error on non-zero |
| 14 | 194-197 | `file-readable-p` | `nil` (unreadable file = "no drift") |
| 17 | 251-252 | `claude-repl--in-sandbox-p` | `nil` (no issues in sandbox) |
| 18 | 255-261 | `(not json)` | Single coarse error instead of per-hook checks |

---

## Section 4: Implicit nil from `assq`/`cdr`/`and`

| # | Line | Context | Consequence |
|---|---|---|---|
| 6 | 79 | Event lookup | `nil` from missing event key |
| 7 | 80-86 | `and` short-circuit | Skip `seq-some` on nil entries |
| 8 | 83-84 | Inner `assq` misses | Malformed JSON silently = "not found" |
| 9 | 93-94 | `when-let*` | Conflates "can't read settings" with "no hooks" |
| 15 | 205 | `and` in drift check | Unreadable file = "no drift" |
| 24 | 94 | `assq 'hooks` | Missing hooks key = hooks not installed |

---

## Section 5: `defconst` Hardcoded Defaults (5 instances)

| # | Line | Variable | Value |
|---|---|---|---|
| 19 | 23-27 | `claude-repl--managed-hooks` | 4 hook/path pairs |
| 20 | 47 | `claude-repl--install-output-buffer` | `"*claude-repl-install*"` |
| 21 | 50 | `claude-repl--settings-file` | `"~/.claude/settings.json"` |
| 22 | 53 | `claude-repl--hooks-dest-dir` | `"~/.claude/hooks/"` |
| 23 | 166-171 | `claude-repl--hook-severity` | 4 event/severity pairs |

---

## Section 6: `cond` with Ordered Branches

### Instance 16: Script file checks
- **Line(s):** 228-242
- **Priority:** missing > not-executable > drifted > no issue

---

## Most Consequential Instances

1. **Instance 4** (`ignore-errors` around JSON parse) -- silently swallows all parse failures
2. **Instance 13** (`or ... 'warn` severity fallback) -- could silently downgrade a missing critical hook to a warning
3. **Instance 18** (early-exit when settings.json unreadable) -- skips script-file checks too
