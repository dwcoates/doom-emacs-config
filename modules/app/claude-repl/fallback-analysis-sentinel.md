# Fallback and Default Logic Report: `sentinel.el`

**Total instances: 39**

---

## Section 1: Core Resolution Chain

### Instance 13-14: Two-tier workspace resolution
- **Line:** 103, 105
- **Code:** `(unless fast (claude-repl--ws-for-dir-container dir))` then `(or fast container)`
- **Chain:** Fast path (git-root matching) -> container path (Docker basename matching) -> nil (unroutable)
- **Consequence:** If the fast path returns non-nil but WRONG, the container path never corrects it.

### Instance 15: `cond` for log source
- **Line:** 107
- **Code:** `(cond (fast "fast-path") (container "container-path") (t "NONE"))`

---

## Section 2: File Reading with Error Handling

### Instance 16-17: `condition-case` for file-missing and generic error
- **Line(s):** 119, 132-137
- **Two handlers:** `file-missing` (race condition) and `error` (any other failure)
- **Both fall back TO:** `nil` (event silently dropped)

### Instance 18: `or` with empty-string fallback for dir
- **Line:** 124
- **Code:** `(or (nth 0 lines) "")`
- **Consequence:** Empty file produces `dir = ""` (not nil). **The downstream `and dir` guard does NOT catch empty strings**, so the empty string leaks into git-root resolution.

### Instance 19: `when` nil guard for session-id
- **Line:** 125
- **Falls back TO:** `nil` -- backward-compatibility for legacy single-line sentinel files.

---

## Section 3: Workspace Matching Guards

### Instances 2-5: Fast-path nil guards
- Lines 32-35, 43-44
- Cascading nil checks: dir -> target-root -> canonical-target -> proj -> canonical-proj

### Instances 8-11: Container-path guards
- Lines 69-83
- `bound-and-true-p persp-mode` gate, proj-dir -> canonical -> basename chain

### Instance 37: Watch directory extraction (Emacs version compat)
- **Line(s):** 374-378
- **Two-branch `cond`:** Modern `file-notify--watch-p` API -> legacy cons-cell format
- **Consequence:** If future Emacs changes internal representation, `watch-dir` is nil and stale watchers leak.

---

## Section 4: Event Processing

### Instance 21: `ignore-errors` around `delete-file`
- **Line:** 178
- **Risk:** Failed deletion means duplicate event processing.

### Instance 22: `cond` -- main dispatch gate
- **Line(s):** 181-193
- **Three branches:** null dir (read failed) -> null ws (unroutable, user warning) -> success

### Instance 27: `cond` in session-start handler
- **Line(s):** 244-258
- **Three branches:** null/dead vterm (error message) -> already ready (no-op) -> ready transition

### Instance 28: `cl-loop` dispatch with no default
- **Line(s):** 287-290
- **Falls back TO:** `nil` when no prefix matches filename

### Instance 32: Event type filtering
- **Line(s):** 313-327
- **Filters:** Non-string files skipped, `hook-debug.log` silently dropped

---

## Section 5: Variable Defaults

| # | Line | Variable | Default |
|---|---|---|---|
| 1 | 10-12 | `claude-repl--sentinel-dir` | `~/.claude/workspace-notifications` (hardcoded) |
| 36 | 359-362 | `claude-repl--sentinel-watch-descriptor` | `nil` |

---

## Section 6: Infrastructure

### Instance 38: `make-directory` with `t`
- **Line:** 407
- Self-bootstrapping directory creation.

### Instance 39: Conditional logging for reap count
- **Line(s):** 409-410
- Only logs when `reaped > 0`.

---

## Most Consequential

1. **Instance 14** (`or fast container`) -- backbone of workspace resolution
2. **Instances 16-17** (`condition-case` catching ALL file read errors) -- masks systemic problems
3. **Instance 21** (`ignore-errors` around `delete-file`) -- duplicate event risk
4. **Instance 37** (Emacs version compat for watch-dir) -- future-proofing gap
5. **Instance 18** (empty string leaking through nil checks) -- downstream matching fails silently
