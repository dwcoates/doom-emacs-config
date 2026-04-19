# Fallback and Default Logic Report: `autosave.el`

**Total instances: 9**

---

## Instance 1: `when` guard -- three-predicate liveness/file/modification check

**Line(s):** 8-10

**Code:**
```elisp
(when (and (buffer-live-p buf)
           (buffer-file-name buf)
           (buffer-modified-p buf))
```

**Falls back FROM:** A buffer `buf` that is simultaneously live, visiting a file, and modified -- the "happy path" where saving is meaningful.

**Falls back TO:** Implicit `nil` return. The entire `when` body (lines 11-15) is skipped. The function returns `nil` instead of `t`.

**Consequence:** When any of the three predicates fails -- the buffer has been killed, the buffer does not visit a file (e.g., a scratch buffer, a `*Messages*` buffer, a vterm buffer), or the buffer has no unsaved modifications -- the function silently does nothing. No save attempt occurs, no log message is emitted, and `nil` is returned. This means the caller (`claude-repl--autosave-workspace-buffers`) will not increment its `saved` counter for this buffer. The fallback is purely protective: it prevents calling `save-buffer` on buffers where saving is impossible or pointless.

---

## Instance 2: `bound-and-true-p` guard on `persp-mode`

**Line:** 20

**Code:**
```elisp
(when (bound-and-true-p persp-mode)
```

**Falls back FROM:** The `persp-mode` variable being both bound (the `persp-mode` package is loaded) and truthy (the mode is actually active).

**Falls back TO:** Implicit `nil`. The entire function body (lines 21-31) is skipped.

**Consequence:** If the perspective/workspace system is not loaded or not active, the autosave function becomes a complete no-op. No buffers are scanned, no saves are attempted, no log messages are produced. The timer on line 33 will still fire every 300 seconds, but each invocation will immediately return `nil`. This is a dual-layer guard: `bound-and-true-p` first checks whether `persp-mode` is even a defined symbol (preventing a void-variable error if `persp-mode.el` was never loaded), then checks whether the mode is enabled. This means the fallback handles two distinct failure modes -- package absence and mode-disabled -- identically.

---

## Instance 3: `let` binding with hardcoded initial counter value

**Line:** 22

**Code:**
```elisp
(let ((saved 0))
```

**Falls back FROM:** N/A -- there is no dynamic value being attempted first. The counter is unconditionally initialized.

**Falls back TO:** The hardcoded integer `0`.

**Consequence:** This is a default initial value, not a conditional fallback. However, it establishes the baseline assumption that zero buffers have been saved. The counter is only incremented when `claude-repl--save-buffer-if-modified` returns non-nil. If no buffers need saving, `saved` remains `0`, which triggers the "no buffers needed saving" log path on line 31 rather than the "saved N buffer(s)" log path on line 30. The default of `0` thus directly controls which branch of the final `if` executes.

---

## Instance 4: `if` guard on perspective validity with warning fallback

**Line(s):** 24-28

**Code:**
```elisp
(if (and persp (not (symbolp persp)))
    (dolist (buf (persp-buffers persp))
      (when (claude-repl--save-buffer-if-modified buf)
        (cl-incf saved)))
  (claude-repl--log nil "WARN: autosave encountered non-perspective entry: %S" persp))
```

**Falls back FROM:** A valid perspective object -- one that is non-nil AND not a bare symbol. The `persp-persps` function is expected to return a list of perspective structs, but may include `nil` entries or symbol entries (e.g., `'none` used by some persp-mode internals to represent a "no perspective" sentinel).

**Falls back TO:** A warning log message. No buffers from this "perspective" are iterated or saved.

**Consequence:** When an entry from `(persp-persps)` is `nil` or a symbol, the code logs a warning and skips that entry entirely. This means any buffers that would have been associated with such an entry are never autosaved during this cycle. The fallback is defensive: it prevents calling `persp-buffers` on something that is not a valid perspective struct, which would likely signal an error. The `%S` format directive in the log message prints the raw Lisp representation of the invalid entry, providing diagnostic information. Note the asymmetry: the primary branch silently saves buffers, while the fallback branch always logs -- making anomalies visible even at non-verbose log levels.

---

## Instance 5: `if` branch on saved count -- zero vs. nonzero logging

**Line(s):** 29-31

**Code:**
```elisp
(if (> saved 0)
    (claude-repl--log nil "autosave: saved %d buffer(s)" saved)
  (claude-repl--log-verbose nil "autosave-workspace-buffers: no buffers needed saving"))
```

**Falls back FROM:** Having saved at least one buffer (`saved > 0`).

**Falls back TO:** A verbose-only log message saying no buffers needed saving.

**Consequence:** When no buffers were saved, the function emits a log message only at the `verbose` debug level (via `claude-repl--log-verbose`), meaning it is invisible unless `claude-repl-debug` is set to `'verbose`. When at least one buffer was saved, the function emits a standard log message (via `claude-repl--log`) that is always visible. This is a deliberate fallback in logging behavior: the "nothing happened" case is treated as low-signal noise and suppressed by default, while the "something happened" case is always reported. The `else` branch is the fallback/default outcome.

---

## Instance 6: `nil` passed as workspace argument to all logging calls

**Lines:** 14, 21, 28, 30, 31

**Code (representative):**
```elisp
(claude-repl--log-verbose nil "save-buffer-if-modified: saved buffer=%s" ...)
```

**Falls back FROM:** A specific workspace name string that would identify which workspace context the log message pertains to.

**Falls back TO:** `nil` -- no workspace context.

**Consequence:** Every logging call in this file passes `nil` as the `ws` (workspace) argument. The autosave module opts out of workspace-scoped log tagging because it operates across ALL workspaces (it iterates `(persp-persps)` globally), so no single workspace name would be accurate.

---

## Instance 7: Hardcoded timer intervals

**Line:** 33

**Code:**
```elisp
(push (run-with-timer 300 300 #'claude-repl--autosave-workspace-buffers)
      claude-repl--timers)
```

**Falls back FROM:** There is no configurable variable or `defcustom` for the timer interval.

**Falls back TO:** The hardcoded integer `300` (seconds = 5 minutes), used for both the initial delay and the repeat interval.

**Consequence:** The autosave interval is baked in at 300 seconds with no way for a user to customize it without editing source code. There is no `defcustom` like `claude-repl-autosave-interval` that would allow configuration.

---

## Instance 8: Implicit `nil` return from `claude-repl--save-buffer-if-modified`

**Line(s):** 5-15 (function as a whole)

**Code:**
```elisp
(defun claude-repl--save-buffer-if-modified (buf)
  ...
  (when (and ...)
    ...
    t))
```

**Falls back FROM:** The `when` body executing and returning `t`.

**Falls back TO:** `nil`, the implicit return value when a `when` form's condition is false.

**Consequence:** The function uses its return value as a boolean signal: `t` means "a save was performed," `nil` means "no save was needed or possible." This return value is consumed on line 26 by `(when (claude-repl--save-buffer-if-modified buf) (cl-incf saved))`.

---

## Instance 9: `inhibit-message` binding to suppress save notification

**Line:** 12

**Code:**
```elisp
(let ((inhibit-message t))
  (save-buffer))
```

**Falls back FROM:** The normal behavior of `save-buffer`, which displays a message like `"Wrote /path/to/file"` in the echo area.

**Falls back TO:** Silent operation -- `inhibit-message` bound to `t` suppresses all `message` calls within the dynamic extent.

**Consequence:** Autosaves are invisible to the user. Without this binding, every autosave would flash a "Wrote ..." message in the minibuffer.

---

## Summary

| Category | Count |
|---|---|
| `when`/`if` guards that skip code on nil/false | 4 |
| Hardcoded numeric defaults | 2 |
| Implicit `nil` return as default | 2 |
| Hardcoded `nil` passed as argument default | 1 |
| Hardcoded behavior override | 1 |
| **Total** | **9** |

**No `defcustom`, `condition-case`, `ignore-errors`, `&optional`, or `or` expressions exist in this file.**
