# Fallback and Default Logic Report: `notifications.el`

**Total instances: 8**

---

## Instance 1: `cond` with catch-all `t` -- backend selection

**Line(s):** 20-27

```elisp
(cond
 ((executable-find "terminal-notifier")
  #'claude-repl--notify-backend-terminal-notifier)
 (t
  #'claude-repl--notify-backend-osascript))
```

**Falls back FROM:** `terminal-notifier` being on PATH.

**Falls back TO:** `osascript`-based backend. **Does NOT check whether `osascript` exists.** On non-macOS systems, every notification silently fails.

---

## Instance 2: `defvar` with load-time computation

**Line(s):** 29-30

```elisp
(defvar claude-repl--notification-backend (claude-repl--select-notification-backend) ...)
```

**Consequence:** Backend is frozen at file-load time. Installing `terminal-notifier` after load has no effect. `defvar` semantics mean pre-existing values are preserved.

---

## Instance 3: `call-process` with `0` destination (terminal-notifier)

**Line:** 8

**Consequence:** Stdout/stderr discarded, exit code unchecked. Process runs async. Notification failures are completely silent.

---

## Instance 4: `start-process` with `nil` buffer (osascript)

**Line(s):** 14-17

**Consequence:** All output discarded. No sentinel, no filter. But `start-process` CAN signal an error if the executable is not found -- and there is no `condition-case` wrapping it.

---

## Instance 5: Hardcoded `"default"` sound name

**Line:** 17

```elisp
(format "display notification %S with title %S sound name \"default\"" ...)
```

**Consequence:** Every osascript notification plays the system default sound. Not configurable. The terminal-notifier backend does NOT specify a sound -- asymmetric behavior.

---

## Instance 6: Hardcoded process name `"claude-notify"`

**Line:** 14

**Consequence:** Every osascript process has the same name. Rapid notifications cause auto-rename (`<2>`, `<3>`, etc.).

---

## Instance 7: `ws` parameter accepts nil

**Line(s):** 32-36

**Consequence:** `ws` exists solely for logging. The notification itself completely ignores `ws`.

---

## Instance 8: `nil` passed to log ws param

**Lines:** 23, 26

**Consequence:** Load-time log entries have no workspace context.

---

## Summary

| Category | Count |
|---|---|
| `cond` catch-all (no existence check) | 1 |
| `defvar` load-time freeze | 1 |
| Silent process failure | 2 |
| Hardcoded values | 2 |
| Nil-tolerant parameters | 2 |
| **Total** | **8** |

**Most consequential:** Instance 1 (osascript selected without existence check) and Instance 2 (load-time bake). Together they mean a non-macOS system silently gets a broken notification backend that is never recoverable.
