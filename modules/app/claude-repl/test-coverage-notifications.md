# Test Coverage Analysis: notifications.el

## Functions

### `claude-repl--notify-backend-terminal-notifier (title message)`
Sends a desktop notification via `terminal-notifier` using `call-process`.

| # | Edge Case | Test Status |
|---|-----------|-------------|
| 1 | Normal title and message are passed as correct args to `call-process` | Covered |
| 2 | Empty string title and message | Covered |
| 3 | Special characters (quotes, newlines) in title/message | Covered |
| 4 | Very long title or message strings | Not covered |
| 5 | Non-ASCII / Unicode characters in title/message | Not covered |

### `claude-repl--notify-backend-osascript (title message)`
Sends a desktop notification via `osascript` using `start-process`.

| # | Edge Case | Test Status |
|---|-----------|-------------|
| 1 | Normal title and message produce correct osascript command | Covered |
| 2 | The process name is "claude-notify" and buffer is nil | Covered |
| 3 | The generated AppleScript includes `display notification`, title, message, and sound | Covered |
| 4 | Empty string title and message | Covered |
| 5 | Special characters (quotes, newlines) -- relies on `format %S` quoting | Covered |
| 6 | Non-ASCII / Unicode characters in title/message | Not covered |
| 7 | `start-process` returning nil (process creation failure) | Not covered (function ignores return value) |

### `claude-repl--select-notification-backend ()`
Selects the best available notification backend based on `executable-find`.

| # | Edge Case | Test Status |
|---|-----------|-------------|
| 1 | `terminal-notifier` is found -- returns `#'claude-repl--notify-backend-terminal-notifier` | Covered |
| 2 | `terminal-notifier` is NOT found -- falls back to `#'claude-repl--notify-backend-osascript` | Covered |
| 3 | Both backends unavailable (osascript is always the fallback, never checked) | Implicitly covered by case 2 |

### `claude-repl--notification-backend` (defvar)
Variable set at load time by calling `claude-repl--select-notification-backend`.

| # | Edge Case | Test Status |
|---|-----------|-------------|
| 1 | Variable is bound after loading | Covered |
| 2 | Variable holds a callable function | Covered |

### `claude-repl--notify (title message)`
High-level dispatch: logs then funcalls the backend.

| # | Edge Case | Test Status |
|---|-----------|-------------|
| 1 | Dispatches title and message to the current backend function | Covered |
| 2 | Calls `claude-repl--log` when `claude-repl-debug` is non-nil | Covered |
| 3 | Logging happens before backend dispatch (ordering) | Covered |
| 4 | Does not error when `claude-repl-debug` is nil | Covered |
| 5 | Backend function signals an error (no error handling in notify) | Not covered |

## Summary

- **Total functions**: 4 (+ 1 defvar)
- **Edge cases identified**: 19
- **Covered by tests**: 14
- **Not yet covered**: 5

### Uncovered edge cases (low priority)

All uncovered cases are low-risk scenarios:

1. **Very long strings** -- `call-process` and `start-process` handle argument length at the OS level; no Elisp-level concern.
2. **Non-ASCII/Unicode** -- would exercise encoding paths in process invocation, but `call-process`/`start-process` pass strings through unchanged.
3. **`start-process` returning nil** -- the osascript backend discards the return value, so there is nothing to assert.
4. **Backend function signaling an error** -- `claude-repl--notify` has no error handling; this is a design choice (let errors propagate), not a bug.
