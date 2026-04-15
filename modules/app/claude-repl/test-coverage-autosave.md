# Test Coverage Analysis: autosave.el

## Functions

### `claude-repl--save-buffer-if-modified (buf)`

Saves BUF silently if it is a live, modified, file-visiting buffer. Returns non-nil (t) if saved.

#### Branch conditions

The function has a single `when` with three conjuncts:
1. `(buffer-live-p buf)` -- buffer must be live
2. `(buffer-file-name buf)` -- buffer must visit a file
3. `(buffer-modified-p buf)` -- buffer must have unsaved modifications

All three must be true for the save path to execute.

#### Edge cases and test mapping

| # | Edge case | Test |
|---|-----------|------|
| 1 | buf is nil | `claude-repl-test-autosave-nil-buffer` |
| 2 | buf is a dead (killed) buffer | `claude-repl-test-autosave-dead-buffer` |
| 3 | buf is live but not visiting a file | `claude-repl-test-autosave-non-file-buffer` |
| 4 | buf is live + file-visiting but not modified | `claude-repl-test-autosave-unmodified-file-buffer` |
| 5 | buf is live + file-visiting + modified (happy path) | `claude-repl-test-autosave-modified-file-buffer-saves` |
| 6 | Save runs with `inhibit-message` set to t (silent) | `claude-repl-test-autosave-save-is-silent` |

**Coverage: 6/6 edge cases tested.**

---

### `claude-repl--autosave-workspace-buffers`

Iterates all perspectives when `persp-mode` is active, saves modified file-visiting buffers, and logs a summary when any buffers were saved.

#### Branch conditions

1. `(bound-and-true-p persp-mode)` -- early exit when persp-mode is nil/unbound
2. `(and persp (not (symbolp persp)))` -- distinguishes real perspective objects from symbol/nil entries
3. `(claude-repl--save-buffer-if-modified buf)` -- delegates per-buffer save decision
4. `(> saved 0)` -- only logs when at least one buffer was saved

#### Edge cases and test mapping

| # | Edge case | Test |
|---|-----------|------|
| 1 | `persp-mode` is nil -- function should be a no-op | `claude-repl-test-autosave-workspace-noop-when-persp-disabled` |
| 2 | `persp-persps` returns empty list | `claude-repl-test-autosave-workspace-no-perspectives` |
| 3 | Perspective entry is nil (symbol) -- should warn | `claude-repl-test-autosave-workspace-symbol-persp-warns` |
| 4 | Perspective entry is a non-nil symbol -- should warn | `claude-repl-test-autosave-workspace-symbol-persp-non-nil` |
| 5 | Perspective has buffers, all modified and file-visiting | `claude-repl-test-autosave-workspace-saves-modified-buffers` |
| 6 | Perspective has only unmodified buffers (saved=0, no log) | `claude-repl-test-autosave-workspace-skips-unmodified` |
| 7 | Mixed buffers: some file-visiting+modified, some not | `claude-repl-test-autosave-workspace-mixed-buffers` |
| 8 | Multiple perspectives each with saveable buffers | `claude-repl-test-autosave-workspace-multiple-perspectives` |
| 9 | Dead buffer within a perspective's buffer list | `claude-repl-test-autosave-workspace-dead-buffer-in-persp` |
| 10 | Perspective with empty buffer list | `claude-repl-test-autosave-workspace-empty-persp-buffers` |

**Coverage: 10/10 edge cases tested.**

---

### Timer registration (top-level side effect, line 31-32)

```elisp
(push (run-with-timer 300 300 #'claude-repl--autosave-workspace-buffers)
      claude-repl--timers)
```

This is a load-time side effect, not a callable function. The test-helpers infrastructure already suppresses `run-with-timer` during module loading (via advice override), preventing timer creation in tests. No dedicated test is needed for the timer push itself; the timer mechanism is an integration concern.

---

## Summary

| Function | Edge cases | Tests | Coverage |
|----------|-----------|-------|----------|
| `claude-repl--save-buffer-if-modified` | 6 | 6 | 100% |
| `claude-repl--autosave-workspace-buffers` | 10 | 10 | 100% |
| **Total** | **16** | **16** | **100%** |

All branches and edge cases in autosave.el are covered by test-autosave.el.
