# Test Coverage Analysis: commands.el

## Summary

Test file: `test-commands.el`
Source file: `commands.el`

Emacs was not available when this analysis was written, so tests could not be executed.
They are structurally valid ERT tests and should be verified once Emacs is available.

---

## Function-by-Function Edge Case Analysis

### 1. `agent-repl--buffer-relative-path`

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | Non-file buffer (no `buffer-file-name`) signals `user-error` | YES | `agent-repl-cmd-test-buffer-relative-path/non-file-buffer` |
| 2 | File-visiting buffer returns relative path | YES | `agent-repl-cmd-test-buffer-relative-path/file-buffer` |
| 3 | Deeply nested subdirectory path | YES | `agent-repl-cmd-test-buffer-relative-path/nested-subdir` |
| 4 | File at project root (bare filename) | YES | `agent-repl-cmd-test-buffer-relative-path/file-at-root` |
| 5 | Project root without trailing slash | YES | `agent-repl-cmd-test-buffer-relative-path/root-without-trailing-slash` |

### 2. `agent-repl--format-file-ref`

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | No active region -- returns `file:line` | YES | `agent-repl-cmd-test-format-file-ref/no-region` |
| 2 | Active region -- returns `file:start-end` and deactivates mark | YES | `agent-repl-cmd-test-format-file-ref/with-region` |
| 3 | Region spanning a single line (start == end) | YES | `agent-repl-cmd-test-format-file-ref/single-line-region` |
| 4 | Point at first line of buffer | YES | `agent-repl-cmd-test-format-file-ref/first-line` |

### 3. `agent-repl--format-magit-hunk-ref`

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | Normal hunk with multi-line range | YES | `agent-repl-cmd-test-format-magit-hunk-ref/basic` |
| 2 | Single-line hunk (len=1, start==end) | YES | `agent-repl-cmd-test-format-magit-hunk-ref/single-line-hunk` |
| 3 | magit-toplevel differs from resolve-root | YES | `agent-repl-cmd-test-format-magit-hunk-ref/different-roots` |

### 4. `agent-repl--context-reference`

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | Non-magit buffer delegates to `format-file-ref` | YES | `agent-repl-cmd-test-context-reference/non-magit-delegates-to-format-file-ref` |
| 2 | Magit diff mode in hunk section delegates to `format-magit-hunk-ref` | YES | `agent-repl-cmd-test-context-reference/magit-hunk-delegates-to-magit-ref` |
| 3 | Magit mode but NOT on a hunk section (e.g., file header) falls through | YES | `agent-repl-cmd-test-context-reference/magit-non-hunk-section` |

### 5. `agent-repl--send-diff-analysis`

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | Formats "for the SPEC, PROMPT" correctly | YES | `agent-repl-cmd-test-send-diff-analysis/formats-message` |

### 6. `agent-repl--resolve-change-spec`

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | String default-spec, no overrides | YES | `agent-repl-cmd-test-resolve-change-spec/string-default` |
| 2 | `:use-branch-diff-spec` returns symbol | YES | `agent-repl-cmd-test-resolve-change-spec/branch-returns-symbol` |
| 3 | Override alist takes precedence over default | YES | `agent-repl-cmd-test-resolve-change-spec/override-takes-precedence` |
| 4 | Override alist present but missing scope falls through to default | YES | `agent-repl-cmd-test-resolve-change-spec/override-missing-scope-falls-through` |
| 5 | Override present but branch scope still returns symbol | YES | `agent-repl-cmd-test-resolve-change-spec/override-branch-still-uses-symbol` |

### 7. `agent-repl--diff-command-form` (macro helper)

Tested indirectly through the macro expansion tests:

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | All 5 scopes generated for all 7 families (35 commands) | YES | `agent-repl-cmd-test-diff-commands/all-scopes-generated` |
| 2 | Worktree scope sends correct spec and prompt | YES | `agent-repl-cmd-test-diff-commands/explain-diff-worktree-sends` |
| 3 | Scope overrides used for update-pr-diff family | YES | `agent-repl-cmd-test-diff-commands/update-pr-diff-uses-override` |
| 4 | Branch scope uses the custom variable value | YES | `agent-repl-cmd-test-diff-commands/branch-uses-custom-var` |

### 8. `agent-repl--send-to-claude`

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | Claude not running -> calls initialize-claude before send | YES | `agent-repl-cmd-test-send-to-claude/not-running-initializes-first` |
| 2 | Claude already running -> skips initialize-claude | YES | `agent-repl-cmd-test-send-to-claude/running-skips-init` |

### 9. `agent-repl-explain` (interactive)

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | Sends "please explain REF" with context reference | YES | `agent-repl-cmd-test-explain/sends-context-reference` |

### 10. `agent-repl--send-interrupt-escape`

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | Sends exactly two escape key presses | YES | `agent-repl-cmd-test-send-interrupt-escape/sends-two-escapes` |

### 11. `agent-repl--enter-insert-mode`

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | Current ws, live input buffer: enters evil insert state | YES | `agent-repl-cmd-test-enter-insert-mode/live-input-buffer` |
| 2 | Never forwards a literal "i" to the vterm (regression) | YES | `agent-repl-cmd-test-enter-insert-mode/never-sends-i-to-vterm` |
| 3 | WS not current: no-op (no focus steal / hidden-buffer flip) | YES | `agent-repl-cmd-test-enter-insert-mode/noop-when-ws-not-current` |
| 4 | Dead input buffer: no-op | YES | `agent-repl-cmd-test-enter-insert-mode/dead-input-buffer` |

### 12. `agent-repl-interrupt` (interactive)

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | Vterm live: sends escape and schedules insert-mode re-entry | YES | `agent-repl-cmd-test-interrupt/sends-escape-when-vterm-live` |
| 2 | Vterm not live: complete no-op | YES | `agent-repl-cmd-test-interrupt/noop-when-vterm-not-live` |

### 13. `agent-repl-update-pr` (interactive)

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | Sends the configured prompt string | YES | `agent-repl-cmd-test-update-pr/sends-prompt` |

### 14. `agent-repl-copy-reference` (interactive)

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | Puts file:line reference on kill ring | YES | `agent-repl-cmd-test-copy-reference/copies-to-kill-ring` |

### 15. Customization Variables (9 defcustom declarations)

| # | Edge Case | Test? | Test Name |
|---|-----------|-------|-----------|
| 1 | All 9 custom variables are non-empty strings | YES | `agent-repl-cmd-test-customization-defaults` |

### 16. Constants (`agent-repl--diff-scopes`, `agent-repl--diff-scope-labels`, `agent-repl--update-pr-diff-scopes`)

These are data constants tested indirectly through the macro expansion and resolve-change-spec tests.

---

## Coverage Statistics

- **Functions in commands.el**: 16 (including macro helper and macro)
- **Tests written**: 37
- **Edge cases identified**: 37
- **Edge cases covered**: 37
- **Coverage**: 100% of identified edge cases

## Notes

- Emacs was not available when this analysis was written; the tests still need a real run.
- Magit tests use mock objects since magit is a heavy dependency.
- The `run-at-time` call in `agent-repl-interrupt` is stubbed to avoid timer side effects.
- The `agent-repl--define-diff-commands` macro is tested through its generated output (35 interactive commands across 7 families x 5 scopes).
