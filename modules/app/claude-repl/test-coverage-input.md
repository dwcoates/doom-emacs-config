# Test Coverage Analysis: input.el

## Functions and Their Test Coverage

### Constants and Variables

| Symbol | Type | Covered | Notes |
|--------|------|---------|-------|
| `claude-repl-send-postfix` | defcustom | Indirect | Used via `claude-repl-send-with-postfix` |
| `claude-repl-paste-delay` | defvar | Not tested | Tested in old `bug9-paste-delay-configurable` (in test-claude-repl.el) |
| `claude-repl-header-line` | defface | Not tested | Visual only |
| `claude-repl--backspace-commands` | defconst | Tested | Via `slash-intercept-backspace` tests |
| `claude-repl--bracketed-paste-threshold` | defconst | Tested | Dedicated constant test + routing tests |
| `claude-repl-metaprompt-exempt-strings` | defvar | Tested | Via `skip-metaprompt-p` tests |
| `claude-repl-send-posthooks` | defvar | Tested | Via `run-send-posthooks` tests |

### Backspace and Editing

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-repl--slash-intercept-backspace` | YES | 4 tests | Slash mode redirect; empty buffer vterm forward; non-empty no-op; non-backspace command ignored | Leading whitespace in buffer (buffer-size=0 vs blank-p) |

### Input Mode

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-input-mode` | NO | - | - | Mode setup: header-line, face remap, visual-line, hooks added |

### Discard Functions

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-repl-discard-input` | YES | 1 test | Push history, clear buffer, evil-insert-state | When `claude-slash-input-mode` is active (should exit slash mode first); empty buffer discard |
| `claude-repl-discard-or-send-interrupt` | YES | 2 tests | Empty buffer sends C-c; non-empty discards | Whitespace-only buffer (string-blank-p returns t) |

### Arrow Key Forwarding

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-repl--send-vterm-key` | YES | 1 test | Forwards key to vterm | When vterm buffer is dead (macro returns nil) |
| `claude-repl--send-up-arrow` | YES | 1 test | Sends `<up>` | - |
| `claude-repl--send-down-arrow` | YES | 1 test | Sends `<down>` | - |

### Vterm History Scrolling

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-repl--send-vterm-down` / `claude-repl-scroll-down` | YES | 1 test | Calls `vterm-send-down` | Dead vterm buffer |
| `claude-repl--send-vterm-up` / `claude-repl-scroll-up` | YES | 1 test | Calls `vterm-send-up` | Dead vterm buffer |

### Vterm Output Scrolling

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-repl--scroll-vterm-output` | NO | - | - | Scrolls by 3 lines; no vterm window (returns nil); window visible |
| `claude-repl-scroll-output-up` | NO | - | - | Calls `scroll-down` (not `scroll-up` -- counterintuitive naming) |
| `claude-repl-scroll-output-down` | NO | - | - | Calls `scroll-up` |

### Single-Character Confirmations

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-repl-send-char` | YES | 1 test | Sends char + return | Dead vterm buffer |
| `claude-repl--send-y` (macro-generated) | NO | - | - | Calls `send-char "y"` |
| `claude-repl--send-n` (macro-generated) | NO | - | - | Calls `send-char "n"` |
| `claude-repl--define-send-char-command` | Indirect | via `send-char` | - | Macro expansion correctness |

### Digit Key Handlers

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-repl--input-send-digit-char` | NO | - | - | Extracts digit from `last-command-event`; sends via `send-char` |
| `claude-repl--insert-digit-or-passthrough` | NO | - | - | Empty buffer -> passthrough; non-empty -> self-insert |

### Metaprompt Logic

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-repl--skip-metaprompt-p` | YES | 4 tests | Exempt strings; bare numerals; trailing whitespace; normal input | Leading whitespace; empty string; mixed numeral+whitespace |
| `claude-repl--should-prepend-metaprompt-p` | YES | 5 tests | Full condition matrix; force flag; skip-permissions off; nil command-prefix; exempt strings; bare numerals | Empty string command-prefix (truthy but empty) |
| `claude-repl--prepare-input` | YES | 5 tests | No prefix when disabled; counter alignment; force flag; nil counter; exempt input | Empty raw input |

### Posthooks

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-repl--posthook-reset-prefix-counter` | YES | 1 test | Resets counter to 1 | - |
| `claude-repl--run-send-posthooks` | YES | 3 tests | Matching pattern; no match; trailing whitespace | Multiple hooks matching same input; empty input |

### Send Pipeline

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-repl--send-input-direct` | YES | 1 test | Sends string, return, refresh in order | - |
| `claude-repl--send-input-bracketed` | YES | 1 test | Uses paste flag, defers return | - |
| `claude-repl--send-input-to-vterm` | YES | 4 tests | Direct mode (<=200); paste mode (>200); exact threshold; empty string | - |
| `claude-repl--mark-ws-thinking` | YES | 1 test | Sets :thinking state | - |
| `claude-repl--increment-prefix-counter` | YES | 2 tests | From nil (0->1); from existing value | - |
| `claude-repl--pin-owning-workspace` | YES | 2 tests | Sets buffer-local; nil buffer no-op | - |
| `claude-repl--do-send` | YES | 4 tests | Increments counter; sets thinking; pins workspace; runs posthooks | Dead vterm buffer (ws-get returns nil buffer) |
| `claude-repl--commit-input-buffer` | YES | 4 tests | With clear; without clear; nil buffer; dead buffer | - |
| `claude-repl--read-input-buffer` | YES | 3 tests | Live buffer; no buffer; dead buffer | - |

### Send Entry Points

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-repl--send` | YES | 4 tests | No workspace error; reads from buffer; explicit prompt; nil raw no-op | Force-metaprompt path; dead vterm buffer |
| `claude-repl-send-and-hide` | NO | - | - | Calls send then hide-panels |
| `claude-repl-send-with-metaprompt` | NO | - | - | Calls send with force=t |
| `claude-repl-send-with-postfix` | NO | - | - | Appends postfix then sends |
| `claude-repl--append-to-input-buffer` | YES | 2 tests | Appends text; no buffer registered | Dead buffer for workspace |

### Deferred Action Helpers

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-repl--run-deferred-action` | YES | 2 tests | Live buffer runs action; dead buffer skips | - |
| `claude-repl--vterm-deferred-action` | YES | 1 test | Schedules via run-at-time with correct delay | Dead buffer at schedule time |
| `claude-repl--bracketed-finalize` | NO | - | - | Sends return + refreshes vterm |
| `claude-repl--bracketed-send-return` | NO | - | - | Sends return + schedules finalize |

### Slash Pass-Through Mode

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-slash-input-mode` | YES | Indirect | Enabled/disabled via other tests | - |
| `claude-repl--exit-slash-mode` | YES | 1 test | Clears stack, disables mode | Already disabled (idempotent) |
| `claude-repl--slash-vterm-send` | YES | Indirect | Via slash-send-and-push | Dead vterm |
| `claude-repl--slash-send-and-push` | YES | 1 test | Sends char, pushes to stack | - |
| `claude-repl--slash-forward-char` | NO | - | - | Uses `last-command-event` to get char |
| `claude-repl--slash-backspace` | YES | 2 tests | Pops stack + sends backspace; exits when empty | Already empty stack (edge) |
| `claude-repl--slash-return` | YES | 1 test | Sends return, exits mode | - |
| `claude-repl--slash-tab` | YES | 1 test | Sends tab, pushes to stack | - |
| `claude-repl--passthrough-start` | YES | 2 tests | Empty buffer enters mode; non-empty inserts | - |
| `claude-repl--slash-start` | NO | - | - | Thin wrapper over passthrough-start with "/" |

## Coverage Summary

| Category | Functions | Tested | Coverage |
|----------|-----------|--------|----------|
| Backspace/editing | 1 | 1 | 100% |
| Mode setup | 1 | 0 | 0% |
| Discard | 2 | 2 | 100% |
| Arrow forwarding | 3 | 3 | 100% |
| History scrolling | 2 | 2 | 100% |
| Output scrolling | 3 | 0 | 0% |
| Send char | 4 | 1 | 25% |
| Digit handlers | 2 | 0 | 0% |
| Metaprompt logic | 3 | 3 | 100% |
| Posthooks | 2 | 2 | 100% |
| Send pipeline | 8 | 8 | 100% |
| Send entry points | 5 | 3 | 60% |
| Deferred actions | 4 | 2 | 50% |
| Slash mode | 10 | 7 | 70% |
| **TOTAL** | **50** | **34** | **68%** |

## Priority Gaps (recommended new tests)

### High Priority
1. **`claude-repl-discard-input` with active slash mode** -- Verifies the `when claude-slash-input-mode (exit-slash-mode)` branch.
2. **`claude-repl-discard-or-send-interrupt` with whitespace-only buffer** -- `string-blank-p` returns t for whitespace, so this sends C-c rather than discarding.
3. **`claude-repl--scroll-vterm-output`** -- Tests the window-lookup + `with-selected-window` + `funcall` pipeline. Important because a nil window should be a no-op.
4. **`claude-repl-send-with-postfix`** -- Verifies the append+send composition.
5. **`claude-repl-send-with-metaprompt`** -- Verifies the force=t delegation.
6. **`claude-repl--skip-metaprompt-p` with empty string** -- Edge case: `""` does not match numerals or exemptions, so it should return nil.

### Medium Priority
7. **`claude-repl--slash-forward-char`** -- Depends on `last-command-event`; can be tested by let-binding it.
8. **`claude-repl--input-send-digit-char`** -- Depends on `last-command-event` for digit extraction.
9. **`claude-repl--insert-digit-or-passthrough`** -- Both branches (empty vs non-empty buffer).
10. **`claude-repl--bracketed-finalize`** -- Unit test for return+refresh.
11. **`claude-repl--bracketed-send-return`** -- Unit test for return+schedule-finalize.

### Low Priority
12. **`claude-input-mode`** -- Mode setup is mostly declarative; testing hooks are added correctly.
13. **`claude-repl-send-and-hide`** -- Thin composition of send + hide-panels.
14. **`claude-repl--slash-start`** -- Trivial wrapper.
15. **`claude-repl--send-y` / `claude-repl--send-n`** -- Macro-generated, trivially delegate to `send-char`.
