# Test Coverage Analysis: input.el

## Functions and Their Test Coverage

### Constants and Variables

| Symbol | Type | Covered | Notes |
|--------|------|---------|-------|
| `agent-repl-send-postfix` | defcustom | Indirect | Used via `agent-repl-send-with-postfix` |
| `agent-repl-paste-delay` | defvar | Not tested | Tested in old `bug9-paste-delay-configurable` (in test-agent-repl.el) |
| `agent-repl-header-line` | defface | Not tested | Visual only |
| `agent-repl--backspace-commands` | defconst | Tested | Via `slash-intercept-backspace` tests |
| `agent-repl--bracketed-paste-threshold` | defconst | Tested | Dedicated constant test + routing tests |
| `agent-repl-metaprompt-exempt-strings` | defvar | Tested | Via `skip-metaprompt-p` tests |
| `agent-repl-send-posthooks` | defvar | Tested | Via `run-send-posthooks` tests |

### Backspace and Editing

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `agent-repl--slash-intercept-backspace` | YES | 4 tests | Slash mode redirect; empty buffer vterm forward; non-empty no-op; non-backspace command ignored | Leading whitespace in buffer (buffer-size=0 vs blank-p) |

### Input Mode

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-input-mode` | NO | - | - | Mode setup: header-line, face remap, visual-line, hooks added |

### Discard Functions

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `agent-repl-discard-input` | YES | 1 test | Push history, clear buffer, evil-insert-state | When `claude-slash-input-mode` is active (should exit slash mode first); empty buffer discard |
| `agent-repl-discard-or-send-interrupt` | YES | 2 tests | Empty buffer sends C-c; non-empty discards | Whitespace-only buffer (string-blank-p returns t) |

### Arrow Key Forwarding

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `agent-repl--send-vterm-key` | YES | 1 test | Forwards key to vterm | When vterm buffer is dead (macro returns nil) |
| `agent-repl--send-up-arrow` | YES | 1 test | Sends `<up>` | - |
| `agent-repl--send-down-arrow` | YES | 1 test | Sends `<down>` | - |

### Vterm History Scrolling

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `agent-repl--send-vterm-down` / `agent-repl-scroll-down` | YES | 1 test | Calls `vterm-send-down` | Dead vterm buffer |
| `agent-repl--send-vterm-up` / `agent-repl-scroll-up` | YES | 1 test | Calls `vterm-send-up` | Dead vterm buffer |

### Vterm Output Scrolling

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `agent-repl--scroll-vterm-output` | NO | - | - | Scrolls by 3 lines; no vterm window (returns nil); window visible |
| `agent-repl-scroll-output-up` | NO | - | - | Calls `scroll-down` (not `scroll-up` -- counterintuitive naming) |
| `agent-repl-scroll-output-down` | NO | - | - | Calls `scroll-up` |

### Single-Character Confirmations

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `agent-repl-send-char` | YES | 1 test | Sends char + return | Dead vterm buffer |
| `agent-repl--send-y` (macro-generated) | NO | - | - | Calls `send-char "y"` |
| `agent-repl--send-n` (macro-generated) | NO | - | - | Calls `send-char "n"` |
| `agent-repl--define-send-char-command` | Indirect | via `send-char` | - | Macro expansion correctness |

### Digit Key Handlers

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `agent-repl--input-send-digit-char` | NO | - | - | Extracts digit from `last-command-event`; sends via `send-char` |
| `agent-repl--insert-digit-or-passthrough` | NO | - | - | Empty buffer -> passthrough; non-empty -> self-insert |

### Metaprompt Logic

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `agent-repl--skip-metaprompt-p` | YES | 4 tests | Exempt strings; bare numerals; trailing whitespace; normal input | Leading whitespace; empty string; mixed numeral+whitespace |
| `agent-repl--should-prepend-metaprompt-p` | YES | 5 tests | Full condition matrix; force flag; skip-permissions off; nil command-prefix; exempt strings; bare numerals | Empty string command-prefix (truthy but empty) |
| `agent-repl--prepare-input` | YES | 5 tests | No prefix when disabled; counter alignment; force flag; nil counter; exempt input | Empty raw input |

### Posthooks

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `agent-repl--posthook-reset-prefix-counter` | YES | 1 test | Resets counter to 1 | - |
| `agent-repl--run-send-posthooks` | YES | 3 tests | Matching pattern; no match; trailing whitespace | Multiple hooks matching same input; empty input |

### Send Pipeline

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `agent-repl--send-input-direct` | YES | 1 test | Sends string, return, refresh in order | - |
| `agent-repl--send-input-bracketed` | YES | 1 test | Uses paste flag, defers return | - |
| `agent-repl--send-input-to-vterm` | YES | 4 tests | Direct mode (<=200); paste mode (>200); exact threshold; empty string | - |
| `agent-repl--mark-ws-thinking` | YES | 1 test | Sets :thinking state | - |
| `agent-repl--increment-prefix-counter` | YES | 2 tests | From nil (0->1); from existing value | - |
| `agent-repl--pin-owning-workspace` | YES | 2 tests | Sets buffer-local; nil buffer no-op | - |
| `agent-repl--do-send` | YES | 4 tests | Increments counter; sets thinking; pins workspace; runs posthooks | Dead vterm buffer (ws-get returns nil buffer) |
| `agent-repl--commit-input-buffer` | YES | 4 tests | With clear; without clear; nil buffer; dead buffer | - |
| `agent-repl--read-input-buffer` | YES | 3 tests | Live buffer; no buffer; dead buffer | - |

### Send Entry Points

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `agent-repl--send` | YES | 4 tests | No workspace error; reads from buffer; explicit prompt; nil raw no-op | Force-metaprompt path; dead vterm buffer |
| `agent-repl-send-and-hide` | NO | - | - | Calls send then hide-panels |
| `agent-repl-send-with-metaprompt` | NO | - | - | Calls send with force=t |
| `agent-repl-send-with-postfix` | NO | - | - | Appends postfix then sends |
| `agent-repl--append-to-input-buffer` | YES | 2 tests | Appends text; no buffer registered | Dead buffer for workspace |

### Deferred Action Helpers

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `agent-repl--run-deferred-action` | YES | 2 tests | Live buffer runs action; dead buffer skips | - |
| `agent-repl--vterm-deferred-action` | YES | 1 test | Schedules via run-at-time with correct delay | Dead buffer at schedule time |
| `agent-repl--bracketed-finalize` | NO | - | - | Sends return + refreshes vterm |
| `agent-repl--bracketed-send-return` | NO | - | - | Sends return + schedules finalize |

### Slash Pass-Through Mode

| Function | Tested | Tests | Edge Cases Covered | Edge Cases Missing |
|----------|--------|-------|--------------------|--------------------|
| `claude-slash-input-mode` | YES | Indirect | Enabled/disabled via other tests | - |
| `agent-repl--exit-slash-mode` | YES | 1 test | Clears stack, disables mode | Already disabled (idempotent) |
| `agent-repl--slash-vterm-send` | YES | Indirect | Via slash-send-and-push | Dead vterm |
| `agent-repl--slash-send-and-push` | YES | 1 test | Sends char, pushes to stack | - |
| `agent-repl--slash-forward-char` | NO | - | - | Uses `last-command-event` to get char |
| `agent-repl--slash-backspace` | YES | 2 tests | Pops stack + sends backspace; exits when empty | Already empty stack (edge) |
| `agent-repl--slash-return` | YES | 1 test | Sends return, exits mode | - |
| `agent-repl--slash-tab` | YES | 1 test | Sends tab, pushes to stack | - |
| `agent-repl--passthrough-start` | YES | 2 tests | Empty buffer enters mode; non-empty inserts | - |
| `agent-repl--slash-start` | NO | - | - | Thin wrapper over passthrough-start with "/" |

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
1. **`agent-repl-discard-input` with active slash mode** -- Verifies the `when claude-slash-input-mode (exit-slash-mode)` branch.
2. **`agent-repl-discard-or-send-interrupt` with whitespace-only buffer** -- `string-blank-p` returns t for whitespace, so this sends C-c rather than discarding.
3. **`agent-repl--scroll-vterm-output`** -- Tests the window-lookup + `with-selected-window` + `funcall` pipeline. Important because a nil window should be a no-op.
4. **`agent-repl-send-with-postfix`** -- Verifies the append+send composition.
5. **`agent-repl-send-with-metaprompt`** -- Verifies the force=t delegation.
6. **`agent-repl--skip-metaprompt-p` with empty string** -- Edge case: `""` does not match numerals or exemptions, so it should return nil.

### Medium Priority
7. **`agent-repl--slash-forward-char`** -- Depends on `last-command-event`; can be tested by let-binding it.
8. **`agent-repl--input-send-digit-char`** -- Depends on `last-command-event` for digit extraction.
9. **`agent-repl--insert-digit-or-passthrough`** -- Both branches (empty vs non-empty buffer).
10. **`agent-repl--bracketed-finalize`** -- Unit test for return+refresh.
11. **`agent-repl--bracketed-send-return`** -- Unit test for return+schedule-finalize.

### Low Priority
12. **`claude-input-mode`** -- Mode setup is mostly declarative; testing hooks are added correctly.
13. **`agent-repl-send-and-hide`** -- Thin composition of send + hide-panels.
14. **`agent-repl--slash-start`** -- Trivial wrapper.
15. **`agent-repl--send-y` / `agent-repl--send-n`** -- Macro-generated, trivially delegate to `send-char`.
