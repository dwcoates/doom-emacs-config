# Fallback and Default Logic Report: `commands.el`

**Total instances: 25**

---

## Section 1: `defcustom` Default Values (9 instances)

### Instance 1 -- `claude-repl-branch-diff-spec`
- **Line(s):** 7-11
- **Default:** `"changes in current branch (git diff $(git merge-base HEAD origin/master))"`
- **Consequence:** All branch-scope diff commands reference `origin/master`. If the user's remote default branch is `origin/main`, the prompt will reference the wrong base.

### Instance 2 -- `claude-repl-explain-diff-prompt`
- **Line(s):** 13-17
- **Default:** `"please explain the changes"`

### Instance 3 -- `claude-repl-update-pr-diff-prompt`
- **Line(s):** 19-23
- **Default:** `"please update the PR description"`

### Instance 4 -- `claude-repl-update-pr-prompt`
- **Line(s):** 25-29
- **Default:** `"please update the PR description for the PR corresponding to our branch"`

### Instance 5 -- `claude-repl-run-tests-prompt`
- **Line(s):** 31-35
- **Default:** `"please run tests, and summarize the issues found and probable causes"`

### Instance 6 -- `claude-repl-run-lint-prompt`
- **Line(s):** 37-41
- **Default:** `"please run lint, and address any issues found"`

### Instance 7 -- `claude-repl-run-all-prompt`
- **Line(s):** 43-47
- **Default:** `"please run lint and tests, and address any issues found for both"`

### Instance 8 -- `claude-repl-test-quality-prompt`
- **Line(s):** 49-53
- **Default:** ~900 characters of opinionated AAA testing instructions.

### Instance 9 -- `claude-repl-test-coverage-prompt`
- **Line(s):** 55-58
- **Default:** ~550 characters of two-phase edge-case/test-plan workflow.

---

## Section 2: `or` Expressions

### Instance 10 -- Workspace parameter fallback
- **Line:** 66
- **Code:** `(let ((ws (or ws (+workspace-current-name))))`
- **Chain:** explicit arg -> current workspace -> hard error (line 67)

---

## Section 3: `&optional` Parameters

### Instance 11 -- `claude-repl--initialize-claude` optional `ws`
- **Location:** panels.el
- **Three code paths:** explicit ws, inferred workspace, error signal

---

## Section 4: Guards Handling Missing Values

### Instance 12 -- Nil file guard in `claude-repl--buffer-relative-path`
- **Line(s):** 89-91
- **Falls back TO:** `user-error` signal

### Instance 13 -- Dead vterm buffer guard in `claude-repl--enter-insert-mode`
- **Line(s):** 274-279
- **Falls back TO:** No-op (log and skip)

### Instance 14 -- Dead vterm guard in `claude-repl-interrupt`
- **Line(s):** 289-293
- **Falls back TO:** No-op (log and skip, no user feedback)

### Instance 15 -- Nil workspace list guard in `claude-repl-nuke-all-workspaces`
- **Line(s):** 355
- **Falls back TO:** `user-error` signal

---

## Section 5: `condition-case`

### Instance 16 -- `kill-session` error swallowing in nuke
- **Line(s):** 319-321
- **Code:** `(condition-case err (claude-repl--kill-session ws) (error ...))`
- **Consequence:** Error is caught, logged, and discarded. Nuke continues best-effort.

---

## Section 6: `cond` with Catch-All

### Instance 17 -- `claude-repl--resolve-change-spec` three-way dispatch
- **Line(s):** 183-192
- **Priority:** override > branch-spec sentinel > raw default-spec

---

## Section 7: Mode-Based Dispatch

### Instance 18 -- `claude-repl--context-reference` magit vs file
- **Line(s):** 131-137
- **Falls back TO:** `claude-repl--format-file-ref` for non-magit contexts

### Instance 18a -- `claude-repl--format-file-ref` region vs point
- **Line(s):** 100-107
- **Falls back TO:** Single-line `file:line` reference when no region is active

---

## Section 8: Sentinel/Constant Values

### Instance 19 -- `:use-branch-diff-spec` sentinel
- **Line:** 154
- **Consequence:** The `branch` scope uniquely defers to a user-customizable variable.

### Instance 20 -- Missing `branch` scope in update-pr-diff scopes
- **Line(s):** 167-173
- **Consequence:** `branch` intentionally absent; falls through to default.

### Instance 21 -- `bound-and-true-p persp-mode` guard in nuke
- **Line(s):** 322-323

### Instance 22 -- Hardcoded `0.25` second delay
- **Line:** 291

### Instance 23 -- Hardcoded `:prefix-counter 0` initialization
- **Line:** 74

---

## Section 9: `when-let` Nil-Guarded Bindings

### Instance 24 -- Git process cleanup guard
- **Line(s):** 315-318
- **Two-level guard:** `when-let` for existence, inner `when` for liveness

---

## Section 10: Macro `&optional`

### Instance 25 -- `scope-overrides` in `claude-repl--define-diff-commands`
- **Line:** 209
- **Consequence:** 6 of 7 command families use standard scopes; `update-pr-diff` gets its own.

---

## Summary

| Category | Count |
|---|---|
| `defcustom` default values | 9 |
| `or` expressions with fallback | 1 |
| `&optional` parameters with nil-handling | 2 |
| `unless`/`when` nil guards | 3 |
| `if` behavioral dispatch | 3 |
| `condition-case` error swallowing | 1 |
| `cond` with catch-all `t` branch | 1 |
| `when-let` nil-guarded bindings | 1 |
| Hardcoded constant defaults | 3 |
| Intentional alist omission | 1 |
| **Total** | **25** |
