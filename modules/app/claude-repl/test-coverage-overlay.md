# Test Coverage Analysis: overlay.el

## Function-by-function edge case enumeration and coverage status

### `claude-repl--hide-overlay-region`

| # | Edge case | Test |
|---|-----------|------|
| 1 | Empty buffer (point-max = 1) returns nil | `claude-repl-test-overlay-region-empty-buffer` |
| 2 | Single character buffer returns valid region | `claude-repl-test-overlay-region-single-char` |
| 3 | Fewer lines than `hide-overlay-line-count` (region covers from beginning) | `claude-repl-test-overlay-region-fewer-lines-than-count` |
| 4 | Exactly `hide-overlay-line-count` lines | `claude-repl-test-overlay-region-exact-line-count` |
| 5 | More lines than `hide-overlay-line-count` (region starts mid-buffer) | `claude-repl-test-overlay-region-more-lines-than-count` |
| 6 | Buffer with no trailing newline | Covered implicitly by single-char test |
| 7 | Narrowed buffer (start != 1) | **NOT TESTED** -- low priority, function uses point-max/save-excursion which respect narrowing |

### `claude-repl--create-hide-overlay`

| # | Edge case | Test |
|---|-----------|------|
| 1 | `claude-repl-hide-input-box` is nil -- no overlay created | `claude-repl-test-create-overlay-respects-flag` |
| 2 | `claude-repl-hide-input-box` is t with content -- overlay created | `claude-repl-test-create-overlay-creates-when-enabled` |
| 3 | `claude-repl-hide-input-box` is t but buffer empty -- no overlay | `claude-repl-test-create-overlay-empty-buffer` |
| 4 | Overlay has `display` property set to "" | `claude-repl-test-create-overlay-has-display-property` |
| 5 | Overlay has `evaporate` property set to t | `claude-repl-test-create-overlay-has-evaporate-property` |
| 6 | Overlay is created with front-advance=t (4th arg to make-overlay) | **NOT TESTED** -- would require inspecting overlay internals not exposed by standard API |
| 7 | Called twice without delete -- leaks first overlay | **NOT TESTED** -- not a crash risk since update-hide-overlay always deletes first, but could note as defensive gap |

### `claude-repl--delete-hide-overlay`

| # | Edge case | Test |
|---|-----------|------|
| 1 | `claude-repl-hide-overlay` is nil -- no-op | `claude-repl-test-delete-overlay-noop-when-nil` |
| 2 | Overlay exists and is attached -- deletes and nils | `claude-repl-test-delete-overlay-deletes-existing` |
| 3 | Overlay exists but already detached (buffer killed) -- nils variable | `claude-repl-test-delete-overlay-handles-already-deleted` |
| 4 | Variable is non-nil but not an overlay object | **NOT TESTED** -- defensive edge case, would error on `overlay-buffer` |

### `claude-repl--resolve-overlay-target-buffer`

| # | Edge case | Test |
|---|-----------|------|
| 1 | Current buffer is a claude buffer -- returns current-buffer | `claude-repl-test-resolve-target-in-claude-buffer` |
| 2 | Non-claude buffer, workspace has vterm-buffer | `claude-repl-test-resolve-target-non-claude-with-ws` |
| 3 | Non-claude buffer, no workspace (nil) | `claude-repl-test-resolve-target-non-claude-no-ws` |
| 4 | Non-claude buffer, workspace exists but no :vterm-buffer | `claude-repl-test-resolve-target-non-claude-no-vterm` |
| 5 | Non-claude buffer, workspace vterm-buffer is a killed buffer | **NOT TESTED** -- returns the killed buffer; caller (update-hide-overlay) checks buffer-live-p |

### `claude-repl--update-hide-overlay`

| # | Edge case | Test |
|---|-----------|------|
| 1 | Resolve returns nil -- no-op | `claude-repl-test-update-overlay-nil-buffer` |
| 2 | Resolve returns a killed buffer -- no-op | `claude-repl-test-update-overlay-dead-buffer` |
| 3 | Resolve returns live buffer -- deletes and recreates overlay | `claude-repl-test-update-overlay-recreates` |
| 4 | Previous overlay is detached before new one created | Covered by `update-overlay-recreates` (checks old overlay-buffer is nil) |

### `claude-repl--enable-hide-overlay`

| # | Edge case | Test |
|---|-----------|------|
| 1 | Refcount 0 -> 1: advice added | `claude-repl-test-enable-overlay-from-zero` |
| 2 | Refcount > 0: advice NOT added again | `claude-repl-test-enable-overlay-from-nonzero` |
| 3 | Multiple enables accumulate refcount correctly | `claude-repl-test-overlay-refcount` |

### `claude-repl--vterm-redraw-advice`

| # | Edge case | Test |
|---|-----------|------|
| 1 | Reentrancy guard active -- skips entirely | `claude-repl-test-redraw-advice-reentrancy-guard` |
| 2 | Non-claude buffer -- skips update | `claude-repl-test-redraw-advice-skips-non-claude` |
| 3 | Claude buffer, no reentrancy -- calls update | `claude-repl-test-redraw-advice-reentrancy-guard` (second half) |
| 4 | Extra arguments passed (as :after advice) | `claude-repl-test-redraw-advice-accepts-args` |
| 5 | Sets reentrancy flag during execution | `claude-repl-test-redraw-advice-sets-reentrancy-flag` |
| 6 | Reentrancy flag restored after execution | `claude-repl-test-redraw-advice-sets-reentrancy-flag` (checks post-return) |

### `claude-repl--disable-hide-overlay`

| # | Edge case | Test |
|---|-----------|------|
| 1 | Refcount 2 -> 1: advice NOT removed | `claude-repl-test-overlay-refcount` |
| 2 | Refcount 1 -> 0: advice removed | `claude-repl-test-overlay-refcount` |
| 3 | Refcount already 0: clamps to 0, does not go negative | `claude-repl-test-disable-overlay-clamps-at-zero` |
| 4 | Always calls `delete-hide-overlay` | `claude-repl-test-disable-overlay-cleans-up-overlay` |

### `claude-repl-toggle-hide-input-box`

| # | Edge case | Test |
|---|-----------|------|
| 1 | nil -> t | `claude-repl-test-toggle-enables-when-disabled` |
| 2 | t -> nil | `claude-repl-test-toggle-disables-when-enabled` |
| 3 | Calls `update-hide-overlay` each time | `claude-repl-test-toggle-calls-update` |
| 4 | Is interactive | **NOT TESTED** -- low priority, trivially correct from `(interactive)` |
| 5 | Displays message | **NOT TESTED** -- could capture with `cl-letf` on `message` |

### Constants and variables

| # | Edge case | Test |
|---|-----------|------|
| 1 | `claude-repl--hide-overlay-line-count` is a positive integer | `claude-repl-test-hide-overlay-line-count-is-positive` |
| 2 | `claude-repl--in-redraw-advice` is bound | `claude-repl-test-in-redraw-advice-declared` |

## Summary

| Category | Tested | Not tested | Coverage |
|----------|--------|------------|----------|
| `claude-repl--hide-overlay-region` | 6 | 1 | High |
| `claude-repl--create-hide-overlay` | 5 | 2 | High |
| `claude-repl--delete-hide-overlay` | 3 | 1 | High |
| `claude-repl--resolve-overlay-target-buffer` | 4 | 1 | High |
| `claude-repl--update-hide-overlay` | 3 | 0 | Complete |
| `claude-repl--enable-hide-overlay` | 3 | 0 | Complete |
| `claude-repl--vterm-redraw-advice` | 5 | 0 | Complete |
| `claude-repl--disable-hide-overlay` | 4 | 0 | Complete |
| `claude-repl-toggle-hide-input-box` | 3 | 2 | High |
| Constants/variables | 2 | 0 | Complete |
| **Total** | **38** | **7** | **84%** |

Untested items are all low-priority defensive or cosmetic edge cases.
