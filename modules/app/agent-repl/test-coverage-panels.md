# Test Coverage Analysis: panels.el

## Summary

This document enumerates every function in `panels.el`, lists edge cases, and tracks which are covered by `test-panels.el`.

---

## Panel visibility predicates

### `agent-repl--ws-buffer-visible-p (key)`
Generic predicate checking if a workspace buffer stored at KEY is visible.

| Edge Case | Covered | Test |
|-----------|---------|------|
| Buffer exists, is live, and is displayed in a window | Yes | `input-visible-p-with-visible-buffer` (indirect) |
| Buffer exists but has been killed (dead buffer) | Yes | `input-visible-p-dead-buffer` (indirect) |
| KEY has no buffer stored (nil) | Yes | `input-visible-p-no-buffer` (indirect) |
| Buffer is live but not displayed in any window | No | Requires multi-window setup |

### `agent-repl--input-visible-p ()`
Thin wrapper around `ws-buffer-visible-p` for `:input-buffer`.

| Edge Case | Covered | Test |
|-----------|---------|------|
| Input buffer visible | Yes | `input-visible-p-with-visible-buffer` |
| Input buffer nil | Yes | `input-visible-p-no-buffer` |
| Input buffer dead | Yes | `input-visible-p-dead-buffer` |

### `agent-repl--vterm-visible-p ()`
Thin wrapper around `ws-buffer-visible-p` for `:vterm-buffer`.

| Edge Case | Covered | Test |
|-----------|---------|------|
| Vterm buffer nil | Yes | `vterm-visible-p-no-buffer` |
| Vterm buffer visible | No | Requires multi-window setup with vterm-mode buffer |
| Vterm buffer dead | No | Similar to input-visible-p-dead-buffer pattern |

### `agent-repl--panels-visible-p ()`
Returns t only when both panels are visible.

| Edge Case | Covered | Test |
|-----------|---------|------|
| Neither panel exists | Yes | `panels-visible-p-both-nil` |
| Only input visible | No | Requires multi-window setup |
| Only vterm visible | No | Requires multi-window setup |
| Both visible | No | Requires multi-window setup |

---

## Panel display and hide

### `agent-repl--safe-buffer-name (b)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| nil input | Yes | `safe-buffer-name-nil` |
| Live buffer | Yes | `safe-buffer-name-live-buffer` |
| Dead buffer (killed) | No | Would error -- `buffer-name` on dead buffer |

### `agent-repl--close-buffer-window (buf)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Buffer has no window | Yes | `close-buffer-window-no-window` |
| Buffer is nil | No | Would need nil guard testing |
| Buffer has a window (successful delete) | No | Requires multi-window setup |
| Delete-window errors (last window) | No | `ignore-errors` handles this |

### `agent-repl--close-buffer-windows (&rest bufs)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| All nil arguments | Yes | `close-buffer-windows-nil-args` |
| Dead buffer in arguments | Yes | `close-buffer-windows-dead-buffer` |
| Mix of live and nil buffers | No | |
| Buffer with visible window | No | Requires multi-window setup |

### `agent-repl--configure-vterm-window (win)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Sets all three properties correctly | Yes | `configure-vterm-window` |

### `agent-repl--show-panels ()`
Complex function: clears the main area, fills the frame with the panels, sets buffers, configures windows.

| Edge Case | Covered | Test |
|-----------|---------|------|
| Fills frame, clearing work windows (fullscreen) | Yes | `show-panels-fills-frame-clearing-work-windows` |
| Saves pre-panel layout as `:fullscreen-config` | Yes | `show-panels-saves-pre-panel-config` |
| Does not overwrite an already-saved config | Yes | `show-panels-does-not-overwrite-saved-config` |
| Workspace has no vterm buffer | No | |
| Workspace has no input buffer | No | |
| Docstring describes the fullscreen layout | Yes | `show-panels-docstring` |
| Sets no-delete-other-windows on both panels | Yes | `show-panels-sets-no-delete-other-windows` |

### `agent-repl--focus-input-panel ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Input buffer exists and has window | No | Requires multi-window setup |
| Input buffer nil | No | `when-let` guards this |
| Input buffer exists but no window | No | |

### `agent-repl--show-panels-and-focus ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Delegates to show-panels + focus-input-panel | No | Composition of tested functions |

---

## Vterm refresh

### `agent-repl--vterm-redraw ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| vterm--term is nil | No | Requires vterm environment |
| vterm--term is set | No | Requires real vterm |

### `agent-repl--do-refresh ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Normal refresh | No | Requires vterm-mode buffer |

### `agent-repl--fix-vterm-scroll (buf)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Vterm window different from selected | No | Requires multi-window |
| Vterm window is selected window (no-op) | No | |
| No vterm window | No | |

### `agent-repl--resolve-vterm-buffer ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Current buffer is not vterm-mode, workspace has vterm | Yes | `resolve-vterm-buffer-from-non-vterm` |
| No active workspace | Yes | `resolve-vterm-buffer-no-workspace` |
| Current buffer IS vterm-mode | No | Requires vterm-mode buffer |

### `agent-repl--refresh-vterm ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| resolve returns nil | No | when-let guards |
| Buffer dead | No | buffer-live-p guards |
| Buffer live but not vterm-mode | No | Major mode check guards |
| Full path: live vterm buffer | No | Requires vterm environment |

---

## Workspace switch

### `agent-repl--mark-viewed (ws)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| :inactive -> :done + :viewed | Yes | `mark-viewed-inactive-to-done` |
| :done -> :viewed (status unchanged) | Yes | `mark-viewed-done-sets-viewed` |
| :thinking -> no-op | Yes | `mark-viewed-thinking-noop` |
| nil status -> no-op | Yes | `mark-viewed-nil-status-noop` |
| :permission -> no-op | Yes | `mark-viewed-permission-noop` |

### `agent-repl--drain-pending-show-panels (ws)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Flag is set -> calls agent-repl, clears flag | Yes | `drain-pending-when-set` |
| Flag is nil -> no-op | Yes | `drain-pending-when-not-set` |

### `agent-repl--on-workspace-switch ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Normal workspace switch | No | Requires workspace/persp infrastructure |
| ws is nil | No | `when ws` guards mark-viewed |

### `agent-repl--non-claude-panel-window-p (w)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Window shows non-Claude buffer | Yes | `non-claude-panel-window-p` |
| Window shows Claude vterm buffer | No | Requires Claude buffer displayed |
| Window shows Claude input buffer | No | Requires Claude buffer displayed |

### `agent-repl--redirect-from-claude-before-save ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Selected window is Claude buffer, other windows exist | No | Requires multi-window |
| Selected window is non-Claude buffer (no-op) | No | |
| Claude is the only window (fullscreen, skip redirect) | No | |

### `agent-repl--before-persp-deactivate (&rest _)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Normal deactivation | No | Requires persp-mode |

### `agent-repl--after-persp-activated (&rest _)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Normal activation (schedules timer) | No | Timer-based |

### `agent-repl--hide-panels ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Both buffers exist | No | Delegates to close-buffer-windows |
| Neither buffer exists | No | |

---

## Window synchronization

### `agent-repl--extract-panel-hex (name)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Vterm buffer name | Yes | `extract-hex-from-vterm` |
| Input buffer name | Yes | `extract-hex-from-input` |
| Non-Claude buffer name | Yes | `extract-hex-non-claude` |

### `agent-repl--partner-buffer-name (name hex)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Vterm -> input partner | Yes | `partner-of-vterm` |
| Input -> vterm partner | Yes | `partner-of-input` |

### `agent-repl--orphaned-panel-p (name)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Vterm with no partner visible | Yes | `orphaned-vterm-p` |
| Input with no partner visible, no loading placeholder | Yes | `orphaned-input-p` |
| Single-window frame (one-window-p) | Yes | `orphaned-vterm-one-window` |
| Loading placeholder exists (input not orphaned) | Yes | `orphaned-input-with-loading` |
| Partner IS visible (not orphaned) | Yes | `orphaned-vterm-partner-visible`, `orphaned-input-partner-visible` |
| Non-Claude buffer name | Yes | `orphaned-vterm-p`, `orphaned-input-p` |

### `agent-repl--sync-panels ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Window with orphaned panel | No | Requires multi-window |
| No orphaned windows | No | |

### `agent-repl--refresh-vterm-window (win)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Window has live Claude vterm-mode buffer | No | Requires vterm |
| Window has non-Claude buffer | No | |
| end-of-buffer error during reset | No | condition-case handles |

### `agent-repl--reset-vterm-cursors ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Multiple windows, skips selected | No | Requires multi-window |

### `agent-repl--on-window-change ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Normal change (sync + overlay + cursors) | No | Integration test |
| sync-panels errors (caught by condition-case) | No | |

### `agent-repl--deferred` (macro)

| Edge Case | Covered | Test |
|-----------|---------|------|
| Debounces rapid calls | Yes | `deferred-debounces` |
| Cancels pending timer | Yes | `deferred-debounces` |

### `agent-repl--bounce-from-vterm (_frame)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Selected window has no-other-window param, keyboard event | No | Requires window params |
| Selected window has no-other-window param, mouse event | No | |
| Normal window (no-op) | No | |

---

## Buffer creation

### `agent-repl--initialize-input-buffer (ws)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| First creation (mode initialization + history restore) | Yes | `agent-repl-test-initialize-input-buffer-fresh` |
| Buffer already in claude-input-mode (signals error) | Yes | `agent-repl-test-initialize-input-buffer-already-initialized` |

### `agent-repl--kill-stale-vterm ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Stale buffer exists (no process) | No | Requires buffer-name setup |
| Buffer has live process (keep alive) | No | |
| No buffer exists | No | |

---

## Panel show/hide strategies

### `agent-repl--show-loading-panels ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Normal placeholder creation and swap | No | Requires window management |

### `agent-repl--initialize-claude (&optional ws)`

Single entry point for starting a Claude session.  Absorbs what used
to live in `initialize-claude-output`, `initialize-new-vterm`, and
`start-claude`.

| Edge Case | Covered | Test |
|-----------|---------|------|
| No active workspace (error) | Yes | `initialize-claude-no-workspace` |
| Already running (error) | Yes | `initialize-claude-already-running-errors` |
| Sets prefix counter, overlay, :claude-state :init | Yes | `initialize-claude-starts-new-session` |
| Sends startup cmd + return to vterm | Yes | `initialize-claude-sends-cmd-and-return` |
| Schedules readiness timer | Yes | `initialize-claude-schedules-ready-timer` |
| Sets buffer-local `agent-repl--ready` to nil | Yes | `initialize-claude-sets-ready-nil` |
| Clears `:fork-session-id` after cmd build | Yes | `initialize-claude-clears-fork-session-id` |
| Calls `initialize-ws-env` when `:active-env` unset | Yes | `initialize-claude-calls-ws-env-init-when-unset` |
| Skips `initialize-ws-env` when `:active-env` set | Yes | `initialize-claude-skips-ws-env-init-when-set` |
| Uses explicit WS arg over `+workspace-current-name` | Yes | `initialize-claude-uses-explicit-ws-arg` |

### `agent-repl--show-existing-panels ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Normal show | No | Requires window management |
| No active workspace (error) | No | |

### `agent-repl--show-hidden-panels ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Clears :panels-hidden, re-activates :inactive | Yes | `show-hidden-clears-flag` |

### `agent-repl--hide-and-preserve-status ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Sets :panels-hidden | Yes | `hide-and-preserve-marks-hidden` |
| No active workspace (error) | Yes | `hide-and-preserve-no-workspace-errors` |

---

## Entry point

### `agent-repl ()` (interactive)

| Edge Case | Covered | Test |
|-----------|---------|------|
| Not running -> initialize-claude | Yes | `entry-point-not-running-initializes-claude` |
| Session starting -> loading message | Yes | `entry-point-session-starting-shows-message` |
| Panels visible -> hide | Yes | `entry-point-visible-hides` |
| Panels hidden -> show | Yes | `entry-point-hidden-shows` |
| Text selected -> send to Claude | Yes | `entry-point-selection-sends` |
| Selection takes priority over all other branches | Yes | `entry-point-selection-sends` (panels visible + selection) |

---

## Session cleanup

### `agent-repl--kill-placeholder ()`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Placeholder exists | Yes | `kill-placeholder-when-exists` |
| Placeholder absent | Yes | `kill-placeholder-when-absent` |

### `agent-repl--sigkill-if-alive (proc)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Dead/nil process | Yes | `sigkill-if-alive-dead-process` |
| Live process | No | Requires real process |

### `agent-repl--schedule-sigkill (proc)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Schedules timer for live process | No | Timer-based |

### `agent-repl--kill-vterm-process (buf)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| nil buffer | Yes | `kill-vterm-process-nil` |
| Dead buffer | Yes | `kill-vterm-process-dead-buffer` |
| Live buffer with process | No | Requires real process |
| Live buffer without process | No | |

### `agent-repl--teardown-session-state (ws)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Full teardown (overlay, timers, state clear) | No | Requires full session |

### `agent-repl--destroy-session-buffers (vterm-buf input-buf)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Both buffers live | No | Requires process setup |
| Input buffer dead | No | |

### `agent-repl--kill-session (ws)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Full session teardown | No | Integration test |

---

## User commands

### `agent-repl-kill ()` (interactive)

| Edge Case | Covered | Test |
|-----------|---------|------|
| Kills session and clears status | No | Integration test |
| No active workspace (error) | No | |

### `agent-repl-restart ()` (interactive)

| Edge Case | Covered | Test |
|-----------|---------|------|
| Kill + recreate buffers + show panels | No | Integration test |

### `agent-repl-focus-input ()` (interactive)

| Edge Case | Covered | Test |
|-----------|---------|------|
| Already in input buffer -> jump back | No | Requires window state |
| Not running -> start fresh | No | |
| Running, panels hidden -> show + focus | No | |
| Running, panels visible -> just focus | No | |

### `agent-repl-cycle ()` (interactive)

| Edge Case | Covered | Test |
|-----------|---------|------|
| Vterm live -> send backtab | No | Requires vterm |
| Vterm not live -> no-op | No | |

### `agent-repl--validate-env-switch (ws new-env worktree-p session-id)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| Not worktree -> error | Yes | `validate-env-switch-no-worktree` |
| No session ID -> error | Yes | `validate-env-switch-no-session-id` |
| Claude thinking -> error | Yes | `validate-env-switch-thinking` |
| Bare-metal switch (valid) -> success | Yes | `validate-env-switch-bare-metal-ok` |

### `agent-repl--seed-new-env-session (ws new-env session-id)`

| Edge Case | Covered | Test |
|-----------|---------|------|
| No existing instantiation -> creates and seeds | Yes | `seed-new-env-creates-inst` |
| Existing instantiation with session-id -> no overwrite | Yes | `seed-new-env-does-not-overwrite` |
| Existing instantiation without session-id -> seeds | No | |

### `agent-repl-switch-environment ()` (interactive)

| Edge Case | Covered | Test |
|-----------|---------|------|
| Full environment switch flow | No | Integration test |

---

## Coverage Statistics

- **Total functions**: 47
- **Functions with at least one test**: 24
- **Functions with zero tests**: 23
- **Individual edge cases identified**: ~120
- **Edge cases covered**: ~50

## Key Gaps

1. **Multi-window operations**: Functions like `show-panels`, `focus-input-panel`, `sync-panels`, and `bounce-from-vterm` require multi-window frame setups that are difficult in batch mode.

2. **Vterm-dependent functions**: `vterm-redraw`, `do-refresh`, `refresh-vterm`, `refresh-vterm-window` require real vterm-mode buffers.

3. **Session lifecycle**: `teardown-session-state`, `destroy-session-buffers`, `kill-session`, `kill` (interactive), `restart`, and `switch-environment` require full session infrastructure with running processes.

4. **Workspace/persp integration**: `before-persp-deactivate`, `after-persp-activated`, `redirect-from-claude-before-save`, and `on-workspace-switch` depend on the perspective/workspace layer.

## Recommendations

1. **Highest value additions**: Test `kill-stale-vterm` (can be mocked with `get-buffer` + process stubs), `ensure-input-buffer` (can test mode initialization), and `show-loading-panels` (can test placeholder swap logic).

2. **seed-new-env-session missing case**: Add a test for an existing instantiation that has no session-id yet (should seed it).

3. **safe-buffer-name with dead buffer**: Add a test to verify behavior when passed a killed buffer object.

4. **Integration test infrastructure**: Consider creating a test helper that sets up multiple windows for the multi-window tests, even in batch mode (Emacs supports `split-window` in batch).
