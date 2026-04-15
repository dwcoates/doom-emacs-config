# Test Coverage Analysis: status.el

## Function-by-function edge case enumeration

### `claude-repl--ws-state` (line 7)
Simple accessor delegating to `ws-get`.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Returns correct keyword after ws-set | Yes | `ws-set-and-state`, `ws-state-transitions` |
| Returns nil when no status set | Yes | `ws-clear` (clears then checks nil) |
| Returns :inactive | Yes | `ws-state-inactive` |
| Returns :permission | Yes | `ws-set-permission` |
| Called on workspace with no plist entry at all | No | Missing -- ws never touched, should return nil |

### `claude-repl--ws-set` (line 12)
Sets status keyword, errors on nil workspace.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Sets :thinking | Yes | `ws-set-and-state` |
| Sets :done | Yes | `ws-set-and-state` |
| Sets :permission | Yes | `ws-set-permission` |
| Sets :inactive | Yes | `ws-state-inactive`, `ws-state-transitions` |
| Overwrites previous state | Yes | `ws-set-and-state` (thinking -> done) |
| nil workspace signals error | Yes | `ws-set-nil-error` |
| Calls `force-mode-line-update` | No | Not verified -- side effect not asserted |
| Setting same state twice (idempotent) | No | Missing |

### `claude-repl--ws-clear-if-status` / `claude-repl--ws-clear` (lines 20-30)
Conditional clear: only clears if current status matches the given state.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Clears matching state (thinking/thinking) | Yes | `ws-clear` |
| Does not clear mismatched state (thinking vs done) | Yes | `ws-clear-done` |
| Does not clear mismatched state (done vs permission) | Yes | `ws-clear-permission` |
| nil workspace signals error | Yes | `ws-clear-nil-error` |
| Clear when status is already nil | No | Missing -- should be no-op |
| Clear matching :done state | No | Missing -- only tested mismatches |
| Clear matching :permission state | No | Missing |
| Clear matching :inactive state | No | Missing |
| Calls `force-mode-line-update` only on match | No | Side effect not verified |

### `claude-repl--ws-dir` (line 32)
Returns :project-dir or errors.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Returns :project-dir when set | No | Not tested |
| Errors when :project-dir is nil/missing | No | Not tested |

### `claude-repl--workspace-clean-p` (line 40)
Returns non-nil when :git-clean is not 'dirty (defaults to clean).

| Edge case | Covered? | Test |
|-----------|----------|------|
| Returns t when :git-clean is 'clean | No | Not tested |
| Returns nil when :git-clean is 'dirty | No | Not tested |
| Returns t (clean) when :git-clean has never been set (nil) | No | Not tested -- important default behavior |

### `claude-repl--git-check-in-progress-p` (line 47)
Checks if a live git-diff process exists for the workspace.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Returns nil when no :git-proc set | No | Not tested |
| Returns nil when :git-proc is a dead process | No | Not tested |
| Returns t when :git-proc is a live process | No | Not tested |

### `claude-repl--git-diff-sentinel` (line 52)
Process sentinel that records git cleanliness and triggers state update.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Sets 'clean when exit status is 0 | No | Not tested |
| Sets 'dirty when exit status is non-zero | No | Not tested |
| Clears :git-proc after completion | No | Not tested |
| Calls update-ws-state after completion | No | Not tested |
| No-op when process is still live | No | Not tested |

### `claude-repl--async-refresh-git-status` (line 65)
Starts an async git diff process. No-op if check already in progress.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Starts process when no check in progress | No | Not tested (requires process infrastructure) |
| No-op when check already in progress | No | Not tested |
| No-op when ws-dir returns nil/errors | No | Not tested |
| Stores process in :git-proc | No | Not tested |

### `claude-repl--workspace-for-buffer` (line 86)
Returns workspace name containing a buffer.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Returns nil when persp-mode is nil | No | Not tested |
| Returns workspace name when buffer found | No | Not tested |
| Returns nil when buffer not in any persp | No | Not tested |

### `claude-repl--status-color` (line 114)
Looks up color property for a given state.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Returns :bg for :thinking | No | Not directly tested |
| Returns :label for :permission ("❓") | Indirectly | `tabline-permission-label` exercises this path |
| Returns nil for unknown state | No | Not tested |
| Returns nil for unknown prop on valid state | No | Not tested |
| Returns nil when state is nil | No | Not tested |

### `claude-repl--render-tab` (line 143)
Low-level tab string builder.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Correct structure with all parts | Indirectly | Via tabline tests |
| img-str is nil (no image segment) | Indirectly | All tabline tests (no priority set) |
| img-str is non-nil (image inserted) | No | Not tested |
| Empty name string | No | Not tested |

### `claude-repl--render-selected-tab` (line 154)
Renders a selected tab with bracket foreground derived from state.

| Edge case | Covered? | Test |
|-----------|----------|------|
| State is nil (bracket-fg defaults to "black") | Indirectly | Tabline tests with unset state |
| State is :thinking (bracket-fg is "#cc3333") | Indirectly | `tabline-selected-suppresses-thinking` |
| State is :permission (bracket-fg is "#2a8c2a") | Indirectly | `tabline-selected-shows-permission` |
| img-str is non-nil | No | Not tested |

### `claude-repl--render-unselected-tab` (line 169)
Renders an unselected tab.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Applies given face to name | Indirectly | `tabline-done-face`, `tabline-inactive-face` |
| img-str is non-nil | No | Not tested |

### `claude-repl--tab-label` (line 180)
Returns label: status-specific label or index as string.

| Edge case | Covered? | Test |
|-----------|----------|------|
| :permission returns "❓" | Indirectly | `tabline-permission-label` |
| :thinking returns index (no :label defined) | Indirectly | `tabline-thinking-face` |
| nil state returns index string | Indirectly | Various tabline tests |
| Index is 0 or negative | No | Not tested |

### `claude-repl--tab-face` (line 186)
Returns face symbol or fallback.

| Edge case | Covered? | Test |
|-----------|----------|------|
| :thinking -> claude-repl-tab-thinking | Indirectly | `tabline-thinking-face` |
| :done -> claude-repl-tab-done | Indirectly | `tabline-done-face` |
| :inactive -> claude-repl-tab-inactive | Indirectly | `tabline-inactive-face` |
| :permission -> claude-repl-tab-permission | Indirectly | `tabline-selected-shows-permission` |
| nil state + selected -> +workspace-tab-selected-face | No | Not directly tested |
| nil state + unselected -> +workspace-tab-face | No | Not directly tested |

### `claude-repl--tab-priority-image-str` (line 193)
Returns propertized image string for priority, or nil.

| Edge case | Covered? | Test |
|-----------|----------|------|
| No :priority set -> nil | Indirectly | All tabline tests |
| :priority set but no image found -> nil | No | Not tested |
| :priority set and image found -> propertized string | No | Not tested |

### `claude-repl--render-tab-entry` (line 199)
Orchestrator: dispatches to selected or unselected renderer.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Selected tab rendering | Indirectly | `tabline-selected-*` tests |
| Unselected tab rendering | Indirectly | `tabline-done-face`, `tabline-inactive-face` |
| current-name does not match any name | No | Not tested |

### `claude-repl--tabline-advice` (line 212)
Full tabline override. Joins rendered tabs with spaces.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Names provided as argument | Yes | All tabline tests pass explicit names |
| Names default from +workspace-list-names | No | Not tested |
| Empty names list | No | Not tested |
| Single workspace | Indirectly | `tabline-permission-label`, `tabline-selected-*` |
| Multiple workspaces | Yes | `tabline-thinking-face`, `tabline-done-face` |

### `claude-repl--wconf-has-claude-p` (line 234)
Recursive tree walk for claude buffer in window config.

| Edge case | Covered? | Test |
|-----------|----------|------|
| nil wconf -> nil | No | Not tested |
| Non-list wconf -> nil | No | Not tested |
| Flat wconf with matching buffer entry | No | Not tested |
| Nested wconf with matching buffer in child | No | Not tested |
| Wconf with no buffer entries | No | Not tested |
| Wconf with non-claude buffer entries | No | Not tested |

### `claude-repl--visible-claude-buffer-p` (line 244)
Checks if a buffer is live, claude, and visible.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Dead buffer -> nil | No | Not tested |
| Live non-claude buffer -> nil | No | Not tested |
| Live claude buffer with no window -> nil | No | Not tested |
| Live claude buffer with window -> t | No | Not tested |

### `claude-repl--claude-visible-in-current-ws-p` (line 250)
Checks if any buffer in buffer-list is a visible claude buffer.

| Edge case | Covered? | Test |
|-----------|----------|------|
| No claude buffers -> nil | No | Not tested |
| Claude buffer visible -> t | No | Not tested |

### `claude-repl--claude-in-saved-wconf-p` (line 255)
Checks saved persp window config for claude buffer.

| Edge case | Covered? | Test |
|-----------|----------|------|
| persp not found -> nil | No | Not tested |
| persp is a symbol (nil persp) -> nil | No | Not tested |
| persp has wconf with claude buffer -> t | No | Not tested |
| persp has wconf without claude buffer -> nil | No | Not tested |

### `claude-repl--ws-claude-open-p` (line 261)
Dispatches between current-ws and background-ws check.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Current workspace -> delegates to visible check | No | Not tested |
| Background workspace -> delegates to saved wconf check | No | Not tested |

### `claude-repl--panels-actively-visible-p` (line 269)
Returns t only if ws is current AND claude panels are visible.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Background workspace -> nil | No | Not tested |
| Current workspace with visible panels -> t | No | Not tested |
| Current workspace with no visible panels -> nil | No | Not tested |

### `claude-repl--update-ws-state` (line 278)
Core state machine with multiple transition rules.

| Edge case | Covered? | Test |
|-----------|----------|------|
| :thinking -> unchanged (any dirty value) | No | Not tested |
| :done + viewed + panels not visible -> :inactive | No | Not tested |
| :done + not viewed -> unchanged | No | Not tested |
| :done + viewed + panels visible -> unchanged | No | Not tested |
| :inactive + dirty -> :done (clears :viewed) | No | Not tested |
| :inactive + clean -> unchanged | No | Not tested |
| nil + dirty -> :done (clears :viewed) | No | Not tested |
| nil + clean -> unchanged | No | Not tested |
| :permission -> unchanged (any dirty value) | No | Not tested |

### `claude-repl--update-all-workspace-states` (line 303)
Iterates all workspaces, updating state or clearing stale state.

| Edge case | Covered? | Test |
|-----------|----------|------|
| persp-mode is nil -> no-op | No | Not tested |
| Workspace with running vterm -> update + refresh | No | Not tested |
| Workspace without running vterm -> clear stale | No | Not tested |
| Calls poll-workspace-notifications | No | Not tested |

### `claude-repl--maybe-clear-stale-state` (line 323)
Clears non-thinking state when no vterm process is running.

| Edge case | Covered? | Test |
|-----------|----------|------|
| State is nil -> no-op | No | Not tested |
| State is :thinking -> preserved (not cleared) | No | Not tested |
| State is :done -> cleared to nil | No | Not tested |
| State is :permission -> cleared to nil | No | Not tested |
| State is :inactive -> cleared to nil | No | Not tested |

### `claude-repl--on-frame-focus` (line 334)
Handler for frame focus events.

| Edge case | Covered? | Test |
|-----------|----------|------|
| Frame has focus -> refreshes and updates | No | Not tested |
| Frame does not have focus -> no-op | No | Not tested |

## Coverage Summary

### Well-covered areas
- **Workspace state accessors** (ws-set, ws-clear, ws-state): Good coverage of basic set/get/clear operations, nil error guards, and state transitions through all four keywords.
- **Tabline rendering**: Good coverage of face assignment for background tabs (:thinking, :done, :inactive), permission label emoji, and selected-tab face behavior.

### Major gaps

1. **State machine (`update-ws-state`)**: Zero direct test coverage. All 9 transition/no-op cases are untested. This is the most important function in the file.
2. **Stale state clearing (`maybe-clear-stale-state`)**: Zero coverage. The :thinking preservation logic is untested.
3. **Git status layer** (`workspace-clean-p`, `git-check-in-progress-p`, `git-diff-sentinel`, `async-refresh-git-status`): Zero coverage. The default-to-clean behavior is untested.
4. **Panel visibility functions** (`wconf-has-claude-p`, `visible-claude-buffer-p`, `claude-visible-in-current-ws-p`, `claude-in-saved-wconf-p`, `ws-claude-open-p`, `panels-actively-visible-p`): Zero coverage.
5. **Buffer/workspace resolution** (`workspace-for-buffer`): Zero coverage.
6. **Helper functions** (`ws-dir`, `status-color`, `tab-label`, `tab-face`): Only indirect coverage through tabline tests. No direct unit tests.
7. **Frame focus handler** (`on-frame-focus`): Zero coverage.
8. **`update-all-workspace-states`**: Zero coverage.
9. **`render-tab` with img-str non-nil**: The priority image path is never exercised.
10. **`ws-clear-if-status` for matching non-thinking states**: Only tested for the mismatch case. Never tested that clearing :done when status IS :done actually works.

### Estimated line coverage
- Lines with direct/indirect test coverage: ~45 of ~120 functional lines (~37%)
- Lines with zero coverage: ~75 of ~120 functional lines (~63%)

### Priority recommendations for new tests
1. `update-ws-state` -- all 9 pcase branches (highest priority, core logic)
2. `maybe-clear-stale-state` -- :thinking preserved, other states cleared
3. `workspace-clean-p` -- clean/dirty/default-nil cases
4. `ws-dir` -- success and error paths
5. `status-color` -- direct lookups including nil/unknown state
6. `tab-label` / `tab-face` -- direct unit tests for all states
7. Panel visibility functions (may require more elaborate stubs)
