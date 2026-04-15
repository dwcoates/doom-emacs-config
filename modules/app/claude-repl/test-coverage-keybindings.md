# Test Coverage Analysis: keybindings.el

## Functions and Edge Cases

### Section 1: Internal Helpers

#### `claude-repl--cons-name-state (name)`
| Edge Case | Covered | Test |
|---|---|---|
| Workspace with active state | Yes | `claude-repl-test-cons-name-state-with-state` |
| Workspace with no state (returns nil) | Yes | `claude-repl-test-cons-name-state-no-state` |

#### `claude-repl--format-workspace-state (pair)`
| Edge Case | Covered | Test |
|---|---|---|
| Pair with non-nil state (keyword) | Yes | `claude-repl-test-format-workspace-state-with-value` |
| Pair with nil state (falls back to "nil") | Yes | `claude-repl-test-format-workspace-state-nil-state` |

#### `claude-repl--format-buffer-info (buf)`
| Edge Case | Covered | Test |
|---|---|---|
| Buffer with owning workspace and persp workspace set | Yes | `claude-repl-test-format-buffer-info-with-values` |
| Buffer with both values nil | Yes | `claude-repl-test-format-buffer-info-nil-values` |
| Buffer with owning set but persp nil | No | -- (partially covered by nil test) |

#### `claude-repl--kill-before-workspace-delete (&rest _)`
| Edge Case | Covered | Test |
|---|---|---|
| vterm is running -> calls claude-repl-kill | Yes | `claude-repl-test-kill-before-workspace-delete-when-running` |
| vterm is not running -> no-op | Yes | `claude-repl-test-kill-before-workspace-delete-when-not-running` |
| Called with extra arguments (advice compatibility) | Yes | `claude-repl-test-kill-before-workspace-delete-ignores-args` |

#### `claude-repl--read-workspace (prompt)`
| Edge Case | Covered | Test |
|---|---|---|
| Returns completing-read result | Yes | `claude-repl-test-read-workspace-returns-match` |
| Requires exact match (REQUIRE-MATCH = t) | Partial | -- (verified via stub, not interactively) |

#### `claude-repl--read-workspace-with-default (prompt)`
| Edge Case | Covered | Test |
|---|---|---|
| Passes current workspace name as default | Yes | `claude-repl-test-read-workspace-with-default` |

#### `claude-repl--write-output-json (filename content)`
| Edge Case | Covered | Test |
|---|---|---|
| Creates file with correct JSON content | Yes | `claude-repl-test-write-output-json-creates-file` |
| Creates output directory if it does not exist | Yes | `claude-repl-test-write-output-json-creates-directory` |
| Returns full path of written file | Yes | `claude-repl-test-write-output-json-returns-full-path` |
| Content is a vector (array) | Yes | `claude-repl-test-write-output-json-returns-full-path` |
| Content is an alist (object) | Yes | `claude-repl-test-write-output-json-creates-file` |

#### `claude-repl--list-claude-vterm-buffers ()`
| Edge Case | Covered | Test |
|---|---|---|
| Filters only matching claude vterm buffers | Yes | `claude-repl-test-list-claude-vterm-buffers-filters` |
| Returns nil when no buffers match | Yes | `claude-repl-test-list-claude-vterm-buffers-empty` |
| Does not include input buffers (only vterm pattern) | No | -- |
| Does not include killed/dead buffers | No | -- (relies on `buffer-list` behavior) |

### Section 2: Utility Commands

#### `claude-repl--send-digit-char ()`
| Edge Case | Covered | Test |
|---|---|---|
| Extracts digit "3" from key sequence [SPC o 3] | Yes | `claude-repl-test-send-digit-char-extracts-last-key` |
| Extracts digit "0" (boundary) | Yes | `claude-repl-test-send-digit-char-zero` |
| Extracts digit "9" (boundary) | Yes | `claude-repl-test-send-digit-char-nine` |
| Non-digit last key (not expected in production) | No | -- |

#### `claude-repl-paste-to-vterm ()`
| Edge Case | Covered | Test |
|---|---|---|
| Forwards Ctrl-V when vterm is live | Yes | `claude-repl-test-paste-to-vterm-when-live` |
| Does nothing when vterm is not live | Yes | `claude-repl-test-paste-to-vterm-when-not-live` |

#### `claude-repl-set-priority (priority)`
| Edge Case | Covered | Test |
|---|---|---|
| Stores priority value in workspace state | Yes | `claude-repl-test-set-priority-stores-value` |
| Empty string clears priority (sets nil) | Yes | `claude-repl-test-set-priority-clears-on-empty` |
| Messages current priority or "cleared" | Yes | `claude-repl-test-set-priority-messages` |

#### `claude-repl-revert-and-eval-buffer ()`
| Edge Case | Covered | Test |
|---|---|---|
| Calls revert-buffer then eval-buffer in order | Yes | `claude-repl-test-revert-and-eval-buffer` |

### Section 3: Debug Helpers

#### `claude-repl-debug/mock-workspace-generation (&optional names)`
| Edge Case | Covered | Test |
|---|---|---|
| Default names (nil -> "DWC/mock-test") | Yes | `claude-repl-test-mock-workspace-generation-default` |
| Custom names list | Yes | `claude-repl-test-mock-workspace-generation-custom-names` |

#### `claude-repl-debug/mock-workspace-commands-with-priority ()`
| Edge Case | Covered | Test |
|---|---|---|
| Interactive-only (requires completing-read + read-string) | No | -- (requires interactive stubs) |

#### `claude-repl-debug/process-pending-commands ()`
| Edge Case | Covered | Test |
|---|---|---|
| No output directory exists | Yes | `claude-repl-test-process-pending-commands-no-dir` |
| Multiple matching files are all processed | Yes | `claude-repl-test-process-pending-commands-processes-files` |
| Non-matching files are ignored | Yes | `claude-repl-test-process-pending-commands-processes-files` |
| Empty directory (no matching files) | Partial | -- (covered by no-dir test) |

#### `claude-repl-debug/workspace-states ()`
| Edge Case | Covered | Test |
|---|---|---|
| Displays all workspace states | Yes | `claude-repl-test-debug-workspace-states` |
| Empty workspace list | No | -- |

#### `claude-repl-debug/buffer-info ()`
| Edge Case | Covered | Test |
|---|---|---|
| Displays info for existing claude buffers | Yes | `claude-repl-test-debug-buffer-info-with-buffers` |
| Shows "(none)" when no buffers exist | Yes | `claude-repl-test-debug-buffer-info-no-buffers` |

#### `claude-repl-debug/clear-state (ws)`
| Edge Case | Covered | Test |
|---|---|---|
| Clears all state types for a workspace | Yes | `claude-repl-test-debug-clear-state` |
| Workspace with no state (no-op) | No | -- |

#### `claude-repl--kill-owned-panel-buffers (ws)`
| Edge Case | Covered | Test |
|---|---|---|
| Kills panel buffers owned by target workspace | Yes | `claude-repl-test-kill-owned-panel-buffers-kills-matching` |
| Preserves buffers owned by other workspaces | Yes | `claude-repl-test-kill-owned-panel-buffers-kills-matching` |
| Ignores non-panel buffers | Yes | `claude-repl-test-kill-owned-panel-buffers-ignores-non-panel` |
| No matching buffers -> no-op | Yes | `claude-repl-test-kill-owned-panel-buffers-no-match` |
| Silences process query flag before kill | Yes | `claude-repl-test-kill-owned-panel-buffers-silences-process` |
| Buffer with window -> closes window first | Partial | -- (window deletion tested implicitly) |
| Buffer with no process (proc is nil) | Yes | `claude-repl-test-kill-owned-panel-buffers-kills-matching` |

#### `claude-repl-debug/obliterate (ws)`
| Edge Case | Covered | Test |
|---|---|---|
| Removes all workspace state from hash table | Yes | `claude-repl-test-obliterate-removes-state` |
| Calls kill-owned-panel-buffers | Yes | `claude-repl-test-obliterate-removes-state` (stubbed) |

#### `claude-repl-debug/set-owning-workspace ()`
| Edge Case | Covered | Test |
|---|---|---|
| Interactive-only (requires two completing-reads) | No | -- (requires interactive stubs) |

#### `claude-repl-debug/toggle-logging (&optional verbose)`
| Edge Case | Covered | Test |
|---|---|---|
| nil -> t (standard toggle on) | Yes | `claude-repl-test-toggle-logging-on-off` |
| t -> nil (standard toggle off) | Yes | `claude-repl-test-toggle-logging-on-off` |
| nil -> verbose (with prefix) | Yes | `claude-repl-test-toggle-logging-verbose` |
| verbose -> nil (with prefix) | Yes | `claude-repl-test-toggle-logging-verbose` |
| t -> verbose (prefix from t) | Yes | `claude-repl-test-toggle-logging-verbose-from-t` |

#### `claude-repl-debug/toggle-metaprompt ()`
| Edge Case | Covered | Test |
|---|---|---|
| nil -> t | Yes | `claude-repl-test-toggle-metaprompt` |
| t -> nil | Yes | `claude-repl-test-toggle-metaprompt` |

#### `claude-repl-debug/prefix-counter ()`
| Edge Case | Covered | Test |
|---|---|---|
| No counter set (defaults to 0) | Yes | `claude-repl-test-prefix-counter-message` |
| Counter with existing value, correct remainder calculation | Yes | `claude-repl-test-prefix-counter-with-existing-count` |

#### `claude-repl-debug/workspace-clean-p (ws-name)`
| Edge Case | Covered | Test |
|---|---|---|
| Clean workspace | Yes | `claude-repl-test-debug-workspace-clean-p` |
| Dirty workspace | Yes | `claude-repl-test-debug-workspace-dirty` |

#### `claude-repl-debug/--gather-ws-diagnostics (ws-name)`
| Edge Case | Covered | Test |
|---|---|---|
| All fields populated (vterm buffer, process, window) | No | -- (requires persp-mode stubs; tested indirectly via format-diagnostics) |
| No persp found (returns nil for all buffer-related fields) | No | -- |
| Persp is a symbol (edge guard `(not (symbolp persp))`) | No | -- |

#### `claude-repl-debug/--apply-state-refresh (ws-name claude-open)`
| Edge Case | Covered | Test |
|---|---|---|
| claude-open -> calls update-ws-state | Yes | `claude-repl-test-apply-state-refresh-claude-open` |
| Not open, state is :done -> clears state | Yes | `claude-repl-test-apply-state-refresh-not-open-clears-non-thinking` |
| Not open, state is :thinking -> preserves state | Yes | `claude-repl-test-apply-state-refresh-not-open-preserves-thinking` |
| Not open, no state -> no-op | Yes | `claude-repl-test-apply-state-refresh-not-open-no-state` |
| Not open, state is :permission -> clears state | No | -- (similar to :done case) |
| Not open, state is :inactive -> clears state | No | -- (similar to :done case) |

#### `claude-repl-debug/--format-diagnostics (ws-name diag before after)`
| Edge Case | Covered | Test |
|---|---|---|
| All fields populated | Yes | `claude-repl-test-format-diagnostics-full` |
| All nil values | Yes | `claude-repl-test-format-diagnostics-nil-values` |
| Dirty = t | Yes | `claude-repl-test-format-diagnostics-dirty` |
| No vterm-buf (buffer-name returns nil) | Yes | `claude-repl-test-format-diagnostics-nil-values` |

#### `claude-repl-debug/refresh-state (ws-name)`
| Edge Case | Covered | Test |
|---|---|---|
| Full integration (gather -> apply -> format -> message) | No | -- (requires many stubs; sub-functions tested individually) |

### Section 4: Keybinding Definitions (map! calls)
Not unit-testable in batch mode (declarative `map!` calls are no-ops under test stubs).

### Section 5: Advice Registration
| Edge Case | Covered | Test |
|---|---|---|
| `advice-add` for `+workspace/kill` | No | -- (side-effect at load time; verified manually) |

### Constant: `claude-repl--output-dir`
| Edge Case | Covered | Test |
|---|---|---|
| Is absolute path ending in output/ | Yes | `claude-repl-test-output-dir-is-absolute` |

---

## Summary

| Category | Functions | Fully Covered | Partially Covered | Not Covered |
|---|---|---|---|---|
| Internal helpers | 7 | 6 | 1 | 0 |
| Utility commands | 4 | 4 | 0 | 0 |
| Debug helpers | 15 | 9 | 2 | 4 |
| Keybinding declarations | N/A | -- | -- | -- |
| Advice registrations | 1 | 0 | 0 | 1 |
| **Total** | **27** | **19** | **3** | **5** |

### Not covered (with rationale)
1. **`claude-repl-debug/mock-workspace-commands-with-priority`** -- Heavily interactive (completing-read + read-string + format-time-string for filename). Would need extensive stubbing of interactive prompts.
2. **`claude-repl-debug/set-owning-workspace`** -- Interactive with two completing-reads.
3. **`claude-repl-debug/--gather-ws-diagnostics`** -- Requires persp-mode runtime with real perspective objects and buffer membership queries. Sub-functions it feeds into (format-diagnostics, apply-state-refresh) are tested individually.
4. **`claude-repl-debug/refresh-state`** -- Integration function orchestrating gather + apply + format. All sub-functions are tested; integration test would require full persp-mode stubs.
5. **Advice registration** -- Load-time side effect; `claude-repl--kill-before-workspace-delete` (the advised function) is tested directly.
