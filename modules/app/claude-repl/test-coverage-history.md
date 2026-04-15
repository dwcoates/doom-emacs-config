# Test Coverage Analysis: history.el

## Constant: `claude-repl--environment-keys`
### Existing tests:
- `claude-repl-test-environment-keys-value`: verifies :bare-metal and :sandbox are members
### Missing edge cases:
- None; this is a constant.

## Macro: `claude-repl--with-error-logging`
### Existing tests:
- `claude-repl-test-with-error-logging-success`: confirms body value is returned on success
- `claude-repl-test-with-error-logging-catches-error`: confirms errors are caught and not propagated
### Missing edge cases:
- Body that returns nil (verify nil is returned, not confused with error case)
- Multiple forms in body (verify last value is returned)
- Verify that `claude-repl--log` is actually called with the label on error (would require mocking `claude-repl--log`)

## Function: `claude-repl--read-sexp-file`
### Existing tests:
- `claude-repl-test-write-and-read-sexp-file`: round-trip with write-sexp-file
### Missing edge cases:
- File with multiple sexps (only the first should be returned)
- File with complex nested data structures (alists, plists, vectors)
- Reading an empty file (should signal a read error)

## Function: `claude-repl--write-sexp-file`
### Existing tests:
- `claude-repl-test-write-and-read-sexp-file`: round-trip test
### Missing edge cases:
- Writing nil value
- Writing data with special characters (strings with quotes, newlines)
- Writing to a path where parent directory does not exist (should error)

## Function: `claude-repl--read-sexp-file-if-exists`
### Existing tests:
- `claude-repl-test-read-sexp-file-if-exists-present`: returns data when file exists
- `claude-repl-test-read-sexp-file-if-exists-absent`: returns nil for nonexistent file
### Missing edge cases:
- File exists but is empty (would error from `read`; should this be handled?)

## Function: `claude-repl--instantiation-to-plist`
### Existing tests:
- `claude-repl-test-instantiation-to-plist-nil`: returns nil for nil input
- `claude-repl-test-instantiation-to-plist-basic`: serializes session-id and had-session
- `claude-repl-test-instantiation-to-plist-empty-struct`: serializes struct with nil fields
### Missing edge cases:
- Struct with start-cmd set (verify start-cmd is intentionally NOT serialized)

## Function: `claude-repl--instantiation-from-plist`
### Existing tests:
- `claude-repl-test-instantiation-from-plist-restores`: restores session-id and had-session
- `claude-repl-test-instantiation-from-plist-nil-inst`: no-op when inst is nil
- `claude-repl-test-instantiation-from-plist-nil-saved`: no-op when saved is nil
- `claude-repl-test-instantiation-from-plist-both-nil`: no-op when both nil
### Missing edge cases:
- Saved plist with extra keys (verify they are silently ignored)
- Saved plist with partial keys (e.g., only :session-id, no :had-session)

## Function: `claude-repl--history-file`
### Existing tests:
- `claude-repl-test-history-file-path`: returns correct path under resolve-root
### Missing edge cases:
- None significant; it is a thin wrapper over expand-file-name.

## Function: `claude-repl--state-file`
### Existing tests:
- `claude-repl-test-state-file-with-root`: returns correct path under root
- `claude-repl-test-state-file-nil-root`: returns nil for nil root
### Missing edge cases:
- None significant; it is a thin wrapper with a nil guard.

## Function: `claude-repl--ws-live-input-buffer`
### Existing tests:
- `claude-repl-test-ws-live-input-buffer-returns-live`: returns live buffer
- `claude-repl-test-ws-live-input-buffer-returns-nil-for-dead`: returns nil for killed buffer
- `claude-repl-test-ws-live-input-buffer-nil-when-no-buffer`: returns nil when unset
- `claude-repl-test-ws-live-input-buffer-explicit-ws`: accepts explicit workspace name
### Missing edge cases:
- WS is nil and +workspace-current-name also returns nil (both fallbacks fail)

## Function: `claude-repl--history-save`
### Existing tests:
- `claude-repl-test-bug8-history-save-uses-input-buffer-root`: verifies file path uses input buffer's root
- `claude-repl-test-history-save-and-restore-round-trip`: full save and restore cycle
- `claude-repl-test-history-save-skips-nil-history`: does not write when history is nil
- `claude-repl-test-history-save-skips-dead-buffer`: does nothing for killed buffer
### Missing edge cases:
- History with a single entry (boundary case for list serialization)

## Function: `claude-repl--history-restore`
### Existing tests:
- `claude-repl-test-history-save-and-restore-round-trip`: restores after save
- `claude-repl-test-history-restore-no-file`: leaves history nil when no file
### Missing edge cases:
- File exists but contains non-list data (e.g., a string or number)
- Restoring into a buffer that already has history (verify it overwrites, not appends)

## Function: `claude-repl--collect-env-state`
### Existing tests:
- `claude-repl-test-collect-env-state`: returns plists for both environment keys
- `claude-repl-test-collect-env-state-nil-envs`: returns nil plists when no envs initialized
### Missing edge cases:
- Only one environment initialized (partial state)

## Function: `claude-repl--state-save`
### Existing tests:
- `claude-repl-test-state-save-writes-file`: writes state file with correct content
- `claude-repl-test-state-save-skips-when-no-project-dir`: no-op when :project-dir is nil
### Missing edge cases:
- :active-env is nil (verify it is serialized as nil)
- Write error (e.g., read-only directory; verify with-error-logging catches it)

## Function: `claude-repl--restore-env-state`
### Existing tests:
- `claude-repl-test-restore-env-state`: restores both environments from saved state
### Missing edge cases:
- Saved state has nil for one environment key
- Workspace struct is nil for one environment (instantiation-from-plist handles it)

## Function: `claude-repl--apply-restored-state`
### Existing tests:
- `claude-repl-test-apply-restored-state-sets-project-dir`: sets :project-dir from state
- `claude-repl-test-apply-restored-state-nil-project-dir`: does not set when nil
### Missing edge cases:
- Completely empty state plist (should not error)

## Function: `claude-repl--state-restore`
### Existing tests:
- `claude-repl-test-state-restore-reads-file`: reads file and applies state
- `claude-repl-test-state-restore-no-file`: graceful no-op when file missing
- `claude-repl-test-state-restore-fallback-to-git-root`: falls back to git-root for root resolution
### Missing edge cases:
- Both :project-dir and git-root return nil (state-file returns nil, no read attempted)
- State file exists but contains corrupt data (should be caught by with-error-logging)

## Variable: `claude-repl--input-history`
### Existing tests:
- Covered indirectly by all history-push/prev/next tests
### Missing edge cases:
- None; it is a buffer-local variable.

## Variable: `claude-repl--history-index`
### Existing tests:
- Covered indirectly by navigation and reset tests
### Missing edge cases:
- None; it is a buffer-local variable.

## Variable: `claude-repl--history-stash`
### Existing tests:
- Covered indirectly by prev/next tests
### Missing edge cases:
- None; it is a buffer-local variable.

## Variable: `claude-repl--history-navigating`
### Existing tests:
- Covered indirectly by on-change and replace-buffer-text tests
### Missing edge cases:
- None; it is a buffer-local variable.

## Function: `claude-repl--history-push`
### Existing tests:
- `claude-repl-test-history-push`: push, skip empty, skip duplicate
- `claude-repl-test-history-push-explicit-text`: explicit text argument
- `claude-repl-test-history-push-trims-whitespace`: trims leading/trailing whitespace
- `claude-repl-test-history-push-duplicate-after-trim`: skip duplicate after trimming
### Missing edge cases:
- Push a non-duplicate that differs only in internal whitespace (should be added)
- Push to a non-empty history where the new entry matches a non-first entry (should still push since only head is checked)

## Function: `claude-repl--history-reset`
### Existing tests:
- `claude-repl-test-history-reset`: sets index to -1
### Missing edge cases:
- Calling reset when already at -1 (idempotent; trivial)

## Function: `claude-repl--history-replace-buffer-text`
### Existing tests:
- `claude-repl-test-history-replace-buffer-text`: replaces text, suppresses on-change
### Missing edge cases:
- Replacing with empty string
- Replacing in an already-empty buffer

## Function: `claude-repl--history-show-entry`
### Existing tests:
- `claude-repl-test-history-show-entry-from-history`: shows entry at given index
- `claude-repl-test-history-show-entry-restores-stash`: restores stash at negative index
- `claude-repl-test-history-show-entry-nil-stash`: inserts empty string when stash is nil
### Missing edge cases:
- Index at exact boundary (last valid index in history list)

## Function: `claude-repl--history-prev`
### Existing tests:
- `claude-repl-test-history-prev-next`: full navigation cycle
- `claude-repl-test-history-prev-empty-list`: no-op with empty history
- `claude-repl-test-history-prev-at-oldest`: no-op when already at oldest
- `claude-repl-test-history-prev-stashes-only-on-first-nav`: stash set only on first prev
### Missing edge cases:
- Single-entry history: prev from -1 goes to 0, second prev stays at 0

## Function: `claude-repl--history-next`
### Existing tests:
- `claude-repl-test-history-prev-next`: full navigation cycle including next
- `claude-repl-test-history-next-at-start`: no-op when index is -1
### Missing edge cases:
- Calling next from index 0 (should go to -1 and restore stash)

## Function: `claude-repl--history-on-change`
### Existing tests:
- `claude-repl-test-history-on-change-resets`: resets index when editing during browsing
- `claude-repl-test-history-on-change-noop-during-navigation`: no-op when navigating flag set
- `claude-repl-test-history-on-change-noop-when-not-browsing`: no-op when index is -1
### Missing edge cases:
- Called with extra arguments (the &rest _args should be ignored)
