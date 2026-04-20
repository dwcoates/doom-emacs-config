# Test Coverage Analysis: core.el

## Function: `claude-repl--cancel-all-timers`
### Existing tests:
- (none)
### Missing edge cases:
- Empty `claude-repl--timers` list (no-op path)
- List with mix of valid timers and nil entries
- List with already-cancelled timers (timerp returns nil)
- Verify `claude-repl--timers` is set to nil after cancellation
- Multiple calls in succession (idempotency)

## Function: `claude-repl-debug/cancel-timers`
### Existing tests:
- (none)
### Missing edge cases:
- Verify it calls `claude-repl--cancel-all-timers`
- Verify it emits a message
- Interactive call with no timers active

## Function: `claude-repl--log-format`
### Existing tests:
- (none)
### Missing edge cases:
- Empty string input
- String with format specifiers (should be passed through literally, not expanded)
- Verify output contains timestamp pattern and [claude-repl] tag

## Function: `claude-repl--do-log`
### Existing tests:
- (none -- tested indirectly via `claude-repl--log`)
### Missing edge cases:
- FMT with no args
- FMT with multiple args
- Nil args list
- Verify message receives the formatted prefix

## Function: `claude-repl--log`
### Existing tests:
- `claude-repl-test-log-respects-debug-flag`: tests nil and t values of debug flag
### Missing edge cases:
- `claude-repl-debug` set to `'verbose` (should still log since non-nil)
- Multiple format arguments
- No format arguments (bare string)
- Verify the message content includes timestamp prefix

## Function: `claude-repl--log-verbose`
### Existing tests:
- (none)
### Missing edge cases:
- `claude-repl-debug` is nil (should not log)
- `claude-repl-debug` is t (should NOT log -- only logs for 'verbose)
- `claude-repl-debug` is `'verbose` (should log)
- Verify message content includes timestamp prefix

## Function: `claude-repl--dir-has-git-p`
### Existing tests:
- (none)
### Missing edge cases:
- Directory with `.git` subdirectory (normal repo)
- Directory with `.git` file (worktree / submodule)
- Directory with neither `.git` directory nor file
- Non-existent directory
- Nil input
- Empty string input

## Function: `claude-repl--git-root`
### Existing tests:
- (tested indirectly via resolve-root and workspace-id tests that stub it out)
### Missing edge cases:
- Called from within a git repo (should return repo root)
- Called from outside any git repo (should return nil)
- Called with explicit DIR argument
- Called with nil DIR (should fall back to `default-directory`)
- Deeply nested directory within a git repo
- Worktree directory (`.git` is a file, not a directory)

## Function: `claude-repl--git-string`
### Existing tests:
- (none)
### Missing edge cases:
- Valid git command (e.g., `rev-parse --show-toplevel`)
- Invalid git command (returns error text since stderr is included)
- Arguments with special characters (shell quoting)
- Empty result from git

## Function: `claude-repl--git-string-quiet`
### Existing tests:
- (none -- called at load time for `claude-repl-git-branch` and `claude-repl--main-git-root`)
### Missing edge cases:
- Valid git command
- Invalid git command (should return empty string, not error text)
- Called outside any git repo

## Function: `claude-repl-print-git-branch`
### Existing tests:
- (none)
### Missing edge cases:
- Verify message includes `claude-repl-git-branch` value

## Function: `claude-repl--path-canonical`
### Existing tests:
- (none -- tested indirectly by workspace-id tests)
### Missing edge cases:
- Path with trailing slash (should be stripped)
- Path without trailing slash (should remain unchanged)
- Path with tilde (should be expanded)
- Symlinked path (should be resolved to true path)
- Relative path
- Empty string
- Root path "/"

## Function: `claude-repl--resolve-root`
### Existing tests:
- `claude-repl-test-resolve-root-priority`: tests git > project-root > default-directory priority
### Missing edge cases:
- All three sources nil (should return nil from default-directory being nil -- unlikely but worth considering)
- Only `default-directory` available (no git, no project root)
- Verify verbose logging is called with correct source label

## Function: `claude-repl--workspace-id`
### Existing tests:
- `claude-repl-test-workspace-id-from-project-root`: uses project root fallback
- `claude-repl-test-workspace-id-default-directory`: uses default-directory fallback
### Missing edge cases:
- `resolve-root` returns nil (should return nil since `when root` guards the body)
- Verify the hash is exactly 8 characters
- Two different roots produce different IDs
- Same root always produces the same ID (deterministic)

## Function: `claude-repl--load-priority-images`
### Existing tests:
- (none)
### Missing edge cases:
- Images directory exists with all PNG files present
- Images directory exists but some PNG files missing (should skip missing)
- Images directory does not exist
- `load-file-name` is nil, `buffer-file-name` is used as fallback

## Function: `claude-repl--priority-image`
### Existing tests:
- (none)
### Missing edge cases:
- Valid priority string that exists in alist (e.g., "p1")
- Invalid/unknown priority string (should return nil)
- Nil priority input
- Empty `claude-repl--priority-images` alist

## Function: `claude-repl--ws-get`
### Existing tests:
- (tested indirectly by many tests that use workspace state)
### Missing edge cases:
- Getting key from non-existent workspace (should return nil)
- Getting non-existent key from existing workspace (should return nil)
- Getting key with nil value (should return nil -- indistinguishable from missing)
- Getting key with 0 value or empty string value

## Function: `claude-repl--ws-put`
### Existing tests:
- (tested indirectly by many tests that set workspace state)
### Missing edge cases:
- Putting to a brand new workspace (creates entry)
- Overwriting an existing key
- Setting value to nil
- Multiple keys on the same workspace

## Function: `claude-repl--ws-del`
### Existing tests:
- (none)
### Missing edge cases:
- Deleting an existing workspace
- Deleting a non-existent workspace (should be no-op)
- Verify get returns nil after delete

## Function: `claude-repl--active-inst`
### Existing tests:
- (none)
### Missing edge cases:
- Workspace with no `:active-env` set (defaults to `:bare-metal`)
- Workspace with `:active-env` set to `:sandbox`
- First call creates the struct; second call returns the same struct
- Verify returned value is a `claude-repl-instantiation` struct
- Verify struct fields are properly initialized (nil by default)

## Function: `claude-repl--open-initial-buffers`
### Existing tests:
- (none)
### Missing edge cases:
- No matching patterns in `claude-repl-workspace-initial-buffers`
- Matching pattern with existing files
- Matching pattern with non-existent file (should warn, not error)
- Multiple matching patterns
- `persp-get-by-name` returns nil (early return)
- Empty FILES list for a matching pattern

## Function: `claude-repl--buffer-name`
### Existing tests:
- `claude-repl-test-buffer-name-format`: tests default and "-input" suffix
- `claude-repl-test-buffer-name-default`: tests nil workspace-id fallback
### Missing edge cases:
- Empty string suffix
- Various suffix values
- Verify output matches vterm-buffer-re / input-buffer-re regexps as appropriate

## Function: `claude-repl--claude-buffer-p`
### Existing tests:
- `claude-repl-test-claude-buffer-p`: tests match, non-match for input buffer, non-match for scratch
### Missing edge cases:
- Buffer named exactly `*claude-*` (no hash -- should not match due to regex requiring hex chars)
- Buffer with name containing extra characters after the pattern (e.g., `*claude-abcd1234*extra`)
- Nil buffer argument (should use current buffer)
- Explicit buffer argument passed

## Function: `claude-repl--claude-panel-buffer-p`
### Existing tests:
- (none directly -- `claude-repl--non-user-buffer-p` depends on it)
### Missing edge cases:
- Vterm buffer name (should match)
- Input buffer name (should match)
- Regular buffer name (should not match)
- Nil buffer argument (should use current buffer)

## Function: `claude-repl--non-user-buffer-p`
### Existing tests:
- (none)
### Missing edge cases:
- Claude panel buffer (should return non-nil)
- Minibuffer (name starts with " *Minibuf") (should return non-nil)
- Dead/killed buffer (should return non-nil)
- Nil input (should return non-nil)
- String name of non-existent buffer (should return non-nil)
- String name of existing normal buffer (should return nil)
- Normal live buffer object (should return nil)

## Function: `claude-repl--non-claude-buffers`
### Existing tests:
- (none)
### Missing edge cases:
- Empty list input
- List with all claude buffers (should return empty)
- List with no claude buffers (should return all)
- Mixed list
- List containing nil entries
- List containing string names vs buffer objects

## Function: `claude-repl--current-ws-p`
### Existing tests:
- (none)
### Missing edge cases:
- WS matches current workspace name (should return non-nil)
- WS does not match (should return nil)
- Nil WS input
- Empty string WS

## Function: `claude-repl--current-ws-live-vterm`
### Existing tests:
- (tested indirectly via `claude-repl--vterm-live-p` tests)
### Missing edge cases:
- No vterm buffer stored (returns nil)
- Stored buffer is dead (returns nil)
- Stored buffer is live (returns the buffer)

## Function: `claude-repl--vterm-live-p`
### Existing tests:
- `claude-repl-test-vterm-live-p-nil`: no buffer stored
- `claude-repl-test-vterm-live-p-dead`: killed buffer
- `claude-repl-test-vterm-live-p-live`: live buffer
### Missing edge cases:
- Buffer stored as nil explicitly (vs. key not present at all)

## Macro: `claude-repl--with-vterm-buf`
### Existing tests:
- (none)
### Missing edge cases:
- No live vterm buffer (body should not execute, form returns nil)
- Live vterm buffer exists (body should execute with `vterm-buf` bound)
- Verify `vterm-buf` is accessible inside body
- Body returns a value (should be returned by macro)

## Function: `claude-repl--grey-hex`
### Existing tests:
- `claude-repl-test-grey-format`: tests 0, 255, 15
### Missing edge cases:
- Boundary value 128 (middle grey)
- Negative input (undefined behavior, but could test)
- Input > 255 (undefined behavior, but could test)

## Function: `claude-repl--set-buffer-background`
### Existing tests:
- (none)
### Missing edge cases:
- Sets default face background to correct hex
- Sets fringe face background to correct hex
- Different grey levels produce different colors

## Function: `claude-repl--default-background-request-p`
### Existing tests:
- (none)
### Missing edge cases:
- Index -1 with no :foreground or :inverse-video (should return non-nil)
- Index -1 with :foreground in args (should return nil)
- Index -1 with :inverse-video in args (should return nil)
- Index 0 (should return nil regardless of args)
- Index -1 with both :foreground and :inverse-video (should return nil)
- Empty args list with index -1 (should return non-nil)

## Function: `claude-repl--vterm-color-advice`
### Existing tests:
- (none)
### Missing edge cases:
- In a claude buffer with default background request (should return dark hex)
- In a claude buffer with non-default request (should pass through original)
- In a non-claude buffer (should pass through original regardless)
- Verify it calls the original function (fn)

## Struct: `claude-repl-instantiation`
### Existing tests:
- (none directly)
### Missing edge cases:
- Creating with `make-claude-repl-instantiation`
- Accessing each field (had-session, session-id, start-cmd)
- Modifying fields with setf
- Default field values are nil

## Cross-cutting: defvar/defcustom declarations
### Existing tests:
- `claude-repl-test-bug10-defvar-declarations`: checks workspaces, hide-input-box, notification-backend, sync-timer (NOTE: only `claude-repl--workspaces` is in core.el; others are in overlay.el, notifications.el, panels.el)
- `claude-repl-test-bug11-fullscreen-config-declared`: checks fullscreen-config (NOTE: this is a plist key in the workspace hash, not a standalone defvar in core.el)
### Missing edge cases:
- Verify `claude-repl--timers` is bound
- Verify `claude-repl-debug` default value is nil
- Verify `claude-repl--vterm-buffer-re` matches expected patterns
- Verify `claude-repl--input-buffer-re` matches expected patterns
- Verify `claude-repl--vterm-background-grey` is 15
- Verify `claude-repl-skip-permissions` default is t
- Verify `claude-repl-prefix-period` default is 7
- Verify `claude-repl--command-prefix` contains the command-prefix text

## Notes on migrated tests from other modules

The following tests were migrated to test-core.el per task instructions but
actually test functionality defined in other source files:

| Test | Actual source file |
|------|--------------------|
| `claude-repl-test-claude-running-p-no-process` | session.el |
| `claude-repl-test-claude-running-p-with-process` | session.el |
| `claude-repl-test-deferred-macro` | panels.el |
| `claude-repl-test-bug1-cursor-reset-timer-defvar` | panels.el |
| `claude-repl-test-bug9-paste-delay-configurable` | input.el |
| `claude-repl-test-bug10-defvar-declarations` | multiple files |
| `claude-repl-test-bug11-fullscreen-config-declared` | panels.el (workspace key) |
| `claude-repl-test-package-provide` | config.el |
| `claude-repl-test-workspace-for-buffer-no-persp` | status.el |
| `claude-repl-test-in-redraw-advice-declared` | overlay.el |

These should be relocated to their respective per-module test files when those
are created.
