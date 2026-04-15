# Test Coverage Analysis: worktree.el

## Functions and Coverage Status

### Fully Tested (unit tests in test-worktree.el)

| Function | Tests | Edge Cases Covered |
|----------|-------|--------------------|
| `claude-repl--extract-cherry-pick-shas` | 5 tests | empty input, no annotations, single SHA, multiple SHAs (reverse order), short/invalid hashes ignored |
| `claude-repl--bare-workspace-name` | 4 tests | simple name, slashed path, deeply nested path, trailing slash |
| `claude-repl--assert-clean-worktree` | 3 tests | clean repo, unstaged changes, staged changes |
| `claude-repl--git-exit-code` | 2 tests | success (0), failure (non-zero) |
| `claude-repl--git-branch-exists-p` | 2 tests | existing branch, non-existent branch |
| `claude-repl--apply-workspace-properties` | 3 tests | nil values skipped, all non-nil stored, empty plist no-op |
| `claude-repl--enqueue-preemptive-prompt` | 3 tests | non-empty prompt stored, nil prompt ignored, empty string ignored |
| `claude-repl--dispatch-prompt-command` | 4 tests | no vterm buffer (enqueue), buffer not ready (enqueue), appends multiple, normalizes branch name |
| `claude-repl--workspace-commands-watch-handler` | 5 tests | ignores non-workspace files, dispatches on created/changed/renamed, ignores delete action |
| `claude-repl--dispatch-workspace-command` | 4 tests | create increments delay, prompt preserves delay, finish preserves delay, unknown type no-op |
| `claude-repl--process-workspace-commands-file` | 3 tests | missing file, staggered create delays, mixed command types |
| `claude-repl--cherry-pick-base` (alias `+dwc/workspace-merge--fork`) | 6 tests | no-annotations fallback, clean chain, already fully merged, growing workspace, deep chain, annotation survives conflict resolution |
| `claude-repl--validate-worktree-creation` | 4 tests | empty name, existing projectile project, existing branch, valid inputs pass |
| `claude-repl--worktree-add-callback` | 2 tests | failure (finalize not called), success (finalize called with args) |
| `claude-repl--worktree-fetch-callback` | 2 tests | calls add-fn on success, calls add-fn on failure |
| `claude-repl--cherry-pick-commits` | 3 tests | empty range signals error, successful cherry-pick, conflict signals error |
| `claude-repl--check-cherry-pick-conflict` | 2 tests | no conflict returns nil, conflict opens magit and signals error |
| `claude-repl--finish-workspace` | 5 tests | non-worktree cleanup, worktree removal, name normalization, vterm kill, skips persp-kill when not listed |

### Partially Tested / Integration-only

| Function | Status | Notes |
|----------|--------|-------|
| `claude-repl--handle-create-command` | Indirectly tested | Tested via `process-workspace-commands-file` integration test; not unit-tested in isolation. Would need stubbing `run-with-timer` to verify timer scheduling and arg forwarding. |
| `claude-repl--handle-prompt-command` | Indirectly tested | Tested via `dispatch-workspace-command`; simple delegation to `dispatch-prompt-command`. |
| `claude-repl--handle-finish-command` | Indirectly tested | Tested via `dispatch-workspace-command`; simple delegation to `finish-workspace`. |

### Not Tested (require Doom/vterm/persp/projectile/magit runtime)

| Function | Reason | Edge Cases to Consider |
|----------|--------|----------------------|
| `claude-repl--register-workspace-commands-watch` | Requires `file-notify-add-watch` runtime; has side effects at load time. | (1) Tears down existing valid watch before creating new one, (2) creates output directory if missing, (3) handles invalid/nil existing watch gracefully. |
| `claude-repl--switch-to-workspace` | Requires `+workspace-switch-to` and `+workspace/switch-to` (Doom). | (1) Primary succeeds, (2) primary fails and fallback succeeds, (3) both fail -- messages but does not error. |
| `claude-repl--register-worktree-ws` | Requires `+workspace-current-name` side effect. | (1) Explicit ws arg used, (2) defaults to `+workspace-current-name`, (3) stores :worktree-p and :project-dir correctly. |
| `claude-repl--setup-worktree-session` | Requires `make-claude-repl-instantiation`, `claude-repl--ensure-session`. | (1) force-bare-metal=t sets :active-env to :bare-metal, (2) force-bare-metal=nil sets :active-env to :sandbox, (3) default-directory is bound to path during ensure-session. |
| `claude-repl--async-git-sentinel` | Requires process objects. | (1) exit with code 0 -> callback(t, output), (2) exit with non-zero -> callback(nil, output), (3) signal -> callback invoked, (4) process buffer killed after callback. |
| `claude-repl--async-git` | Requires `start-process`. | (1) Creates temp buffer, (2) stores callback as process property, (3) sets sentinel. |
| `claude-repl--resolve-worktree-paths` | Requires `claude-repl--git-string` to call real git. | (1) Not in git repo -> user-error, (2) inside worktree (.git is file) -> sibling path, (3) normal repo (.git is dir) -> creates -worktrees directory, (4) nested name extracts dirname correctly. |
| `claude-repl--register-projectile-project` | Requires projectile. | (1) Writes .projectile marker with dirname content, (2) adds to projectile known projects. |
| `claude-repl--finalize-worktree-workspace` | Requires Doom workspace APIs. | (1) Creates workspace, opens buffers, applies properties, starts session, (2) callback invoked when provided, (3) callback nil is handled. |
| `claude-repl--async-worktree-add` | Requires `start-process`. | (1) fork-session-id present -> bases on HEAD, (2) no fork -> bases on origin/master. |
| `claude-repl--do-create-worktree-workspace` | Orchestrator; requires Doom/git runtime. | (1) fork-session-id skips fetch, (2) no fork -> fetches then adds, (3) validation failures propagate. |
| `claude-repl--worktree-creation-switch-callback` | Requires Doom workspace switching + magit. | (1) Switches workspace, (2) opens magit-status at path. |
| `claude-repl--resolve-fork-session-id` | Requires `claude-repl--active-inst` with live session. | (1) raw < 16 -> nil, (2) raw >= 16 with session-id -> returns it, (3) raw >= 16 without session-id -> user-error. |
| `claude-repl-create-worktree-workspace` | Interactive command; requires user input + full runtime. | (1) C-u -> bare-metal, (2) C-u C-u -> fork, (3) C-u C-u C-u -> fork+bare-metal, (4) preemptive prompt present -> no switch callback, (5) blank prompt -> switch callback. |
| `claude-repl--new-workspace` | Requires Doom workspace + magit. | (1) Inside git repo -> uses git root, (2) outside git repo -> uses default-directory. |
| `claude-repl--remove-git-worktree` | Requires real git worktree setup. | (1) Parent has .git -> removes worktree, (2) no .git found -> skips removal, (3) projectile deregistration always happens. |
| `claude-repl--create-worktree-from-command` | Timer callback; requires main-git-root. | (1) Binds default-directory before calling do-create-worktree-workspace. |
| `claude-repl--workspace-branch` | Requires live workspace with :project-dir. | (1) No project-dir -> nil, (2) git fails -> nil, (3) detached HEAD -> returns SHA, (4) normal branch -> returns branch name, (5) empty branch string -> nil. |
| `claude-repl--workspace-merge-do` | Orchestrator; requires live workspaces. | (1) No target branch -> user-error, (2) branch not found in repo -> user-error, (3) successful merge -> finishes target workspace, (4) conflict -> user-error from cherry-pick. |
| `claude-repl-workspace-merge` | Interactive; requires Doom. | (1) No other workspaces -> user-error, (2) uncommitted changes -> user-error from assert-clean, (3) successful merge flow. |
| `claude-repl-workspace-merge-current-into-master` | Interactive; requires Doom. | (1) No master workspace -> user-error, (2) already on master -> user-error, (3) uncommitted changes -> user-error, (4) switches to master then merges. |

### Constants

| Name | Value | Notes |
|------|-------|-------|
| `claude-repl--worktree-stagger-seconds` | 5 | Used in stagger delay tests; verified indirectly. |

### Aliases

| Alias | Target | Tested Via |
|-------|--------|-----------|
| `+dwc/workspace-merge--fork` | `claude-repl--cherry-pick-base` | merge-fork tests |
| `+dwc/workspace->branch` | `claude-repl--workspace-branch` | Not tested |
| `+dwc/workspace-merge--do` | `claude-repl--workspace-merge-do` | Not tested |
| `+dwc/workspace-merge` | `claude-repl-workspace-merge` | Not tested |
| `+dwc/workspace-merge-current-into-master` | `claude-repl-workspace-merge-current-into-master` | Not tested |

## Summary

- **18 functions** fully unit-tested with **62 test cases**
- **3 functions** indirectly tested via integration
- **17 functions** not directly testable without Doom/persp/magit/vterm runtime
- **1 constant**, **5 aliases**

## Recommended Future Work

1. **`claude-repl--resolve-worktree-paths`** -- Could be tested with a real temp git repo (normal and worktree scenarios). Requires mocking `claude-repl--git-string` or using a real git environment.

2. **`claude-repl--workspace-branch`** -- The detached HEAD fallback and error handling paths could be tested with mock `cl-letf` on `claude-repl--git-string` and `claude-repl--ws-get`.

3. **`claude-repl--resolve-fork-session-id`** -- Testable by mocking `claude-repl--active-inst` to return a struct with/without session-id.

4. **`claude-repl--setup-worktree-session`** -- Testable by mocking `make-claude-repl-instantiation` and `claude-repl--ensure-session`; verifies :active-env selection logic.

5. **`claude-repl--async-git-sentinel`** -- Testable by creating a mock process object with appropriate properties and calling the sentinel directly.
