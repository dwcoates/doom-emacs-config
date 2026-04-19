# Fallback and Default Logic Report: `worktree.el`

**Total instances: 55**

---

## Section 1: Variable Defaults (4 instances)

| # | Line | Variable | Default | Consequence |
|---|---|---|---|---|
| 1 | 7 | `+dwc/workspace-history` | `nil` | No default merge target suggested |
| 2 | 13 | `claude-repl-workspace-initial-buffers` | `nil` | No initial buffers opened |
| 3 | 41 | `claude-repl--workspace-generation-watch` | `nil` | No watch to clean up on first load |
| 4 | 518-519 | `claude-repl--worktree-stagger-seconds` | `5` | Fixed stagger, not configurable |

---

## Section 2: `or` Fallback Chains (7 instances)

### Instance 5: Workspace resolution
- **Line:** 128
- **Code:** `(or ws (+workspace-current-name))`

### Instance 6: Base-commit context-sensitive default
- **Line:** 318
- **Code:** `(or base-commit (if fork-session-id "HEAD" "origin/master"))`
- **Three-tier:** explicit -> "HEAD" (fork) -> "origin/master" (fresh)

### Instance 8: Root defaulting
- **Line:** 422
- **Code:** `(or (claude-repl--git-root) default-directory)`
- **Risk:** Workspace could be rooted at arbitrary directory outside git.

### Instance 10: Cherry-pick base fallback
- **Line(s):** 632-635
- **Code:** `(or (cl-find-if ... incorporated) (merge-base HEAD target-branch))`
- **Consequence:** First-time merge without cherry-pick annotations replays ALL commits since divergence.

### Instance 11: Master dir or hard error
- **Line(s):** 758-759
- **Code:** `(or (claude-repl--ws-get master-ws :project-dir) (user-error ...))`

---

## Section 3: `condition-case` / `ignore-errors` (3 instances)

### Instance 12: Double-fallback workspace switch
- **Line(s):** 98-109
- **Chain:** `+workspace-switch-to` -> `+workspace/switch-to` -> silent failure
- **Risk:** Downstream code proceeds as if switch succeeded.

### Instance 13: `ignore-errors` around `claude-repl--active-inst`
- **Line:** 501
- **Converts:** Any error into a clean "no session ID" error message.

### Instance 14: `condition-case` around fork resolution
- **Line(s):** 533-539
- **Falls back TO:** `nil` -- workspace creation is SKIPPED entirely (anti-silent-degradation).

---

## Section 4: `&optional` Parameters (3 instances)

### Instance 16: Six optional parameters in `do-create-worktree-workspace`
- **Line:** 305
- Each nil triggers specific behavior:
  - `force-bare-metal` nil -> `:sandbox` (default Docker)
  - `fork-session-id` nil -> `"origin/master"` base
  - `preemptive-prompt` nil -> no enqueue
  - `callback` nil -> no post-creation action
  - `priority` nil -> not stored
  - `base-commit` nil -> context-sensitive default

---

## Section 5: Guards and Dispatch (18 instances)

Key instances:

| # | Line | Context | Falls back TO |
|---|---|---|---|
| 19 | 34-38 | File existence for initial buffers | Log warning, skip file |
| 21 | 219-224 | Preemptive prompt enqueue | Skip if empty/nil |
| 22 | 384 | `unless has-preemptive` callback | nil (background creation) |
| 25 | 336-345 | `cond` dispatch for create | Three branches: fork / remote-fetch / direct |
| 26 | 436-445 | `cond` dispatch for prompt | Ready -> send immediately; not ready -> enqueue |
| 27 | 567-579 | `cond` dispatch for commands | Unknown type silently skipped |
| 29 | 644-654 | Branch resolution | `when-let*` with three nil conditions |
| 30 | 650-654 | Detached HEAD handling | Returns raw SHA instead of branch name |
| 34 | 186-191 | Worktree parent path | In worktree -> parent dir; normal repo -> create sibling dir |
| 36 | 139 | Environment selection | `(if force-bare-metal :bare-metal :sandbox)` -- sandbox is default |

---

## Section 6: `alist-get` with nil Defaults (3 instances)

Lines 529-531: `prompt`, `priority`, `fork_from` all default to `nil` from JSON commands.

---

## Section 7: Hardcoded Values (6 instances)

| Value | Line | Purpose |
|---|---|---|
| `"master"` | 747 | Hardcoded master workspace name |
| `"origin/master"` | 318 | Default base branch |
| `"HEAD"` | 318 | Fork base |
| `"~/.claude/output/"` | 63 | Watch directory |
| `"origin/"` | 339 | Prefix triggering fetch |
| `5` sec | 518 | Stagger delay |

---

## Section 8: Silent No-ops (5 instances)

| # | Line | Context | Consequence |
|---|---|---|---|
| 49 | 207-208 | `apply-workspace-properties` | nil values silently dropped (cannot clear a property) |
| 50 | 249 | `when callback` | No-op for background creation |
| 51 | 482-483 | `when (member ws ...)` | Skip persp-kill if already gone |
| 52 | 486-487 | Worktree removal guard | Three conditions must be true |
| 53 | 455-460 | `if git-root` in remove | Skip git worktree remove if no git root |

---

## Section 9: Merge Default Selection (2 instances)

### Instance 54: Default from workspace history
- **Line(s):** 726-729
- **Falls back TO:** `nil` (no default pre-selected)

### Instance 55: Prompt with/without default
- **Line(s):** 731-736
- Different prompt text and `completing-read` behavior based on whether default exists.

---

## Most Consequential

1. **Instance 10** (cherry-pick base merge-base fallback) -- changes which commits are replayed
2. **Instance 12** (double condition-case in switch) -- can silently fail to switch workspaces
3. **Instance 6/44** (base-commit defaulting to `"origin/master"`) -- hardcodes remote/branch assumptions
4. **Instance 8** (root defaulting to default-directory) -- arbitrary directory risk
5. **Instance 14** (fork resolution failure) -- the explicit ANTI-fallback that refuses to silently degrade
