---
name: workspace-merge
description: Merge an existing workspace into its source. Use when the user wants to merge one or more named workspaces back into the workspace they were created from (the equivalent of running `SPC TAB M` in Doom Emacs while focused on each workspace), asks to "/workspace-merge", or wants to dispatch a merge to existing workspaces.
---

# Workspace Merge

The user will name one or more existing workspaces to merge. Your job is to write a JSON file dispatching a merge for each named workspace. A downstream text editor will pick up the file and perform the equivalent of `SPC TAB M` on each — cherry-picking that workspace's commits into the workspace it was originally created from (its `:source-ws-dir`), then finishing the worktree.

Do NOT attempt to merge the workspaces yourself in any way — **with the two explicit carve-outs below**. The handling of the merge is EXCLUSIVELY the responsibility and right of downstream consumers. Your EXCLUSIVE job is to generate the aforementioned JSON file, and NOTHING else. **Carve-outs**: (1) when the current workspace is one of the named merge targets, it MUST be rebased onto the merge target branch before dispatch (see step 2) — the goal is that downstream consumers receive conflict-free commits and do not have to reason about conflicts themselves; (2) gns-sockets subscriptions bound to the current Claude session MUST be closed before the dispatch file is written when the current workspace is one of the named merge targets (see step 3). Without the sockets cleanup, the downstream editor tears down the workspace while leaving live subscriptions wired to a dead session, dangling the daemon's state and any associated hooks.

## Arguments

| Argument | Behaviour |
|---|---|
| `<workspace>...` | One or more workspace names to merge (e.g. `DWC/feature-one`). |
| `--pr-was-merged` | The named workspace's PR has **already merged into `origin/master`**. The dispatch JSON carries `"pr_was_merged": true` for each merge entry, and the downstream editor advances local `master` to `origin/master` (fast-forward) and closes the workspace, instead of cherry-picking the branch's commits. **When `--pr-was-merged` is passed, SKIP the step-2 rebase entirely** — the commits are already upstream, so there is nothing to rebase or cherry-pick. The callers `create-or-update-pr` and `check-cicd` pass this flag because they only invoke `/workspace-merge` after confirming the PR merged. |

## Steps

1. **Interpret** the user's request to identify which workspaces to merge (by name, e.g. `DWC/feature-one`).

2. **Rebase onto the merge target (current workspace only).** Skip this step if the current workspace is NOT among the named merge targets identified in step 1. **Also skip this step entirely when `--pr-was-merged` was passed** — the PR has already merged into `origin/master`, so the commits are already upstream and a rebase would only replay already-merged commits (the exact conflict the `--pr-was-merged` path exists to avoid).

   a. Determine the merge target branch:
      - Read `.claude/emacs/state.el` under the current worktree root (obtain the root via `git rev-parse --show-toplevel`).
      - Extract the `:source-ws-dir` value from the plist.
      - Run `git -C <source-ws-dir> branch --show-current` to get the merge target branch name.

   b. Run the rebase:
      ```bash
      git rebase <merge-target-branch>
      ```
      - EXIT CODE 0 (clean, no conflicts): continue to step 3.
      - If the rebase pauses with conflicts: enter conflict resolution (step 2c).

   c. For each conflict hunk, examine the conflict markers alongside the replayed commit's message and any surrounding context. Determine whether the motivations of the two conflicting commits are **orthogonal** or **conceptually in conflict**:
      - **Orthogonal motivations** (the commits address independent concerns that happen to touch the same lines): resolve the conflict by incorporating both sets of changes, then run `git add <file>`. Once all files in the current hunk are staged, run `git rebase --continue`. Repeat for each subsequent conflict that arises.
      - **Conceptually conflicting motivations** (the commits represent genuinely competing design decisions or mutually exclusive changes): run `git rebase --abort` IMMEDIATELY, surface the specific conflict along with both commit motivations to the user, and STOP — do not proceed to step 3 or beyond.

3. **Close any gns-sockets subscriptions bound to this Claude session** before writing the dispatch file. The downstream editor will tear the workspace down; any subscription whose `session_id` equals the current Claude session would otherwise outlive the session that owned it. Pass every named workspace from step 1 — the script itself decides whether the current branch is among them and no-ops when it is not. Subscriptions for workspaces not currently checked out are bound to different Claude sessions and are not reachable from here.

   ```bash
   bash .claude/skills/workspace-merge/run.sh \
     --close-current-session-sockets DWC/feature-one DWC/feature-two
   ```

   - EXIT CODE 0: success. Stdout may be empty (no-op) or one closed subscription id per line. Capture the ids so step 5 can mention them, then continue to step 4.
   - EXIT CODE 2: script error (e.g. no workspaces passed). Stop and surface the error to the user.

4. **Write the commands** by piping JSON to `run.sh --emit-commands` using the Bash tool:
   ```bash
   bash .claude/skills/workspace-merge/run.sh --emit-commands << 'EOF'
   [
     {"type": "merge", "workspace": "DWC/feature-one"},
     {"type": "merge", "workspace": "DWC/feature-two"}
   ]
   EOF
   ```

   **When `--pr-was-merged` was passed, add `"pr_was_merged": true` to every merge entry** so the downstream editor advances local `master` from `origin/master` instead of cherry-picking:
   ```bash
   bash .claude/skills/workspace-merge/run.sh --emit-commands << 'EOF'
   [
     {"type": "merge", "workspace": "DWC/feature-one", "pr_was_merged": true}
   ]
   EOF
   ```

   - EXIT CODE 0: success. Continue to step 5.
   - EXIT CODE 2: script error. If the error is missing `uuidgen`, **stop immediately** and tell the user they need to rebuild the sandbox image by running `.claude/install.sh`. Otherwise surface the error and stop.

5. **Tell the user** which workspaces were targeted for merge, and (if step 3 printed any subscription ids on stdout) which subscriptions were closed.

## Notes

- **`run.sh` is a black box.** Do not read, edit, or reason about its internals. Surface any `run.sh` failure to the user and stop.
- **No self-remediation of `run.sh` failures.** If a subcommand exits non-zero, surface the error and stop — do not retry, do not work around, do not invoke a different path.
- **CRITICAL: The only permitted mutating git operations are** the rebase in step 2 and its associated `git add` / `git rebase --continue` / `git rebase --abort` operations. No other mutating git commands are permitted — no resets, checkouts that discard work, pushes, or any other git mutations.
- **CRITICAL: the only side effects this skill performs are** the optional rebase in step 2 and the gns-sockets cleanup in step 3. Everything else either reads state or writes the single dispatch JSON via `run.sh --emit-commands`. Do not add additional mutating effects.
