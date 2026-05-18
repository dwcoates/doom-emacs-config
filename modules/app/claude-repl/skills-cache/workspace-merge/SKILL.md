---
name: workspace-merge
description: Merge an existing workspace into its source. Use when the user wants to merge one or more named workspaces back into the workspace they were created from (the equivalent of running `SPC TAB M` in Doom Emacs while focused on each workspace), asks to "/workspace-merge", or wants to dispatch a merge to existing workspaces.
---

# Workspace Merge

The user will name one or more existing workspaces to merge. Your job is to write a JSON file dispatching a merge for each named workspace. A downstream text editor will pick up the file and perform the equivalent of `SPC TAB M` on each — cherry-picking that workspace's commits into the workspace it was originally created from (its `:source-ws-dir`), then finishing the worktree.

Do NOT attempt to merge the workspaces yourself in any way. Under NO circumstances. The handling of the merge is EXCLUSIVELY the responsibility and right of downstream consumers. Your EXCLUSIVE job is to generate the aforementioned JSON file, and NOTHING else. To that end, no code, git operations, or any other files or mutating effects should be done, either — **with one explicit carve-out**: gns-sockets subscriptions bound to the current Claude session MUST be closed before the dispatch file is written when the current workspace is one of the named merge targets (see step 2). Without that cleanup the downstream editor tears down the workspace while leaving live subscriptions wired to a dead session, dangling the daemon's state and any associated hooks.

## Steps

1. **Interpret** the user's request to identify which workspaces to merge (by name, e.g. `DWC/feature-one`).

2. **Close any gns-sockets subscriptions bound to this Claude session** before writing the dispatch file. The downstream editor will tear the workspace down; any subscription whose `session_id` equals the current Claude session would otherwise outlive the session that owned it. Pass every named workspace from step 1 — the script itself decides whether the current branch is among them and no-ops when it is not. Subscriptions for workspaces not currently checked out are bound to different Claude sessions and are not reachable from here.

   ```bash
   bash /home/claude/.claude/skills/workspace-merge/run.sh \
     --close-current-session-sockets DWC/feature-one DWC/feature-two
   ```

   - EXIT CODE 0: success. Stdout may be empty (no-op) or one closed subscription id per line. Capture the ids so step 4 can mention them, then continue to step 3.
   - EXIT CODE 2: script error (e.g. no workspaces passed). Stop and surface the error to the user.

3. **Write the commands** by piping JSON to `run.sh --emit-commands` using the Bash tool:
   ```bash
   bash /home/claude/.claude/skills/workspace-merge/run.sh --emit-commands << 'EOF'
   [
     {"type": "merge", "workspace": "DWC/feature-one"},
     {"type": "merge", "workspace": "DWC/feature-two"}
   ]
   EOF
   ```

   - EXIT CODE 0: success. Continue to step 4.
   - EXIT CODE 2: script error. If the error is missing `uuidgen`, **stop immediately** and tell the user they need to rebuild the sandbox image by running `.claude/install.sh`. Otherwise surface the error and stop.

4. **Tell the user** which workspaces were targeted for merge, and (if step 2 printed any subscription ids on stdout) which subscriptions were closed.

## Notes

- **`run.sh` is a black box.** Do not read, edit, or reason about its internals. Surface any `run.sh` failure to the user and stop.
- **No self-remediation of `run.sh` failures.** If a subcommand exits non-zero, surface the error and stop — do not retry, do not work around, do not invoke a different path.
- **No mutating git commands.** Do not run rebases, resets, checkouts that discard work, pushes, or any other mutating git operation in this skill. The merge itself is the downstream editor's job.
- **CRITICAL: the only side effect this skill performs is the gns-sockets cleanup in step 2.** Everything else either reads state or writes the single dispatch JSON via `run.sh --emit-commands`. Do not add additional mutating effects.
