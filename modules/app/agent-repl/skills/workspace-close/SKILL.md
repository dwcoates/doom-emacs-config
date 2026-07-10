---
name: workspace-close
description: Close an existing editor workspace without merging it. Use when the user wants to dispatch a close for one or more named workspaces (kill its Claude session, buffers, and Doom perspective) without cherry-picking commits anywhere, asks to "/workspace-close", or wants to discard a workspace's editor state while leaving the git worktree on disk untouched.
---

# Workspace Close

The user will name one or more existing workspaces to close. Your job is to write a JSON file dispatching a close for each named workspace. A downstream text editor will pick up the file and tear down the editor workspace — kill the Claude vterm session, kill the workspace's buffers, kill the Doom perspective, and drop the workspace from the in-memory registry. The git worktree on disk is intentionally left alone.

This skill is the editor-state-only counterpart to `/workspace-merge`:

- `/workspace-merge` cherry-picks the workspace's commits into its source and *then* closes the editor workspace.
- `/workspace-close` does ONLY the close half — no cherry-pick, no tag, no config reload, no focus switch, no worktree removal from disk.

Use `/workspace-close` when the user wants to abandon a workspace's editor state without merging it (e.g. it's no longer needed, or its commits have already been delivered some other way). Use `/workspace-merge` when the workspace's commits still need to land in its source. Use `/workspace-finish` (the `finish` command type, dispatched by Emacs internals) when the on-disk git worktree should also be removed.

Do NOT attempt to close the workspace yourself in any way. Under NO circumstances. The handling of the close is EXCLUSIVELY the responsibility and right of downstream consumers. Your EXCLUSIVE job is to generate the aforementioned JSON file, and NOTHING else. To that end, no code, git operations, or any other files or mutating effects should be done, either.

## Steps

1. **Interpret** the user's request to identify which workspaces to close (by name, e.g. `DWC/feature-one`).

2. **Write the commands** by piping JSON to `run.sh` using the Bash tool:
   ```bash
   bash /home/claude/.claude/skills/workspace-close/run.sh << 'EOF'
   [
     {"type": "close", "workspace": "DWC/feature-one"},
     {"type": "close", "workspace": "DWC/feature-two"}
   ]
   EOF
   ```
   If the command fails due to missing `uuidgen`, **stop immediately** and tell the user they need to rebuild the sandbox image by running `.claude/install.sh`.

3. **Tell the user** which workspaces were targeted for close.
