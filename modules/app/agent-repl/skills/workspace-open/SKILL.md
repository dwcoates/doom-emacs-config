---
name: workspace-open
description: Re-open an existing workspace that was previously closed or nuked. Use when the user wants to bring back a workspace whose Claude session and Doom perspective were torn down but whose git worktree still exists on disk, asks to "/workspace-open", or wants to resume work in a workspace they closed earlier. Re-establishes the perspective and resumes the Claude session from the saved session id.
---

# Workspace Open

The user will name one or more existing workspaces to re-open. Your job is to write a JSON file dispatching an open for each named workspace. A downstream text editor will pick up the file and re-establish the editor workspace — recreate the Doom perspective, rehydrate persisted display state, and resume the Claude session from its saved session id. The git worktree on disk (left untouched by close/nuke) is reused as-is.

This skill is the inverse of `/workspace-close`:

- `/workspace-close` tears down a workspace's editor state (kills its Claude session, buffers, and perspective) while leaving the git worktree on disk.
- `/workspace-open` re-establishes that editor state for a workspace that was previously closed or nuked, reusing the on-disk worktree.

Use `/workspace-open` when the user wants to resume a workspace they closed or nuked earlier. The workspace's git worktree and per-project state must still exist on disk — a workspace whose worktree was removed by `/workspace-finish` cannot be re-opened.

Do NOT attempt to open the workspace yourself in any way. Under NO circumstances. The handling of the open is EXCLUSIVELY the responsibility and right of downstream consumers. Your EXCLUSIVE job is to generate the aforementioned JSON file, and NOTHING else. To that end, no code, git operations, or any other files or mutating effects should be done, either.

## Steps

1. **Interpret** the user's request to identify which workspaces to open (by name, e.g. `DWC/feature-one`).

2. **Write the commands** by piping JSON to `run.sh` using the Bash tool:
   ```bash
   bash /home/claude/.claude/skills/workspace-open/run.sh << 'EOF'
   [
     {"type": "open", "workspace": "DWC/feature-one", "git_root": "/abs/path/to/repo"},
     {"type": "open", "workspace": "DWC/feature-two"}
   ]
   EOF
   ```
   - `git_root` is OPTIONAL. Include it (the absolute repo root — e.g. the current repository's root) so the on-disk worktree can be located when the workspace's in-memory registry entry was already dropped. Omit it only when it is genuinely unknown.
   - If the command fails due to missing `uuidgen`, **stop immediately** and tell the user they need to rebuild the sandbox image by running `.claude/install.sh`.

3. **Tell the user** which workspaces were targeted for open.
