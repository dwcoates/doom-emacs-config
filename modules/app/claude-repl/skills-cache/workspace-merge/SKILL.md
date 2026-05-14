---
name: workspace-merge
description: Merge an existing workspace into its source. Use when the user wants to merge one or more named workspaces back into the workspace they were created from (the equivalent of running `SPC TAB M` in Doom Emacs while focused on each workspace), asks to "/workspace-merge", or wants to dispatch a merge to existing workspaces.
---

# Workspace Merge

The user will name one or more existing workspaces to merge. Your job is to write a JSON file dispatching a merge for each named workspace. A downstream text editor will pick up the file and perform the equivalent of `SPC TAB M` on each — cherry-picking that workspace's commits into the workspace it was originally created from (its `:source-ws-dir`), then finishing the worktree.

Do NOT attempt to merge the workspaces yourself in any way. Under NO circumstances. The handling of the merge is EXCLUSIVELY the responsibility and right of downstream consumers. Your EXCLUSIVE job is to generate the aforementioned JSON file, and NOTHING else. To that end, no code, git operations, or any other files or mutating effects should be done, either.

## Steps

1. **Interpret** the user's request to identify which workspaces to merge (by name, e.g. `DWC/feature-one`).

2. **Write the commands** by piping JSON to `run.sh` using the Bash tool:
   ```bash
   bash /home/claude/.claude/skills/workspace-merge/run.sh << 'EOF'
   [
     {"type": "merge", "workspace": "DWC/feature-one"},
     {"type": "merge", "workspace": "DWC/feature-two"}
   ]
   EOF
   ```
   If the command fails due to missing `uuidgen`, **stop immediately** and tell the user they need to rebuild the sandbox image by running `.claude/install.sh`.

3. **Tell the user** which workspaces were targeted for merge.
