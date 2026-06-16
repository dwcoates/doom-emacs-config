---
name: workspace-update
description: Send a prompt to one or more existing workspaces. Use when the user wants to dispatch a message to named workspaces, asks to "/workspace-update", or wants to send a follow-up prompt to workspaces that already exist.
---

# Workspace Update

The user will name one or more existing workspaces and provide a prompt to send to each. Your job is to write a JSON file dispatching that prompt to each named workspace. A downstream text editor will pick up the file and deliver the prompt to each workspace's Claude session.

Do NOT attempt to interact with the workspaces yourself in any way. Under NO circumstances. The handling of prompt dispatch is EXCLUSIVELY the responsibility and right of downstream consumers. Your EXCLUSIVE job is to generate the aforementioned JSON file, and NOTHING else. To that end, no code or any other files or mutating effects should be done, either.

## Steps

1. **Interpret** the user's request to identify:
   - Which workspaces to target (by name, e.g. `DWC/feature-one`)
   - What prompt to send to each

2. **Write the commands** by piping JSON to `run.sh` using the Bash tool:
   ```bash
   bash /home/claude/.claude/skills/workspace-update/run.sh << 'EOF'
   [
     {"type": "prompt", "workspace": "DWC/feature-one", "prompt": "hello world"},
     {"type": "prompt", "workspace": "DWC/feature-two", "prompt": "hello world"}
   ]
   EOF
   ```
   If the command fails due to missing `uuidgen`, **stop immediately** and tell the user they need to rebuild the sandbox image by running `.claude/install.sh`.

4. **Tell the user** which workspaces were targeted and what prompt was sent.

## Handing off uncommitted changes via the stash (`stash-as-transfer-mechanism`)

When the source workspace has uncommitted working-tree edits that a target workspace should pick up, you can transfer them through the git stash without committing the WIP, sharing a patch file, or copying files by hand. Git worktrees attached to the same repository share one underlying object store — including the stash list — so a stash created in the source workspace is reachable from the target worktree.

The flow:

1. In the **source** workspace, park the relevant changes and capture the stash's stable SHA:
   ```bash
   git stash push -m "handoff: feature-one wip" -- path/to/file.cpp path/to/other.h
   git rev-parse stash@{0}    # → e.g. 2bf11a4d... — copy this SHA
   ```
2. Embed that SHA in the dispatched prompt so applying it is the target's first step:
   ```json
   [
     {"type": "prompt", "workspace": "DWC/feature-one", "prompt": "First, run `git stash apply 2bf11a4d` to materialize the parked working-tree changes, then <the actual task>."}
   ]
   ```
3. The **target** workspace runs `git stash apply 2bf11a4d` as its first action. `apply` (not `pop`) materializes the changes into its own worktree while leaving the stash entry intact, so the source workspace's stash is undisturbed.

**Always use the stash's SHA, never `stash@{N}`.** `stash@{N}` is a positional index, not a stable handle — any subsequent `git stash push` in the source workspace shifts every index by one, so `stash@{0}` in the prompt may point at a different (or nonexistent) stash by the time the target workspace runs. The SHA from `git rev-parse stash@{0}` is stable and is the only reliable way to refer to the parked changes from another worktree's prompt.

This is the path for already-running workspaces. For bootstrapping a freshly-spawned workspace with uncommitted changes at creation time, the `generate-workspace` skill documents the equivalent flow.
