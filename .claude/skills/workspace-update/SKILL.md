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
