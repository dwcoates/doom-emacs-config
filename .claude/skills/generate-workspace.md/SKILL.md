---
name: workspace-generation
description: Generate git branch/worktree names for planned work. Use when the user describes work they want to do and wants workspace or branch names generated, asks to "/workspace-generation", or wants to plan out branches for upcoming tasks.
---

# Workspace Generation

The user will describe work they want to do across one or more workspaces in plain English. Your job is to generate practical git branch/worktree names for each workspace, write them to a JSON file, and confirm. A downstream text editor is going to use this file you generate to create workspaces for itself (which will have a corresponding branch/worktree it'll create for them as well).

Do NOT attempt to generate git branches or worktrees yourself in git. Under NO circumstances. The handling of branch/worktree generation is EXCLUSIVELY the responsibility and right of downstream consumers. Your EXCLUSIVE job is to generate the aforementioned JSON file, and NOTHING else. To that end, so code or any other files or mutating effects should be done, either. 

## Steps

1. **Interpret** the user's description as a description of the branches or a description of the process to generate the branch names. 
  - **EXAMPLE**: "one for each of the skipped tests listed in the test suite output" should be interpreted as the following process by you
    - first -> identify how to run the corresponding tests
    - second -> run the tests
    - third -> enumerate the items found in the output
    - fourth -> spin up one agent item, and determine the corresponding branch names
  - **NOTE**: attempt to spin up agents dedicated to each soon-to-be branch name when possible

2. **Generate branch names** for each workspace:
  - Branch Names should be short, lowercase, hyphen-separated slugs — not long descriptions. 
    - E.g., Longer descriptions take up lots of space in the editor! Should not be longer than 3 words (after the prefixes). 
  - Branch names should start with `DWC/` prefix. 
    - E.g., `DWC/hello-world`
  - If the user supplied a Jira ticket, be sure to include it in the name as a prefix using the format `DWC/<ticket-id>/<feature-name>` 
    - E.g., `DWC/CV-100/fix-login`. 

3. **Ensure the output directory exists** by running:
   ```
   mkdir -p ~/.claude/output
   ```

4. **Write the names** as a JSON array to `~/.claude/output/workspace_generation.json` using the Bash tool. For example:
   ```bash
   cat > ~/.claude/output/workspace_generation.json << 'EOF'
   ["DC/feature-one", "DC/feature-two"]
   EOF
   ```

5. **Tell the user** the names that were written and that the workspaces will be created automatically.
