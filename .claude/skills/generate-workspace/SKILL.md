---
name: workspace-generation
description: Generate git branch/worktree names for planned work. Use when the user describes work they want to do and wants workspace or branch names generated, asks to "/workspace-generation", or wants to plan out branches for upcoming tasks.
---

# Workspace Generation

The user will describe work they want to do across one or more workspaces in plain English. Your job is to generate practical git branch/worktree names for each workspace, write them to a JSON file, and confirm. A downstream text editor is going to use this file you generate to create workspaces for itself (which will have a corresponding branch/worktree it'll create for them as well).

Do NOT attempt to generate git branches or worktrees yourself in git. Under NO circumstances. The handling of branch/worktree generation is EXCLUSIVELY the responsibility and right of downstream consumers. Your EXCLUSIVE job is to generate the aforementioned JSON file, and NOTHING else. To that end, no code or any other files or mutating effects should be done, either.

## Gathering Context via GNS

When the user's request references external resources (Slack messages, GitHub PRs, etc.), use the `gns` CLI to fetch context before generating workspaces. Use `gns --help` and `gns <subcommand> --help` for full details beyond what's listed here.

### Slack

Read a Slack conversation (root message + all replies) given a link:
```bash
gns slack convo <slack_link> --all --json
```

Parse a Slack link into channel ID and timestamps:
```bash
gns slack link <slack_url> --json
```

Read thread replies by channel and timestamp:
```bash
gns slack thread <channel_id> <thread_ts> --all --json
```

Search Slack messages:
```bash
gns slack search "<query>" --count 20 --json
```

Look up a Slack user by ID:
```bash
gns slack user get <user_id> --json
```

Look up a Slack channel by name:
```bash
gns slack channel lookup <name> --json
```

Get your own identity:
```bash
gns whoami --json
```

### GitHub

Use the `gh` CLI for GitHub operations:
```bash
gh pr view <number> --json title,body,url,files
gh issue view <number> --json title,body,url
```

### Knowledge Base

Search the company knowledge base:
```bash
gns search "<query>" --limit 10 --json
```

### Following Links

When analyzing Slack threads, **follow all links exhaustively**:
- **Slack links** (`chesscom.slack.com/archives/...`): use `gns slack convo <link>`
- **GitHub PR/issue links** (`github.com/org/repo/pull/N`): use `gh pr view` or `gh issue view`
- **Jira links**: extract the ticket ID for branch naming
- **Other URLs**: use `WebFetch` if available, or note them for the workspace prompt

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

3. **Determine commands**: Build an array of typed command objects.
  - Always emit one `"create"` entry per workspace.
  - If the user **explicitly** asked to send a message to the generated workspaces, attach it as an inline `"prompt"` field on each `"create"` entry. Do **not** emit separate `"prompt"` entries for newly created workspaces — the inline form is preferred.
  - Separate `"prompt"` entries (without a `"create"`) are only for targeting **existing** workspaces by name.
  - If the user specifies a priority for a workspace, include a `"priority"` field on the `"create"` entry. Valid values are `"p05"`, `"p1"`, `"p2"`, `"p3"`. This displays a priority badge image in the tab-bar. The field is optional — omit it if no priority is specified.

   Example — create with initial prompt (inline, preferred):
   ```json
   [
     {"type": "create", "name": "DWC/feature-one", "prompt": "hello world"},
     {"type": "create", "name": "DWC/feature-two", "prompt": "hello world"}
   ]
   ```

   Example — create without prompt:
   ```json
   [
     {"type": "create", "name": "DWC/feature-one"},
     {"type": "create", "name": "DWC/feature-two"}
   ]
   ```

   Example — create with priority:
   ```json
   [
     {"type": "create", "name": "DWC/urgent-fix", "priority": "p05"},
     {"type": "create", "name": "DWC/feature-one", "priority": "p1"},
     {"type": "create", "name": "DWC/nice-to-have", "priority": "p3"}
   ]
   ```

   Example — prompt existing workspaces only:
   ```json
   [
     {"type": "prompt", "workspace": "DWC/feature-one", "prompt": "hello world"}
   ]
   ```

4. **Write the commands** by piping JSON to `run.sh` using the Bash tool:
   ```bash
   bash /home/claude/.claude/skills/generate-workspace/run.sh << 'EOF'
   [
     {"type": "create", "name": "DWC/feature-one"},
     {"type": "create", "name": "DWC/feature-two"}
   ]
   EOF
   ```
   If the command fails due to missing `uuidgen`, **stop immediately** and tell the user they need to rebuild the sandbox image by running `.claude/install.sh`.

6. **Tell the user** the workspace names that were written and that the workspaces will be created automatically. If prompt commands were included, mention that the prompts will be dispatched once each session is ready.
