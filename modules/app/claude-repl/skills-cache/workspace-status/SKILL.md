---
name: workspace-status
description: Read the live state of one or more peer workspaces. Use when the user wants to check what another workspace's Claude session is doing, asks to "/workspace-status", or asks something like "is workspace X still running" / "which workspaces are idle".
---

# Workspace Status

A live JSON snapshot of every claude-repl workspace is published by the
host editor to `~/.claude/emacs/workspace-status.json` and refreshed
once per second.  Your job for this skill is to read that file and
report the relevant fields back to the user — never to mutate it and
never to act on behalf of another workspace.

## File shape

```json
{
  "updated_at": "2026-05-11T15:30:00-0700",
  "workspaces": {
    "DWC/feature-one": {
      "claude_state": "thinking",
      "repl_state":   "active",
      "project_dir":  "/path/to/project",
      "source_ws_dir":"/path/to/source",
      "priority":     "p1",
      "last_prompt_summary": "Fix the bug in auth",
      "git_clean":    "dirty",
      "done_acked":   false
    }
  }
}
```

State vocabulary:

- `claude_state`: `init`, `idle`, `thinking`, `done`, `permission`, `stop-failed`, or `null` (no session).
- `repl_state`: `active`, `inactive`, `hidden`, `dead`, or `null`.
- `git_clean`: `clean`, `dirty`, or `null` (not yet polled).

## Steps

1. **Identify** which workspace(s) the user is asking about.  If they
   named one (e.g. `DWC/feature-one`), look it up directly.  If they
   asked a categorical question ("which workspaces are still
   thinking?"), filter the `workspaces` object.

2. **Read** via `jq` against `~/.claude/emacs/workspace-status.json`.
   Common queries:
   ```bash
   # Full status for one workspace
   jq '.workspaces."DWC/feature-one"' ~/.claude/emacs/workspace-status.json

   # Just the claude_state field
   jq -r '.workspaces."DWC/feature-one".claude_state' ~/.claude/emacs/workspace-status.json

   # List every registered workspace name
   jq -r '.workspaces | keys[]' ~/.claude/emacs/workspace-status.json

   # Filter: every workspace currently :thinking
   jq '.workspaces | to_entries | map(select(.value.claude_state == "thinking")) | from_entries' \
      ~/.claude/emacs/workspace-status.json

   # Freshness — when was the snapshot last written?
   jq -r '.updated_at' ~/.claude/emacs/workspace-status.json
   ```

3. **Sanity-check freshness** if the user's question depends on the
   data being current.  If `updated_at` is more than ~10 seconds old,
   the editor may have been killed; mention that in your reply so the
   user knows the data is stale.

4. **Report** the answer to the user.  Keep it terse — the user is
   asking a status question, not requesting an essay.

## What NOT to do

- Do not write to `~/.claude/emacs/workspace-status.json`.  It is
  published by the editor; any edit will be overwritten on the next
  poll tick.
- Do not dispatch prompts or merges from this skill — use
  `/workspace-update` or `/workspace-merge` for those.
- Do not infer state from anywhere other than this file.  Reading the
  other workspace's terminal output, project files, or `.claude/`
  state is out of scope.
