---
name: workspace-annotate
description: Attach metadata to a workspace for the downstream editor (Emacs) to consume. Use when Claude wants the user to run a command, paste text, or otherwise act on something — instead of dumping it inline. Examples: "/workspace-annotate", "put this command in a vterm", "copy this to my clipboard".
---

# Workspace Annotate

The user is running an editor (Emacs) that watches `~/.claude/output/` for JSON files emitted by Claude.  Your job is to write one such file describing **annotations** that apply to a workspace.  A downstream consumer picks up the file and acts on each annotation — for example, opening a vterm pre-populated with a command, or copying text to the system clipboard.

Use this skill instead of:

- Telling the user to copy/paste a command from your message
- Asking the user to manually run something you've described in prose
- Trying to invoke `pbcopy` / `xclip` / similar yourself (these are usually unavailable in the sandbox and fragile on the host)

Do NOT attempt to interact with the workspace yourself in any way.  Under NO circumstances.  The handling of annotation dispatch is EXCLUSIVELY the responsibility and right of downstream consumers.  Your EXCLUSIVE job is to generate the JSON file and tell the user what was annotated.  No code, no other files, no mutating effects.

## Annotation types

Each entry in the JSON array has a `type` field.  Known types:

- `command_to_run` — Claude has a command it wants the user to execute.  Fields:
  - `workspace` (optional, string) — workspace name, e.g. `DWC/feature-one`.  Omit when the annotation is ambient / not tied to a specific workspace.
  - `command` (required, string) — the literal command to execute.
  - `note` (optional, string) — short human-readable explanation of what the command does or why.

- `clipboard` — Claude wants text on the user's system clipboard.  Fields:
  - `workspace` (optional, string) — as above.
  - `text` (required, string) — the text to copy.
  - `note` (optional, string) — short human-readable explanation.

If you're unsure whether one of the existing types fits, ask the user before inventing a new one.  New types must be coordinated with the downstream consumer or they will be ignored.

## Steps

1. **Interpret** the user's request to identify:
   - What annotation type fits (`command_to_run`, `clipboard`, ...)
   - Which workspace is targeted, if any
   - The payload (command string, text, etc.)

2. **Write the annotations** by piping JSON to `run.sh` using the Bash tool:
   ```bash
   bash /home/claude/.claude/skills/workspace-annotate/run.sh << 'EOF'
   [
     {"type": "command_to_run", "workspace": "DWC/feature-one", "command": "make test", "note": "run the unit tests"},
     {"type": "clipboard", "text": "some text the user wants on their clipboard"}
   ]
   EOF
   ```
   If the command fails due to missing `uuidgen`, **stop immediately** and tell the user they need to rebuild the sandbox image by running `.claude/install.sh`.

3. **Tell the user** what was annotated — name each annotation type, the targeted workspace (if any), and a short summary of the payload.  Keep it terse: one bullet per annotation is enough.
