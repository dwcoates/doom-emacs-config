---
name: workspace-profile
description: Enable or disable the running Emacs's profiler. Use when the user wants to start/stop CPU/memory profiling in their editor, asks to "/workspace-profile", or wants to capture a profiler report for a slow editor operation.
---

# Workspace Profile

The user is running an editor (Emacs) that watches `~/.claude/output/` for JSON command files. Your job is to write one such file dispatching a profiler toggle to the editor. A downstream consumer picks up the file and either starts the Emacs profiler or stops it (and pops up `profiler-report`).

This skill is the editor-instrumentation counterpart to the rest of the workspace-command skills: it does not touch git, perspectives, buffers, or the Claude session — it only flips the profiler on or off in the running editor.

Use `/workspace-profile` when:

- The user reports an Emacs slowdown and wants to capture a profile.
- The user explicitly asks to start/stop the profiler.
- A debug workflow needs the profiler enabled around a specific action.

Do NOT attempt to toggle the profiler yourself in any way. Under NO circumstances. The handling of profiler toggling is EXCLUSIVELY the responsibility and right of downstream consumers. Your EXCLUSIVE job is to generate the JSON file, and NOTHING else. No code, no other files, no other mutating effects.

## JSON contract

Each entry is a single command object of type `"profile"`:

- `type` (required, string) — must be `"profile"`.
- `enabled` (required, boolean) — `true` starts the profiler; `false` stops it and pops up the report.
- `mode` (optional, string) — `"cpu"`, `"mem"`, or `"cpu+mem"`. Defaults to `"cpu+mem"`. Ignored when `enabled` is `false`.

The `workspace` field is intentionally absent: the profiler is an Emacs-process-wide instrument, not a per-workspace one, so dispatch is ambient.

## Steps

1. **Interpret** the user's request to decide:
   - Whether to start (`enabled: true`) or stop (`enabled: false`) the profiler.
   - Which mode to start in, if the user specified one (otherwise omit `mode`).

2. **Write the command** by piping JSON to `run.sh` using the Bash tool:
   ```bash
   bash /home/claude/.claude/skills/workspace-profile/run.sh << 'EOF'
   [
     {"type": "profile", "enabled": true, "mode": "cpu+mem"}
   ]
   EOF
   ```
   If the command fails due to missing `uuidgen`, **stop immediately** and tell the user they need to rebuild the sandbox image by running `.claude/install.sh`.

3. **Tell the user** what was dispatched in one short line — start vs stop, and the mode if specified.
