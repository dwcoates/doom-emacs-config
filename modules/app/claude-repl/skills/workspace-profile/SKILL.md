---
name: workspace-profile
description: Enable or disable the running Emacs's profiler. Use when the user wants to start/stop CPU/memory profiling in their editor, asks to "/workspace-profile", or wants to capture a profiler report for a slow editor operation.
---

# Workspace Profile

The user is running an editor (Emacs) that watches `~/.claude/output/` for JSON command files. Your job is to write one such file dispatching a profiler toggle to the editor. A downstream consumer picks up the file and either starts the Emacs profiler or stops it (and pops up `profiler-report`).

This skill is the editor-instrumentation counterpart to the rest of the workspace-command skills: it does not touch git, perspectives, buffers, or the Claude session — it only flips the profiler on or off in the running editor.

For a hands-off "start, wait N minutes, auto-stop, analyze" flow, use `/profile` instead — it wraps this skill with `ScheduleWakeup`-based auto-stop, a wait-duration policy, and JSONL session logging. Stay on plain `/workspace-profile` only when the user explicitly wants manual toggle control.

Use `/workspace-profile` when:

- The user reports an Emacs slowdown and wants to capture a profile.
- The user explicitly asks to start/stop the profiler.
- A debug workflow needs the profiler enabled around a specific action.

Do NOT attempt to toggle the profiler yourself in any way. Under NO circumstances. The handling of profiler toggling is EXCLUSIVELY the responsibility and right of downstream consumers. Your EXCLUSIVE job is to generate the JSON file, and NOTHING else. No code, no other files, no other mutating effects.

## JSON contract

Each entry is a single command object of type `"profile"`:

- `type` (required, string) — must be `"profile"`.
- `enabled` (required, boolean) — `true` starts the profiler; `false` stops it, pops up the report, and pipes the report text back to `workspace`'s Claude session.
- `mode` (optional, string) — `"cpu"`, `"mem"`, or `"cpu+mem"`. Defaults to `"cpu+mem"`. Ignored when `enabled` is `false`.
- `workspace` (required on disable, string) — the workspace name of the agent invoking this skill. The handler routes the captured `profiler-report` back to this workspace's Claude session via the normal input pipeline. The profiler itself is process-wide; this field is purely the return address. Omit on `enabled: true` (no report is generated yet); include on `enabled: false`.

The workspace name is the basename of the current working directory (e.g. for `/Users/foo/.config/doom-worktrees/my-feature` it is `my-feature`).

## Steps

1. **Interpret** the user's request to decide:
   - Whether to start (`enabled: true`) or stop (`enabled: false`) the profiler.
   - Which mode to start in, if the user specified one (otherwise omit `mode`).
   - On stop: determine your own workspace name via `basename "$PWD"` and include it as `workspace`.

2. **Write the command** by piping JSON to `run.sh` using the Bash tool. Enable:
   ```bash
   bash /home/claude/.claude/skills/workspace-profile/run.sh << 'EOF'
   [
     {"type": "profile", "enabled": true, "mode": "cpu+mem"}
   ]
   EOF
   ```
   Disable (substitute the real workspace name for `<ws>`):
   ```bash
   bash /home/claude/.claude/skills/workspace-profile/run.sh << EOF
   [
     {"type": "profile", "enabled": false, "workspace": "<ws>"}
   ]
   EOF
   ```
   If the command fails due to missing `uuidgen`, **stop immediately** and tell the user they need to rebuild the sandbox image by running `.claude/install.sh`.

3. **Tell the user** what was dispatched in one short line — start vs stop, and the mode if specified. On stop, mention that the report will arrive as a follow-up user message in this session.
