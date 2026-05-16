---
name: workspace-eval
description: Send a snippet of elisp to the running Emacs to be evaluated, and receive the result back as a follow-up message. Use when the user invokes "/workspace-eval", when an agent needs to inspect or mutate live editor state, or when driving a profiling session (combine with /workspace-profile or /profile).
---

# Workspace Eval

The user is running an editor (Emacs) that watches `~/.claude/output/` for JSON command files. Your job is to write one such file dispatching an `"eval"` command — the editor reads it, evaluates the embedded elisp, and pipes the formatted result back to this workspace's Claude session via the same return-address pipeline used by `/workspace-profile` on stop.

This skill is the editor-instrumentation counterpart to the rest of the workspace-command skills: it does not touch git, perspectives, buffers, or the Claude session — it just hands code to the editor and waits for the response message.

Use `/workspace-eval` when:

- The user explicitly asks to send elisp to the editor and observe what it returns.
- You need to inspect live editor state (current buffer, point, window config, an internal data structure) without rewriting source files.
- You are driving a profiling workflow and need the editor to run a specific snippet between profiler start and stop. See "Profiling workflow" below.

Do NOT attempt to evaluate elisp yourself in any way. The handling of evaluation is EXCLUSIVELY the responsibility of the editor downstream. Your EXCLUSIVE job is to generate the JSON file, and NOTHING else. No spawned subprocesses, no `emacs --batch`, no other mutating effects.

## JSON contract

Each entry is a single command object of type `"eval"`:

- `type` (required, string) — must be `"eval"`.
- `code` (required, string) — the elisp source to evaluate. Multiple top-level forms are supported; they run in order and `:result` reports the value of the last form.
- `workspace` (optional, string) — the workspace name of the agent invoking this skill. The handler routes the formatted result back to this workspace's Claude session via the normal input pipeline. **Omit `workspace` only when you genuinely don't want the result back** (rare — almost always include it).
- `note` (optional, string) — short label echoed in the response header. Use it to label long-running profiling steps (e.g. `"warmup"`, `"hot path"`).

The workspace name is the basename of the current working directory (e.g. for `/Users/foo/.config/doom-worktrees/my-feature` it is `my-feature`).

## Response format

When `workspace` is set, the editor sends a follow-up user message back into this session formatted as:

```
Elisp eval result (note: <note>):

```elisp
;; code:
<the code you sent>

;; printed:        # only when (princ ...) etc produced output
<captured stdout>

;; result:
<prin1-to-string of the return value>
```
```

Errors are reported with `Elisp eval ERROR` as the header and a `;; error:` section in place of `;; result:`. Long output is truncated to `claude-repl-eval-output-max-chars` (default 8000) with a `[truncated to N chars]` marker.

Captured output uses a buffer-bound `standard-output`, so `princ` / `print` work but `message` does NOT — `message` writes directly to `*Messages*` and bypasses the capture. If you need a side-effect visible in `:printed`, use `princ`.

## Steps

1. **Interpret** the user's request to decide what elisp to send. Keep snippets small and self-contained. Prefer pure functions that return data over side-effecting commands.

2. **Determine your own workspace name** via `basename "$PWD"` and include it as `workspace` so the response routes back here.

3. **Write the command** by piping JSON to `run.sh` using the Bash tool. Single-form snippet:
   ```bash
   bash /home/claude/.claude/skills/workspace-eval/run.sh << 'EOF'
   [
     {"type": "eval",
      "code": "(buffer-name)",
      "workspace": "<ws>"}
   ]
   EOF
   ```
   Multi-form snippet (use a `note` to label it):
   ```bash
   bash /home/claude/.claude/skills/workspace-eval/run.sh << 'EOF'
   [
     {"type": "eval",
      "code": "(princ \"loading\") (length (buffer-list))",
      "workspace": "<ws>",
      "note": "buffer-count-warmup"}
   ]
   EOF
   ```
   If the command fails due to missing `uuidgen`, **stop immediately** and tell the user they need to rebuild the sandbox image by running `.claude/install.sh`.

4. **Tell the user** what was dispatched in one short line, and mention that the result will arrive as a follow-up user message in this session.

## Quoting elisp inside JSON

The JSON `code` field is a string, so every embedded double-quote and backslash needs to be JSON-escaped. Two common patterns:

- **Single-quoted heredoc** (`<< 'EOF'`) — preserves the elisp literally; you only have to escape JSON specials (`"`, `\`). This is the preferred form.
- **Double-quoted heredoc** (`<< EOF`) — the shell additionally expands `$`, `` ` ``, and `\`, which usually breaks elisp. Avoid unless you specifically need shell interpolation.

When in doubt, write the elisp to a temp file and `cat` it through `jq` to build the JSON object — that sidesteps escaping entirely.

## Profiling workflow

`/workspace-eval` is the missing piece that lets an agent run a fully hands-off profile of a specific snippet:

1. Call `/workspace-profile` (or `/profile`) to **start** the profiler.
2. Call `/workspace-eval` with the snippet you want to measure.
3. Call `/workspace-profile` (or wait for `/profile`'s auto-stop) to **stop** and receive the `profiler-report`.

`/profile` already wraps step 1 + step 3 — pair it with a single `/workspace-eval` in between for an end-to-end "measure exactly this code" run.

## What NOT to do

- Do NOT evaluate the elisp yourself (no `emacs --batch`, no subshell `emacsclient`). The editor is the only legitimate evaluator.
- Do NOT use `message` and expect output back — it doesn't roundtrip. Use `princ` for side-effect output you want captured.
- Do NOT send mutation-heavy snippets without a `note` and a clear user request — the agent does not get to silently rewrite the user's running editor state.
- Do NOT omit `workspace` unless the user explicitly says they don't want the result back.
