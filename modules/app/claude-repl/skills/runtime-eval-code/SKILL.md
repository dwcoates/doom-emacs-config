---
name: runtime-eval-code
description: Send elisp to the running Emacs at runtime — evaluate snippets, dump live buffer state, capture *Messages* output, or run a measured snippet inside a profiling session — and receive the result back as a follow-up message. The skill scopes every dispatch to the current workspace so operations like "create a buffer" or "evaluate a file" land in the workspace the agent is running in. Use when you need to inspect or mutate live editor state without rewriting source, when the bug is signaled by 3rd-party output the claude-repl log does not carry, or when pairing with /workspace-profile or /profile to measure exactly one operation.
argument-hint: "[free-form intent describing what should happen inside the editor]"
---

## What This Skill Does

Hands an elisp snippet to the running Emacs and receives the value (plus any `princ`-captured output) back as a follow-up user message. Every dispatch is scoped to the **current workspace** resolved in Step 0, so operations target the workspace the agent is running in rather than whichever buffer happened to be selected when the editor picked the command up.

This is the editor-instrumentation entry point. It does not touch git, perspectives, or the Claude session — it only hands code to the editor and waits for the response.

## Arguments

| Argument | Behaviour |
|---|---|
| *(free-form text)* | Describes what should happen inside the editor. Step 1 chooses an elisp snippet that satisfies the intent and scopes it to the workspace resolved in Step 0. |
| *(none)* | Refuse — there is nothing to dispatch. Surface the missing intent and stop. |

## Steps

0. **Resolve the current workspace.**
  - Run `bash ~/.claude/skills/runtime-eval-code/run.sh resolve-ws`.
  - EXIT CODE 0: capture stdout as `WS` and continue. This value is the return-address routing key and the scoping anchor for every later step.
  - EXIT CODE 1: unknown verb / usage error. IMMEDIATELY terminate and surface the raw stderr.
  - EXIT CODE 2: missing prerequisite. IMMEDIATELY terminate and tell the user to rebuild the sandbox image by running `.claude/install.sh`.
  - **Why this lives here**: every later dispatch needs `WS`, and computing it once up front (rather than per snippet) keeps the scoping rule in one place.

1. **Interpret the user's intent** and construct exactly one elisp snippet. Keep it small and self-contained — prefer pure functions that return data over side-effecting commands. Common shapes (use as templates, adapt to the intent rather than picking blindly):

   a. **Raw inspection** (intent: "what is X right now?", "evaluate this expression"):
      ```elisp
      (list :buf (buffer-name) :pt (point) :win (window-total-width))
      ```

   b. **Dump `*Messages*` to disk** (intent: "what did 3rd-party code emit?", "the bug is signaled by a magit / transient / byte-compile warning that doesn't show in the claude-repl log"):
      ```elisp
      (let ((file (expand-file-name "messages-dump.txt" "~/.claude/emacs/")))
        (make-directory (file-name-directory file) t)
        (with-current-buffer "*Messages*"
          (write-region (point-min) (point-max) file nil 'silent))
        file)
      ```
      The dump path is stable (always `~/.claude/emacs/messages-dump.txt`) so subsequent dumps overwrite — read promptly or rename if you need to keep a copy. After the eval response arrives:
      ```bash
      tail -n 500 ~/.claude/emacs/messages-dump.txt
      grep -E 'Error|Warning|cannot|failed|void-function|void-variable' ~/.claude/emacs/messages-dump.txt
      ```

   c. **Create a buffer in this workspace** (intent: "open/create a buffer named X for me to look at"):
      ```elisp
      (with-current-buffer (get-buffer-create "*runtime-eval-<name>*")
        (erase-buffer)
        (insert "<body>")
        (buffer-name))
      ```
      `WS` is the routing key — the editor knows which workspace to surface the buffer in.

   d. **Evaluate a file in this workspace** (intent: "load/evaluate this elisp file"):
      ```elisp
      (load-file (expand-file-name "<relative-path>" "<absolute workspace root>"))
      ```
      Embed the workspace root as a literal string in the snippet rather than relying on Emacs's `default-directory`. The workspace root is the host directory whose basename equals `WS`.

   *NOTE*: When you need a side effect visible in the response, use `princ` — `message` writes directly to `*Messages*` and bypasses the capture buffer (see "Response format" below).

2. **Dispatch the command** by piping JSON to `run.sh dispatch`. Substitute the real `WS` and the chosen snippet:
   ```bash
   bash ~/.claude/skills/runtime-eval-code/run.sh dispatch << 'EOF'
   [
     {"type": "eval",
      "code": "<elisp from step 1>",
      "workspace": "<WS>",
      "note": "<short label>"}
   ]
   EOF
   ```
   - EXIT CODE 0: continue to step 3.
   - EXIT CODE 1: usage error from the wrapper. IMMEDIATELY terminate and surface the raw stderr.
   - EXIT CODE 2: missing prerequisite. IMMEDIATELY terminate and tell the user to rebuild the sandbox image by running `.claude/install.sh`.

3. **Tell the user** what was dispatched in one short line. Name the intent (e.g. "dumped `*Messages*` for `<WS>`", "evaluated `<expr>` in `<WS>`") and mention that the result will arrive as a follow-up user message in this session.

## JSON contract

Each entry in the array is a single command object of type `"eval"`:

- `type` (required, string) — must be `"eval"`.
- `code` (required, string) — the elisp source to evaluate. Multiple top-level forms are supported; they run in order and `:result` reports the value of the last form.
- `workspace` (recommended, string) — the workspace name resolved in Step 0. The handler routes the formatted result back to this workspace's Claude session via the normal input pipeline. **Omit `workspace` only when the result genuinely doesn't need to come back** (rare — almost always include it).
- `note` (optional, string) — short label echoed in the response header. Use it to label long-running profiling steps (e.g. `"warmup"`, `"hot path"`).

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

## Quoting elisp inside JSON

The JSON `code` field is a string, so every embedded double-quote and backslash needs to be JSON-escaped. Two common patterns:

- **Single-quoted heredoc** (`<< 'EOF'`) — preserves the elisp literally; you only have to escape JSON specials (`"`, `\`). This is the preferred form.
- **Double-quoted heredoc** (`<< EOF`) — the shell additionally expands `$`, `` ` ``, and `\`, which usually breaks elisp. Avoid unless you specifically need shell interpolation.

When in doubt, write the elisp to a temp file and `cat` it through `jq` to build the JSON object — that sidesteps escaping entirely.

## Profiling workflow

A fully hands-off profile of a specific snippet:

1. Call `/workspace-profile` (or `/profile`) to **start** the profiler.
2. Call `/runtime-eval-code` with the snippet you want to measure.
3. Call `/workspace-profile` (or wait for `/profile`'s auto-stop) to **stop** and receive the `profiler-report`.

Three reasons to reach for this pairing over a bare `/profile`:

- **You want sampling around a specific user-invocable command** rather than whatever-happens-to-be-running. The eval snippet calls the command once (or a known number of times) so the report is dominated by that code path.
- **You want a deterministic re-run.** A snippet sent via `/runtime-eval-code` is identical across runs; the user "scrolling for 90 seconds" is not.
- **You need to inspect editor state before/after** the measured operation (e.g. confirm a cache was warm) — wrap your snippet with `princ` calls and they roundtrip in the `;; printed:` section of the eval response.

`/profile` already wraps step 1 + step 3 — pair it with a single `/runtime-eval-code` dispatch in between for an end-to-end "measure exactly this code" run.

## Notes

- **CRITICAL NOTE: Do NOT evaluate the elisp yourself.** No `emacs --batch`, no subshell `emacsclient`. The editor is the only legitimate evaluator.
- **CRITICAL NOTE: Do NOT skip Step 0.** The workspace name is the return-address routing key and the scoping anchor for every later step. Never guess it from context.
- **IMPORTANT NOTE: Do NOT use `message` and expect output back** — it doesn't roundtrip. Use `princ` for side-effect output you want captured.
- **IMPORTANT NOTE: Do NOT send mutation-heavy snippets without a `note` and a clear user request** — the agent does not get to silently rewrite the user's running editor state.
- **IMPORTANT NOTE: Do NOT omit the resolved workspace** from the `workspace` JSON field unless the user explicitly says they don't want the result back.
