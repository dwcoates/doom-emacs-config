---
name: runtime-eval-code
description: Send elisp to the running Emacs at runtime — evaluate snippets, dump live buffer state, capture *Messages* output, or run a measured snippet inside a profiling session — and receive the result back as a follow-up message. The skill scopes every dispatch to the current workspace so operations like "create a buffer" or "evaluate a file" land in the workspace the agent is running in. Use when you need to inspect or mutate live editor state without rewriting source, when the bug is signaled by 3rd-party output the claude-repl log does not carry, or when pairing with /workspace-profile or /profile to measure exactly one operation.
argument-hint: "[free-form intent describing what should happen inside the editor]"
---

## What This Skill Does

Hands an elisp snippet to the running Emacs and receives the value (plus any `princ`-captured output) back as a follow-up user message. Every dispatch is scoped to the **current workspace** resolved in Step 0, so operations target the workspace the agent is running in rather than whichever buffer happened to be selected when the editor picked the command up.

This is the editor-instrumentation entry point. It does not touch git, perspectives, or the Claude session — it only hands code to the editor and waits for the response.

## When To Reach For This

Reach for this skill **proactively** — do not wait to be told to "eval some elisp."

- **Whenever the user suggests or implies an unexpected or incorrect runtime state** for which actually investigating the live editor is useful, dispatch a snippet to inspect the relevant state rather than reasoning from source alone.
  - Example: the user complains a UI element is the wrong color → proactively read that element's backing state (the face, the variable, the workspace plist key that drives it) instead of guessing from the code.
  - The source tells you what *should* happen; only the running editor tells you what *is* happening.
- **Always prefer this skill over asking the user to run elisp themselves.**
  - If you would otherwise type "can you run `(...)` and paste the result?", dispatch it through this skill instead and read the roundtripped result.
  - The sole exception is genuinely dangerous code (destructive mutations, anything irreversible) — surface that to the user and let them decide rather than dispatching it silently.
- **After performing any mutating change, proactively verify it landed** rather than assuming success from a clean dispatch.
  - This applies to every mutation, whether it ran through this skill or through any other path (a command, a hook, an edit-then-reload).
  - Verify by checking the logs and/or by reading the relevant runtime state that the change was supposed to affect.
    - Read the claude-repl log (or dump `*Messages*` per the cheatsheet) to confirm the expected log line appeared and no error followed.
    - Re-read the specific state the mutation targets and confirm it now holds the intended value.
    - Where the change should surface in the UI, inspect the backing state for the affected element (e.g. the tabline / tab-bar) to confirm it reflects the new state.
  - Treat the verification read as part of the mutation, not an optional follow-up — a dispatch returning exit 0 only means the command was queued, not that the intended effect occurred.

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

## Cheatsheet

Common operational inspections. Each entry: when to reach for it, the snippet to send, and what to watch out for. Snippets return data (or `princ` it) so the value roundtrips in the eval response.

### Check the merge queue

- **When**: the user reports a merge that didn't land, a workspace stuck "waiting to merge," or suspected serialization behind an in-flight cherry-pick.
- **Send**:
  ```elisp
  (or claude-repl--merge-queue "<empty>")
  ```
- **Watch out for**:
  - Empty state is `nil`, not an empty list — the `or` above surfaces `"<empty>"` so the response isn't ambiguous.
  - Each element is a plist `(:source-ws WS :silent BOOL :auto-resolve BOOL)`; the queue is FIFO (head is next to run).
  - The queue only holds *deferred* merges (parked behind an active cherry-pick) — an empty queue does not mean no merge is in flight, just that none are waiting.

### Link the user to code

- **When**: any time the user asks about an implementation detail, or whenever there is relevant code worth pointing them at — reach for this **proactively**.
  - Applies when the user is *explicitly* asking about specific code.
  - Applies equally when the user is asking about implementation *logic* (not code per se) but there is code that answers it — find it and link it anyway.
  - In these situations, proactively grep/search the repo for the most relevant definition first, then link it rather than only describing it in prose.
- **Send** (`claude-repl-link-code` opens the file in a left window, jumps to the code, and selects the full line range):
  ```elisp
  (claude-repl-link-code "<absolute-path-to-file>" <start-line> <end-line>)
  ```
  - `end-line` is optional — omit it to select a single line: `(claude-repl-link-code "<path>" <line>)`.
- **Watch out for**:
  - Pass an **absolute** path (resolve relative paths against the workspace root yourself before sending).
  - Line numbers are **1-indexed and inclusive**; the selected region runs from the start line's beginning to the end line's end.
  - It opens in a left-docked window and selects the region — it does not edit, so it is always safe to dispatch.

### Check the workspaces hashmap

- **When**: the user reports a workspace in the wrong state (stale priority, wrong env, missing buffer, a tombstone that should be gone) or you need to confirm a UI element's backing per-workspace state.
- **Send** (all live workspaces, one key of interest at a time keeps output small):
  ```elisp
  (mapcar (lambda (ws)
            (list ws :priority (plist-get (gethash ws claude-repl--workspaces) :priority)))
          (claude-repl--live-ws-names))
  ```
  - To dump one workspace's full plist (readably), target it by name:
    ```elisp
    (pp-to-string (gethash "<WS>" claude-repl--workspaces))
    ```
- **Watch out for**:
  - Use `claude-repl--live-ws-names` (not `hash-table-keys`) — the raw keys include **tombstoned** entries (`:nuked-at` set) that liveness-filtered UI ignores.
  - Values are large nested plists (`:vterm-buffer`, `:active-env` structs, etc.) — dumping the whole hash with `pp` is verbose and can hit the 8000-char truncation, so project the one or two keys you care about.
  - A key bound to `nil` is distinct from an absent key; internals use the `claude-repl--ws-absent` sentinel to tell them apart, so don't read a `nil` `plist-get` as "key missing."

### Reopen a workspace

- **When**: the user asks to reopen, switch back to, or re-activate a workspace (including closed/tombstoned ones), or to open a whole set of them at once.
- **Send** (derive the workspace name, resolve it to its project root with `claude-repl--ws-dir`, then hand that root to `claude-repl-switch-to-project`):
  ```elisp
  (claude-repl-switch-to-project (claude-repl--ws-dir "<ws-name>"))
  ```
  - To reopen several (e.g. every workspace parked in the merge queue), loop over the names:
    ```elisp
    (dolist (ws (mapcar (lambda (e) (plist-get e :source-ws)) claude-repl--merge-queue))
      (claude-repl-switch-to-project (claude-repl--ws-dir ws)))
    ```
- **Watch out for**:
  - `claude-repl-switch-to-project` takes a project **root path**, not a workspace name, so always resolve through `claude-repl--ws-dir` first.
  - It is **side-effecting**: it creates/activates the persp and visits a file, so a loop ends focused on the last workspace opened.
  - Tombstoned (closed) workspaces still resolve because `:project-dir` is preserved on close, so reopening them works.
  - `claude-repl--ws-dir` errors when a name has no `:project-dir`, so feed it real workspace names (not arbitrary strings).

## Notes

- **CRITICAL NOTE: Do NOT evaluate the elisp yourself.** No `emacs --batch`, no subshell `emacsclient`. The editor is the only legitimate evaluator.
- **CRITICAL NOTE: Do NOT skip Step 0.** The workspace name is the return-address routing key and the scoping anchor for every later step. Never guess it from context.
- **IMPORTANT NOTE: Do NOT use `message` and expect output back** — it doesn't roundtrip. Use `princ` for side-effect output you want captured.
- **IMPORTANT NOTE: Do NOT send mutation-heavy snippets without a `note` and a clear user request** — the agent does not get to silently rewrite the user's running editor state.
- **IMPORTANT NOTE: Do NOT omit the resolved workspace** from the `workspace` JSON field unless the user explicitly says they don't want the result back.
