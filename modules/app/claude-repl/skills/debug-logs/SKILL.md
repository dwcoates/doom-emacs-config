---
name: debug-logs
description: Read and interpret the claude-repl debug log at ~/.claude/emacs/doom-claude-repl.log, and emphatically recommend adding instrumentation when the log lacks coverage of the suspect code path. Use when the user is debugging anything in modules/app/claude-repl/ (workspaces, drawer, sentinels, REPL state, autosave, hooks, vterm panels) or invokes /debug-logs.
---

# Debug Logs (claude-repl)

The doom claude-repl module writes a single rolling log file. Use it as your first source of truth when investigating any claude-repl bug — assume it is more authoritative than your memory of what the code does.

## 1. Where the log lives

- Path: `~/.claude/emacs/doom-claude-repl.log`
- Configured by `claude-repl-log-file-name` (defcustom in `modules/app/claude-repl/core.el`)
- Writing is ON by default (`claude-repl-log-to-file` defaults to `t`)
- The parent directory `~/.claude/emacs/` is created on demand by `claude-repl--logfile-path`
- Toggle at runtime with `M-x claude-repl-debug/toggle-log-to-file`

If the file is missing entirely, the user has either turned `claude-repl-log-to-file` off or no claude-repl event has yet passed through `claude-repl--do-log` in this Emacs session. Reproducing the bug after re-enabling will populate it.

## 2. Line format

Each line:

```
HH:MM:SS.mmm [claude-repl] <message-body> {ws=<name> id=<8hex> dir=<path> cst=<claude-state> rst=<repl-state> env=<…> vt=<live|dead|-> in=<live|dead|-> cnt=<n> git=<clean|dirty|-> gproc=<run|done|-> wt=<t|-> fork=<…> rtmr=<t|-> pri=<…> pend=<n> pshow=<t|->}
```

Workspace metadata trails the message. Keys:

- `ws` — workspace name (e.g. `DWC/feature-foo`)
- `id` — 8-char hash of `:project-dir` (stable identifier across renames)
- `dir` — expanded `:project-dir`
- `cst` — `:claude-state` — `init` | `idle` | `thinking` | `done` | `permission` | `stop-failed` | `-`
- `rst` — `:repl-state` — `active` | `inactive` | `hidden` | `dead` | `-`
- `env` — active env keyword
- `vt` — vterm buffer liveness (`live` / `dead` / `-`)
- `in` — input buffer liveness (`live` / `dead` / `-`)
- `cnt` — prefix counter (metaprompt re-injection)
- `git` — git clean/dirty for the workspace
- `gproc` — live async git sentinel process (`run` / `done` / `-`)
- `wt` — `t` if this workspace is a worktree
- `fork` — forked session id (if any)
- `rtmr` — ready-timer present
- `pri` — workspace priority
- `pend` — length of pending-prompts queue
- `pshow` — pending-show-panels flag

A trailing `-` for a key means "not set / not applicable".

## 3. What is logged automatically vs. gated

`claude-repl-log-to-file` does NOT itself unlock all events. It controls whether messages that pass through `claude-repl--do-log` get appended to disk. WHICH messages pass through is gated separately:

- **Always logged (no toggle needed):**
  - Errors signalled via `claude-repl--error`
  - `claude-repl--ws-put` STUB-CREATE warnings (workspace entry created without `:project-dir` — the "(no repo)" drawer-bucket trap)
- **Logged only when `claude-repl-debug` is non-nil:**
  - Every `claude-repl--log` call site (the majority of the module — `commands.el`, `panels.el`, `input.el`, `keybindings.el`, `history.el`, etc.)
- **Logged only when `claude-repl-debug` is `'verbose`:**
  - Every `claude-repl--log-verbose` call site — high-frequency events (timer ticks, window changes, git-diff sentinels, `resolve-root`)

If the user is reproducing a bug live, **ask whether `claude-repl-debug` is on**. Toggle with `M-x claude-repl-debug/toggle-logging` (prefix arg for verbose).

## 4. How to read it

Useful bash recipes:

- Last 200 lines: `tail -n 200 ~/.claude/emacs/doom-claude-repl.log`
- Live tail during repro: `tail -f ~/.claude/emacs/doom-claude-repl.log`
- Filter by workspace: `grep 'ws=DWC/feature-foo' ~/.claude/emacs/doom-claude-repl.log | tail -100`
- Errors/warnings: `grep -E 'WARNING|ERROR|STUB-CREATE|backtrace|BUG' ~/.claude/emacs/doom-claude-repl.log | tail -50`
- Time-window: `awk '$1 >= "14:32:00" && $1 < "14:35:00"' ~/.claude/emacs/doom-claude-repl.log`
- Top message prefixes (find hot code paths): `grep '\[claude-repl\]' ~/.claude/emacs/doom-claude-repl.log | sed -E 's/^[^[]+\[claude-repl\] ([a-z-]+).*/\1/' | sort | uniq -c | sort -rn | head -30`
- File size sanity: `wc -l ~/.claude/emacs/doom-claude-repl.log; ls -lh ~/.claude/emacs/doom-claude-repl.log`

The companion buffer `*claude-repl-log-bug*` (inside Emacs) captures the first backtrace whenever `claude-repl--log-format` receives a non-string FMT. Ask the user to surface its contents if logging looks malformed.

## 5. When the log is too sparse — SUGGEST INSTRUMENTATION EMPHATICALLY

The single most common pitfall in this codebase: a bug is reported in a code path that has **no `claude-repl--log` calls**, the log shows nothing useful, and Claude proceeds to speculate. **Do not do that.** If you find yourself reading the log around the timestamp of the bug and cannot find any line that corresponds to the suspect function or branch, stop and surface this emphatically.

Required user-facing message in that case:

> ⚠️ The log around `<timestamp>` contains no entries for `<function/feature>`. I cannot confidently diagnose this from the log alone. I **strongly recommend** adding `claude-repl--log` calls at the following points before reproducing again:
>
> - `<file>:<line>` — `<function>` (entry + which branch was taken)
> - `<file>:<line>` — `<function>` (the suspected early-return)
>
> Want me to add them?

Apply this rule when **any** of the following is true:

- The suspect function has zero `claude-repl--log` / `claude-repl--log-verbose` / `claude-repl--error` calls
- The conditional branch you suspect (an `if` arm, an early return, an error fallback) has no log line
- The hot code path appears to have run but produces no log evidence of which branch was taken
- The repro produces a visible symptom but the log between the user's action and the symptom is empty or routine

Be **emphatic, not soft**. Say "strongly recommend", "I need more instrumentation before I can be confident", "the log is uninformative here" — do not bury the recommendation under speculation. The instrumentation is cheap, lives in the same module, and the user has explicitly chosen this debugging style. Adding three or four `claude-repl--log` calls and re-running the repro is almost always faster than guessing.

## 6. Template for adding instrumentation

When proposing new log calls, follow the module's conventions:

```elisp
;; Default (gated on `claude-repl-debug'):
(claude-repl--log ws "myfn: entered cond=%s key=%s" cond key)

;; Verbose (gated on `claude-repl-debug = 'verbose') — for hot/timer paths:
(claude-repl--log-verbose ws "myfn: tick state=%s" state)

;; Unconditional (always written, even with debug off) — reserve for
;; invariant violations or state shapes that must never be lost:
(claude-repl--do-log ws "myfn: INVARIANT-BREACH cond=%s" (list cond))

;; Error — logs AND signals:
(claude-repl--error ws "myfn: refusing to do X because Y=%s" y)
```

Always pass the workspace as the first argument so the trailing `{ws=…}` metadata block carries context. Use `nil` only for genuinely workspace-free contexts (loaders, global hooks before any workspace is known).

Prefer logging:

- **Function entry + the values that drove the branch decision** — not just "entered fn".
- **Both arms of an `if`/`cond`** — silence on one arm is ambiguous; explicit logs on both arms remove the ambiguity.
- **Early returns and error fallbacks** — the most common place a bug hides.
- **Async sentinel callbacks** — log on every invocation, with the relevant state, since async paths are hardest to reason about.

## 7. What NOT to do

- Do not mutate the log file. It is append-only by the module.
- Do not infer state by reading the live `*Messages*` buffer when the log file has the same data — the file survives session restarts and is the canonical record.
- Do not silently rotate or truncate the file unless the user explicitly asks; some bugs need historical context.
- Do not skip step 5. If the log lacks coverage of the suspect code path, surface that emphatically and propose instrumentation — that IS the right next step.
