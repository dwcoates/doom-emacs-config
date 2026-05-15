---
name: debug-logs
description: Read and interpret (a) the claude-repl debug log at ~/.claude/emacs/doom-claude-repl.log and (b) the per-workspace memory-state.el snapshot under <root>/.claude/emacs/memory-state.el, and emphatically recommend adding instrumentation when the log lacks coverage of the suspect code path. Use when the user is debugging anything in modules/app/claude-repl/ (workspaces, drawer, sentinels, REPL state, autosave, hooks, vterm panels) or invokes /debug-logs.
---

# Debug Logs (claude-repl)

The doom claude-repl module writes two on-disk artifacts you can read without an Emacs session:

1. A single rolling **log file** at `~/.claude/emacs/doom-claude-repl.log` (history of events).
2. A per-workspace **memory-state snapshot** at `<project-root>/.claude/emacs/memory-state.el` (current full plist).

Use them as your first source of truth when investigating any claude-repl bug — assume they are more authoritative than your memory of what the code does.

## 1. Where the log lives

- Path: `~/.claude/emacs/doom-claude-repl.log` (current Emacs session)
- Prior session: `~/.claude/emacs/doom-claude-repl.log.prev` (one session retained — clobbered on each startup)
- Configured by `claude-repl-log-file-name` (defcustom in `modules/app/claude-repl/core.el`)
- Writing is ON by default (`claude-repl-log-to-file` defaults to `t`)
- Rollover: at Emacs startup the current log is renamed to `.prev`, so the active file always reflects the current session only
- Size cap: 1 GiB (`claude-repl-log-size-cap-bytes`); checked every 1000 writes; over-cap files have their first 80% dropped with a `WARNING: log truncated` line appended
- The parent directory `~/.claude/emacs/` is created on demand by `claude-repl--logfile-path`
- Toggle at runtime with `M-x claude-repl-debug/toggle-log-to-file`

If the active log is empty, no event has fired yet this session — check `.prev` for the previous session's events.

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

**File writes are unconditional.** Every call to `claude-repl--log`, `claude-repl--log-verbose`, `claude-repl--do-log`, and `claude-repl--error` appends to the logfile whenever `claude-repl-log-to-file` is non-nil (the default). The `claude-repl-debug` toggle only controls whether the line is ALSO emitted to the minibuffer / `*Messages*` buffer:

- **`claude-repl-debug` is nil (default):** file gets everything; minibuffer is quiet (except for unconditional `--do-log` / `--error` calls, which always also `message`).
- **`claude-repl-debug` is `t`:** file gets everything; `--log` calls also emit to minibuffer.
- **`claude-repl-debug` is `'verbose`:** file gets everything; `--log` AND `--log-verbose` calls emit to minibuffer (high-frequency events become visible).

Implication for diagnosis: the log file is the source of truth regardless of debug level. You do NOT need to ask the user to enable debug before reading historical evidence — it is already there.

The 1 GiB size cap (truncate-first-80%-on-overflow) is the safety valve that lets us keep verbose writes always-on.

## 4. How to read it

Useful bash recipes:

- Last 200 lines: `tail -n 200 ~/.claude/emacs/doom-claude-repl.log`
- Live tail during repro: `tail -f ~/.claude/emacs/doom-claude-repl.log`
- Filter by workspace: `grep 'ws=DWC/feature-foo' ~/.claude/emacs/doom-claude-repl.log | tail -100`
- Errors/warnings: `grep -E 'WARNING|ERROR|STUB-CREATE|backtrace|BUG' ~/.claude/emacs/doom-claude-repl.log | tail -50`
- Time-window: `awk '$1 >= "14:32:00" && $1 < "14:35:00"' ~/.claude/emacs/doom-claude-repl.log`
- Top message prefixes (find hot code paths): `grep '\[claude-repl\]' ~/.claude/emacs/doom-claude-repl.log | sed -E 's/^[^[]+\[claude-repl\] ([a-z-]+).*/\1/' | sort | uniq -c | sort -rn | head -30`
- File size sanity: `wc -l ~/.claude/emacs/doom-claude-repl.log; ls -lh ~/.claude/emacs/doom-claude-repl.log`
- Previous session: `tail -n 200 ~/.claude/emacs/doom-claude-repl.log.prev`
- Confirm truncation has fired: `grep -E 'WARNING: log truncated' ~/.claude/emacs/doom-claude-repl.log`

The companion buffer `*claude-repl-log-bug*` (inside Emacs) captures the first backtrace whenever `claude-repl--log-format` receives a non-string FMT. Ask the user to surface its contents if logging looks malformed.

## 5. When the log is too sparse — SUGGEST INSTRUMENTATION EMPHATICALLY

Since file writes are now unconditional, the only way the log can be uninformative is if the suspect code path has **no `claude-repl--log` calls at all**. When that happens, Claude tends to fill the silence with speculation. **Do not do that.** If you find yourself reading the log around the timestamp of the bug and cannot find any line that corresponds to the suspect function or branch, stop and surface this emphatically.

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

## 7. The per-workspace `memory-state.el` snapshot

Alongside the rolling log, every workspace writes a **point-in-time dump of its full in-memory plist** to disk. This is the same data the user would see by running `SPC j h p` (`claude-repl-debug/dump-workspace`) inside Emacs — accessible to you without an Emacs session.

### 7.1 Where it lives

- Path: `<project-root>/.claude/emacs/memory-state.el`
- One file per workspace, written under that workspace's `:project-dir`.
- Auto-overwritten on every `:claude-state` or `:repl-state` change for that workspace (see `claude-repl--ws-set-claude-state` and `claude-repl--ws-set-repl-state` in `status.el`).
- Filename constant: `claude-repl-memory-state-filename` (defined in `modules/app/claude-repl/memory-state.el`).
- The parent directory is auto-created on first write; no rotation, no size cap (each write replaces the file).

To find every snapshot on the host:

```
fd -uu memory-state.el ~/workspace ~/.config 2>/dev/null
# or:
find ~/workspace ~/.config -name memory-state.el -not -path '*/node_modules/*'
```

If you already know which workspace you are debugging, just read `<that-workspace-root>/.claude/emacs/memory-state.el` directly.

### 7.2 File format

A single readable plist sexp. Header comments at the top identify the file; the body is one `:key value` pair per line. Example shape:

```elisp
;;; -*- lexical-binding: t; -*-
;;; claude-repl memory-state dump — auto-written by Emacs.
;;; Mirrors `SPC j h p' / `claude-repl-debug/dump-workspace'.
;;; Read with: (with-temp-buffer (insert-file-contents FILE) (read (current-buffer)))

(:ws "DWC/feature-foo"
 :written-at "2026-05-15T14:33:32-0400"
 :project-dir "/path/to/worktree"
 :claude-state :thinking
 :repl-state :active
 :vterm-buffer "#<buffer claude-DWC/feature-foo live>"
 :git-proc "#<process git-status running>"
 :ready-timer "#<timer pending>"
 …)
```

Values that don't survive `read` round-trip — **buffers, processes, timers, cl-structs** — are pre-stringified using the same rendering as the interactive dump:

- buffer → `#<buffer NAME live>` or `#<buffer NAME dead>` (note: killed buffers report `nil` as the name because Emacs nulls it on kill — the `dead` marker is the load-bearing signal)
- process → `#<process NAME running>` or `#<process NAME exited>`
- timer → `#<timer pending>` or `#<timer triggered>`
- cl-struct → the pp'd struct as a trimmed string

All other values (keywords, strings, numbers, plain lists, nil, t) pass through unchanged, so the whole file is valid elisp that you can `read` programmatically:

```elisp
(with-temp-buffer
  (insert-file-contents "<root>/.claude/emacs/memory-state.el")
  (read (current-buffer)))
```

The header `:ws` and `:written-at` keys are prepended by the writer; everything after them is the verbatim workspace plist.

### 7.3 What keys you'll find

Far more than the trailing `{ws=… id=… …}` log-line metadata block in §2. The full plist surface (see `core.el` `claude-repl--workspaces` docstring and the keys written via `claude-repl--ws-put` across the module) includes — non-exhaustively:

- Core identity: `:project-dir`, `:ws-id`, `:name`, `:active-env`, `:bare-metal`, `:sandbox`, `:fork-session-id`, `:session-id`, `:worktree-p`, `:source-ws-name`, `:source-ws-dir`, `:priority`, `:group-key`, `:type`
- Lifecycle state: `:claude-state`, `:repl-state`, `:claude-ready`, `:done-acked`, `:done-acked-at`, `:stop-received`, `:pending-subagents`, `:viewed`, `:flashing`, `:bogus`, `:ws-loaded`
- Buffers/processes/timers: `:vterm-buffer`, `:input-buffer`, `:ready-timer`, `:git-proc`, `:merge-proc`
- Git/branch state: `:git-clean`, `:branch-merged`, `:branch-merged-last-check`, `:detail-branch`, `:detail-dirty-count`, `:detail-last-commit`, `:detail-last-commit-time`, `:detail-master-ahead`, `:detail-source-ahead`
- Merge lifecycle: `:merge-completed`, `:merge-completed-at`, `:merge-failed`, `:merge-conflict`, `:merge-queued`, `:merge-parent-dir`, `:merging`, `:merged`
- Prompts/UI: `:pending-prompts`, `:pending-show-panels`, `:pending-initial-buffers`, `:pending-magit`, `:deferred-prompts`, `:last-prompt-text`, `:last-prompt-time`, `:last-prompt-summary`, `:last-prompt-summary-pending`, `:last-notify-time`, `:clipboard`, `:saved-tab-index`, `:fullscreen-config`, `:ai-title-cache`
- Counters: `:prefix-counter`, `:counter`
- State-machine flag keywords (set as `t` when active): `:active`, `:idle`, `:thinking`, `:done`, `:permission`, `:init`, `:inactive`, `:hidden`, `:dead`, `:stop-failed`

If a key is absent from the file, it means it was never set on this workspace (treat it as `nil`).

### 7.4 When to use it

Read `memory-state.el` instead of the rolling log when:

- You need to know **what state the workspace is in *right now*** — the log shows transitions, the snapshot shows the destination.
- You need a **dirty list of timers/processes/buffers** owned by a single workspace (the trailing log metadata only carries a few of these).
- You're cross-checking the log against ground truth — e.g. log says `claude-state -> :done`, snapshot says `:claude-state :thinking` → there's an unwritten-back transition or a sync bug.
- The user mentions `SPC j h p` or asks "what does the dump look like for ws X" — read the file instead of asking them to run the command.

Read the rolling log file (§1–§4) instead when:

- You need history, ordering, or "what fired between T1 and T2".
- You're looking for warnings, errors, sentinel callbacks, or hot-path traces — those go to the log, not the snapshot.

### 7.5 Caveats

- The snapshot is **only refreshed on `:claude-state` or `:repl-state` changes.** Other plist mutations (counters, pending-prompts, prompt summaries) don't trigger a write on their own — they appear in the snapshot only when the next state transition flushes the file. So the snapshot can lag for non-state fields between transitions. If you need a guaranteed-fresh dump, ask the user to run `SPC j h p`.
- Stub workspaces (entries without `:project-dir`) have no snapshot — they appear in the log's `STUB-CREATE` lines instead.
- A killed buffer renders as `#<buffer nil dead>` because Emacs clears `buffer-name` on kill — the `nil` is normal, not a bug.

## 8. What NOT to do

- Do not mutate the log file. It is append-only by the module.
- Do not mutate `memory-state.el`. It is rewritten by Emacs on every state change; any edits will be clobbered.
- Do not infer state by reading the live `*Messages*` buffer when the log file has the same data — the file survives session restarts and is the canonical record.
- Do not silently rotate or truncate the log unless the user explicitly asks; some bugs need historical context.
- Do not skip step 5. If the log lacks coverage of the suspect code path, surface that emphatically and propose instrumentation — that IS the right next step.
