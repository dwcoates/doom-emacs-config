---
name: debug-emacs-agent-repl
description: Debug the agent-repl system across all six of its planes — Emacs, daemon, shim, webapp, the SSM state database, and the store data plane — by reading its on-disk logs and directly querying its two SQLite databases, and emphatically recommend adding instrumentation when the evidence lacks coverage of the suspect code path. Use when the user is debugging anything in modules/app/agent-repl/ (workspaces, sentinels, REPL state, autosave, hooks, vterm panels), asks why a workspace is a given color or stuck in a given state (blocked, blue, purple, spinning), reports missing or garbled conversation content, or hits daemon / shim / store / sidecar symptoms. Invoked as /debug-emacs-agent-repl, or by its legacy name /debug-logs.
---

# Debug agent-repl (all planes)

agent-repl is not one program. It is **six planes**, each with its own evidence, and the first job of any investigation is deciding which plane you are actually in:

| Plane | What it is | Primary evidence |
|---|---|---|
| **Emacs** | The editor module — workspaces, panels, sentinels, REPL state | `~/.claude-emacs/doom-agent-repl.log`, per-workspace `memory-state.el` (§2) |
| **Daemon** | The Go process brokering everything | `~/.claude-emacs/claude-repld.log` (§3) |
| **Shim** | The TypeScript process wrapping the Claude SDK | Same daemon log, prefixed `shim stderr: ` (§3.4) |
| **Webapp** | The UI inside the Emacs webview | Same daemon log, forwarded client-logs (§3.5) |
| **State** | The SSM database that decides every workspace's color | `~/.cache/agent-repl/ssm/state.db` (§4) |
| **Data** | The event store holding conversation content | `~/.cache/agent-repl/store/events.db` (§5) |

Two of those planes are **SQLite databases you can query directly**. That is the single biggest lever in this skill: "why is this workspace purple" and "why is this conversation missing content" are *queries*, not guesses. Use them.

Treat this evidence as more authoritative than your memory of what the code does.

**Before trusting any of it, ask whether the code you are reading is the code that is running** — every component is a built artifact, so a merged fix that was never deployed looks exactly like a fix that does not work. See §6.

If the suspected problem is **performance** (Emacs feels slow, an operation hitches, a hot path is suspect) rather than logic / state, this skill is the wrong tool. Use `/profile` instead — it orchestrates a time-boxed capture of the Emacs profiler with auto-stop and analysis. This skill is for reading history that already exists; `/profile` is for capturing fresh sampling data.

If the relevant evidence is **not in any log** because the failing code is 3rd-party (magit, transient, doom-core, vterm, byte-compiler) and never calls `agent-repl--log`, the only on-host record is Emacs's `*Messages*` buffer. Use `/runtime-eval-code` to dump it to disk — see §9. `/runtime-eval-code` is also the right tool when you need to inspect live editor state (a value, a predicate, an internal data structure) that no logger has captured.

## 1. Triage first

**Start every investigation here.** Do not open a log until you know which plane you are in.

### 1.1 Run the health sweep

```sh
modules/app/agent-repl/scripts/agent-shim-doctor.sh
# machine-readable:
modules/app/agent-repl/scripts/agent-shim-doctor.sh --json
```

- It is **strictly read-only** — it never starts, stops, or mutates anything, so it is always safe to run first.
- Nine checks: store socket present + connectable, both launchd services, frontend socket, per-session sockets, both service logs, store DB present + `PRAGMA integrity_check`.
- Exit 0 when nothing FAILed. A **SKIP** means an optional tool was absent — an honest "could not evaluate", never a silent pass. Do not read a SKIP as a green light.
- Honors `AGENT_REPL_STATE_ROOT`.

### 1.2 Route by symptom

| Symptom | Plane | Go to |
|---|---|---|
| Workspace is the wrong **color**, or stuck (blocked, blue, purple, endless spinner) | State | **§4** — query the SSM database |
| Conversation content **missing, garbled, or truncated** | Data | **§5** — query the store DB and read the sidecar log |
| **UI-only** misbehavior (scroll jumps, render stall, seq gaps, lost replay) | Webapp | **§3.5** — the client-log forwarded into the daemon log |
| Editor behavior: panels, buffers, keybindings, workspace lifecycle | Emacs | **§2** |
| Turn never starts, session won't spawn, vendor call errors | Daemon / shim | **§3**, **§3.4** |
| 3rd-party Emacs failure (magit, transient, byte-compiler, package init) | — | **§9** — `*Messages*` via `/runtime-eval-code` |
| Performance, hitching, "it feels slow" | — | **Not this skill.** Use `/profile`. |

### 1.3 Always ask the deploy question

Before concluding "the code is wrong", confirm the running artifact matches the source you are reading. See **§6**. This is cheap and it is the single most common way an investigation wastes its time.

---

## 2. The Emacs plane

The doom agent-repl module writes two on-disk artifacts you can read without an Emacs session:

1. A single rolling **log file** at `~/.claude-emacs/doom-agent-repl.log` (history of events).
2. A per-workspace **memory-state snapshot** at `<project-root>/.claude/emacs/memory-state.el` (current full plist).

### 2.1 Where the log lives

- Path: `~/.claude-emacs/doom-agent-repl.log` (current Emacs session)
- Prior session: `~/.claude-emacs/doom-agent-repl.log.prev` (one session retained — clobbered on each startup)
- Configured by `agent-repl-log-file-name` (defcustom in `modules/app/agent-repl/core.el`)
- Writing is ON by default (`agent-repl-log-to-file` defaults to `t`)
- Rollover: at Emacs startup the current log is renamed to `.prev`, so the active file always reflects the current session only
- Size cap: 1 GiB (`agent-repl-log-size-cap-bytes`); checked every 1000 writes; over-cap files have their first 80% dropped with a `WARNING: log truncated` line appended
- The parent directory `~/.claude-emacs/` is created on demand by `agent-repl--logfile-path`
- Toggle at runtime with `M-x agent-repl-debug/toggle-log-to-file`

If the active log is empty, no event has fired yet this session — check `.prev` for the previous session's events.

### 2.2 Line format

Each line:

```
HH:MM:SS.mmm [agent-repl] <message-body> {ws=<name> id=<8hex> dir=<path> cst=<claude-state> rst=<repl-state> env=<…> vt=<live|dead|-> in=<live|dead|-> cnt=<n> git=<clean|dirty|-> gproc=<run|done|-> wt=<t|-> fork=<…> rtmr=<t|-> pri=<…> pend=<n> pshow=<t|->}
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

**This trailing `{k=v}` block is deliberately the same shape the daemon log uses** (§3.2), so the same grep idioms work on both files.

### 2.3 What is logged automatically vs. gated

**File writes are unconditional.** Every call to `agent-repl--log`, `agent-repl--log-verbose`, `agent-repl--do-log`, and `agent-repl--error` appends to the logfile whenever `agent-repl-log-to-file` is non-nil (the default). The `agent-repl-debug` toggle only controls whether the line is ALSO emitted to the minibuffer / `*Messages*` buffer:

- **`agent-repl-debug` is nil (default):** file gets everything; minibuffer is quiet (except for unconditional `--do-log` / `--error` calls, which always also `message`).
- **`agent-repl-debug` is `t`:** file gets everything; `--log` calls also emit to minibuffer.
- **`agent-repl-debug` is `'verbose`:** file gets everything; `--log` AND `--log-verbose` calls emit to minibuffer (high-frequency events become visible).

Implication for diagnosis: the log file is the source of truth regardless of debug level. You do NOT need to ask the user to enable debug before reading historical evidence — it is already there.

The 1 GiB size cap (truncate-first-80%-on-overflow) is the safety valve that lets us keep verbose writes always-on.

### 2.4 How to read it

Useful bash recipes:

- Last 200 lines: `tail -n 200 ~/.claude-emacs/doom-agent-repl.log`
- Live tail during repro: `tail -f ~/.claude-emacs/doom-agent-repl.log`
- Filter by workspace: `grep 'ws=DWC/feature-foo' ~/.claude-emacs/doom-agent-repl.log | tail -100`
- Errors/warnings: `grep -E 'WARNING|ERROR|STUB-CREATE|backtrace|BUG' ~/.claude-emacs/doom-agent-repl.log | tail -50`
- Time-window: `awk '$1 >= "14:32:00" && $1 < "14:35:00"' ~/.claude-emacs/doom-agent-repl.log`
- Top message prefixes (find hot code paths): `grep '\[agent-repl\]' ~/.claude-emacs/doom-agent-repl.log | sed -E 's/^[^[]+\[agent-repl\] ([a-z-]+).*/\1/' | sort | uniq -c | sort -rn | head -30`
- File size sanity: `wc -l ~/.claude-emacs/doom-agent-repl.log; ls -lh ~/.claude-emacs/doom-agent-repl.log`
- Previous session: `tail -n 200 ~/.claude-emacs/doom-agent-repl.log.prev`
- Confirm truncation has fired: `grep -E 'WARNING: log truncated' ~/.claude-emacs/doom-agent-repl.log`

The companion buffer `*agent-repl-log-bug*` (inside Emacs) captures the first backtrace whenever `agent-repl--log-format` receives a non-string FMT. Ask the user to surface its contents if logging looks malformed.

### 2.5 The per-workspace `memory-state.el` snapshot

Alongside the rolling log, every workspace writes a **point-in-time dump of its full in-memory plist** to disk. This is the same data the user would see by running `SPC j h p` (`agent-repl-debug/dump-workspace`) inside Emacs — accessible to you without an Emacs session.

#### 2.5.1 Where it lives

- Path: `<project-root>/.claude/emacs/memory-state.el`
- One file per workspace, written under that workspace's `:project-dir`.
- Auto-overwritten on every `:claude-state` or `:repl-state` change for that workspace (see `agent-repl--ws-set-claude-state` and `agent-repl--ws-set-repl-state` in `status.el`).
- Filename constant: `agent-repl-memory-state-filename` (defined in `modules/app/agent-repl/memory-state.el`).
- The parent directory is auto-created on first write; no rotation, no size cap (each write replaces the file).

To find every snapshot on the host:

```
fd -uu memory-state.el ~/workspace ~/.config 2>/dev/null
# or:
find ~/workspace ~/.config -name memory-state.el -not -path '*/node_modules/*'
```

If you already know which workspace you are debugging, just read `<that-workspace-root>/.claude/emacs/memory-state.el` directly.

#### 2.5.2 File format

A single readable plist sexp. Header comments at the top identify the file; the body is one `:key value` pair per line. Example shape:

```elisp
;;; -*- lexical-binding: t; -*-
;;; agent-repl memory-state dump — auto-written by Emacs.
;;; Mirrors `SPC j h p' / `agent-repl-debug/dump-workspace'.
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

#### 2.5.3 What keys you'll find

Far more than the trailing `{ws=… id=… …}` log-line metadata block in §2.2. The full plist surface (see `core.el` `agent-repl--workspaces` docstring and the keys written via `agent-repl--ws-put` across the module) includes — non-exhaustively:

- Core identity: `:project-dir`, `:ws-id`, `:name`, `:active-env`, `:bare-metal`, `:sandbox`, `:fork-session-id`, `:session-id`, `:worktree-p`, `:source-ws-name`, `:source-ws-dir`, `:priority`, `:group-key`, `:type`
- Lifecycle state: `:claude-state`, `:repl-state`, `:claude-ready`, `:done-acked`, `:done-acked-at`, `:stop-received`, `:pending-subagents`, `:viewed`, `:flashing`, `:bogus`, `:ws-loaded`
- Buffers/processes/timers: `:vterm-buffer`, `:input-buffer`, `:ready-timer`, `:git-proc`, `:merge-proc`
- Git/branch state: `:git-clean`, `:branch-merged`, `:branch-merged-last-check`, `:detail-branch`, `:detail-dirty-count`, `:detail-last-commit`, `:detail-last-commit-time`, `:detail-master-ahead`, `:detail-source-ahead`
- Merge lifecycle: `:merge-completed`, `:merge-completed-at`, `:merge-failed`, `:merge-conflict`, `:merge-queued`, `:merge-parent-dir`, `:merging`, `:merged`
- Prompts/UI: `:pending-prompts`, `:pending-show-panels`, `:pending-initial-buffers`, `:pending-magit`, `:deferred-prompts`, `:last-prompt-text`, `:last-prompt-time`, `:last-prompt-summary`, `:last-prompt-summary-pending`, `:last-notify-time`, `:clipboard`, `:saved-tab-index`, `:fullscreen-config`, `:ai-title-cache`
- Counters: `:prefix-counter`, `:counter`
- State-machine flag keywords (set as `t` when active): `:active`, `:idle`, `:thinking`, `:done`, `:permission`, `:init`, `:inactive`, `:hidden`, `:dead`, `:stop-failed`

If a key is absent from the file, it means it was never set on this workspace (treat it as `nil`).

**This is NOT where the rendered workspace color comes from.** The Emacs-persisted `:agent-state` was replaced entirely by the SSM database — see §4. Read `memory-state.el` for what Emacs believes about buffers, timers, and lifecycle; read the SSM for what determines the color.

#### 2.5.4 When to use it

Read `memory-state.el` instead of the rolling log when:

- You need to know **what state the workspace is in *right now*** — the log shows transitions, the snapshot shows the destination.
- You need a **dirty list of timers/processes/buffers** owned by a single workspace (the trailing log metadata only carries a few of these).
- You're cross-checking the log against ground truth — e.g. log says `claude-state -> :done`, snapshot says `:claude-state :thinking` → there's an unwritten-back transition or a sync bug.
- The user mentions `SPC j h p` or asks "what does the dump look like for ws X" — read the file instead of asking them to run the command.

Read the rolling log file (§2.1–§2.4) instead when:

- You need history, ordering, or "what fired between T1 and T2".
- You're looking for warnings, errors, sentinel callbacks, or hot-path traces — those go to the log, not the snapshot.

#### 2.5.5 Caveats

- The snapshot is **only refreshed on `:claude-state` or `:repl-state` changes.** Other plist mutations (counters, pending-prompts, prompt summaries) don't trigger a write on their own — they appear in the snapshot only when the next state transition flushes the file. So the snapshot can lag for non-state fields between transitions. If you need a guaranteed-fresh dump, ask the user to run `SPC j h p`.
- Stub workspaces (entries without `:project-dir`) have no snapshot — they appear in the log's `STUB-CREATE` lines instead.
- A killed buffer renders as `#<buffer nil dead>` because Emacs clears `buffer-name` on kill — the `nil` is normal, not a bug.

## 3. The daemon plane

`~/.claude-emacs/claude-repld.log` is the daemon's full stderr, teed to disk. **Three planes land in this one file** — the daemon itself, the shim (§3.4), and the webapp (§3.5) — so it is the highest-yield file in the system.

### 3.1 The file

- Path: `~/.claude-emacs/claude-repld.log`
- Line format: `YYYY/MM/DD HH:MM:SS.ffffff <message>`
- **Rotation is restart-scoped.** On every boot the previous file becomes `claude-repld.log.<mtime-stamp>` (stamp format `20060102-150405`), and only the newest **5** backups survive. Implemented in `daemon/internal/replog/replog.go` (`FileName`, `KeepBackups = 5`).
- There is also an in-run size cap (`CapBytes`) that rotates mid-run if a single run logs too heavily. So a backup file is not necessarily a previous *boot* — check timestamps before assuming.

```sh
ls -lt ~/.claude-emacs/claude-repld.log*     # find the run you want
tail -n 300 ~/.claude-emacs/claude-repld.log
tail -f ~/.claude-emacs/claude-repld.log     # live tail during repro
```

### 3.2 Structured tags — grep the same way as the Emacs log

`daemon/internal/dlog/dlog.go` holds the logging combinators. Two conventions matter:

**`Tag(logf, k, v, …)` / `TagFunc`** wrap a logger so every line it emits carries a trailing ` {k=v k2=v2}` block. This is **deliberately the same shape as the Emacs log's `{ws=… id=…}` block (§2.2), so both logs grep the same way**:

```sh
grep 'ws=/path/to/worktree' ~/.claude-emacs/claude-repld.log | tail -60
grep 'ws=/path/to/worktree' ~/.claude-emacs/doom-agent-repl.log | tail -60
```

A mis-paired call site renders `key=!MISSING` rather than silently shifting the pairs — so if you see `!MISSING`, that is a real instrumentation bug, not a truncation artifact:

```sh
grep '=!MISSING' ~/.claude-emacs/claude-repld.log
```

### 3.3 Call-boundary logging

**`Call(logf, name, kv…)`** logs BOTH edges of an external model or subprocess call — classifier, summarizer, remediation analyst, the shim. It returns a completion func that writes the duration, the output clamped to `OutputClampLen` (2000 bytes, marked ` …[clamped]`), and the error.

The rationale is worth internalizing, because it is also the standard you should hold new code to:

> A call that logs only its failure is unattributable; a call that logs nothing is invisible.

When a model-boundary call misbehaves, look for the **pair** of lines. A start with no completion means the call never returned — a hang, not an error.

### 3.4 Shim logs live here, and ONLY here

`shimLog()` in `agent-shim/claude/shim/src/uds/log.ts` emits

```
HH:MM:SS.mmm [component] {k=v} message
```

to **stderr only** — stdout and the UDS carry protocol frames exclusively, so nothing else may be written to them. The daemon pumps that stderr into its own log with a `shim stderr: ` prefix (`daemon/internal/shim/proc.go:128`).

**The shim has no log file of its own.** This is the only place to read it:

```sh
grep 'shim stderr:' ~/.claude-emacs/claude-repld.log | tail -100
```

### 3.5 Webapp client-logs are forwarded here too

`webapp/src/wslog.ts` defines `ForwardingLogger`: every line goes to the JS console **always**, and is additionally best-effort mirrored to the daemon as a `client-log` frame (handled at `daemon/internal/frontend/commands.go:139`).

Why this exists: the webapp runs inside an Emacs webview whose JS console **nobody can see and nothing persists**. Before forwarding, a delivery-path failure — a seq-gap loop, a lost replay-request, a stalled `requestAnimationFrame` — left no evidence anywhere at all.

- Rate limit: `MAX_FORWARDS_PER_WINDOW = 60` per `FORWARD_WINDOW_MS = 60_000`. The console side is never limited, so a burst that exceeds the window is still visible in-page even when it is missing from the daemon log.
- **Timestamps are stamped by the webapp**, not at daemon receipt (`HH:MM:SS.mmm` UTC — the same slice the shim's `stamp` uses). This is precisely so a webapp line and a shim line sitting in the same daemon log read on **one clock**. Stamping at receipt would have hidden the delay between the two, which is usually the thing you are trying to measure.

Reach for this plane for UI-only symptoms: scroll misbehavior, render stalls, sequence gaps, replay that never arrives.

### 3.6 Registry, sockets, and locks

- Session registry: `~/.claude-emacs/claude-repld-sessions.json`, plus `~/.claude-emacs/claude-repld-sessions.json.checkpoints`
- Sockets, in `~/.cache/agent-repl/sock/`:
  - `daemon-frontend.sock` — the editor/webapp side
  - `daemon-shim.sock` — the shim side
  - `session-s_<hex>.sock` — one per session
- Session locks: `~/.cache/agent-repl/run/session-s_<hex>.lock`

```sh
ls -la ~/.cache/agent-repl/sock/ ~/.cache/agent-repl/run/
jq '.' ~/.claude-emacs/claude-repld-sessions.json
```

A session with a registry entry but no socket is a session the daemon believes in and cannot reach.

## 7. When the evidence is too sparse — SUGGEST INSTRUMENTATION EMPHATICALLY

Since file writes are now unconditional, the only way the log can be uninformative is if the suspect code path has **no `agent-repl--log` calls at all**. When that happens, Claude tends to fill the silence with speculation. **Do not do that.** If you find yourself reading the log around the timestamp of the bug and cannot find any line that corresponds to the suspect function or branch, stop and surface this emphatically.

Required user-facing message in that case:

> ⚠️ The log around `<timestamp>` contains no entries for `<function/feature>`. I cannot confidently diagnose this from the log alone. I **strongly recommend** adding `agent-repl--log` calls at the following points before reproducing again:
>
> - `<file>:<line>` — `<function>` (entry + which branch was taken)
> - `<file>:<line>` — `<function>` (the suspected early-return)
>
> Want me to add them?

Apply this rule when **any** of the following is true:

- The suspect function has zero `agent-repl--log` / `agent-repl--log-verbose` / `agent-repl--error` calls
- The conditional branch you suspect (an `if` arm, an early return, an error fallback) has no log line
- The hot code path appears to have run but produces no log evidence of which branch was taken
- The repro produces a visible symptom but the log between the user's action and the symptom is empty or routine

Be **emphatic, not soft**. Say "strongly recommend", "I need more instrumentation before I can be confident", "the log is uninformative here" — do not bury the recommendation under speculation. The instrumentation is cheap, lives in the same module, and the user has explicitly chosen this debugging style. Adding three or four `agent-repl--log` calls and re-running the repro is almost always faster than guessing.

### 7.1 Template — Emacs (elisp)

When proposing new log calls, follow the module's conventions:

```elisp
;; Default (gated on `agent-repl-debug'):
(agent-repl--log ws "myfn: entered cond=%s key=%s" cond key)

;; Verbose (gated on `agent-repl-debug = 'verbose') — for hot/timer paths:
(agent-repl--log-verbose ws "myfn: tick state=%s" state)

;; Unconditional (always written, even with debug off) — reserve for
;; invariant violations or state shapes that must never be lost:
(agent-repl--do-log ws "myfn: INVARIANT-BREACH cond=%s" (list cond))

;; Error — logs AND signals:
(agent-repl--error ws "myfn: refusing to do X because Y=%s" y)
```

Always pass the workspace as the first argument so the trailing `{ws=…}` metadata block carries context. Use `nil` only for genuinely workspace-free contexts (loaders, global hooks before any workspace is known).

Prefer logging:

- **Function entry + the values that drove the branch decision** — not just "entered fn".
- **Both arms of an `if`/`cond`** — silence on one arm is ambiguous; explicit logs on both arms remove the ambiguity.
- **Early returns and error fallbacks** — the most common place a bug hides.
- **Async sentinel callbacks** — log on every invocation, with the relevant state, since async paths are hardest to reason about.

## 8. What NOT to do

- Do not mutate the log file. It is append-only by the module.
- Do not mutate `memory-state.el`. It is rewritten by Emacs on every state change; any edits will be clobbered.
- Do not infer state by reading `*Messages*` when the same data is in the log file — the log file is canonical for agent-repl events. (Reading `*Messages*` for 3rd-party output that the log does NOT carry is fine and covered in §9.)
- Do not silently rotate or truncate the log unless the user explicitly asks; some bugs need historical context.
- Do not skip §7. If the evidence lacks coverage of the suspect code path AND that path is in `modules/app/agent-repl/`, surface that emphatically and propose instrumentation — that IS the right next step. (If the path is 3rd-party, use §9 instead — you don't get to add `agent-repl--log` calls to magit.)

## 9. Capturing 3rd-party output via the *Messages* buffer

The agent-repl log file contains ONLY messages emitted by `agent-repl--log` and friends. Output from 3rd-party packages — magit, transient, doom-core, vterm, the byte-compiler, package load failures — writes to Emacs's `*Messages*` buffer and is NOT mirrored to the log file. When a bug surfaces *through* agent-repl but is caused by 3rd-party code, the only on-host record of the upstream failure lives in `*Messages*`.

Dispatch `/runtime-eval-code` with intent "dump `*Messages*` to disk" to capture it, then read the dump from this session. The snippet, dump path, and follow-up grep recipes live in that skill — do not duplicate them here.

### 9.1 When to reach for this

- The bug presents at Emacs startup or during a path where agent-repl is downstream of the failure (magit failing to load, transient redefinition warnings, byte-compile errors after a package bump).
- The agent-repl log around the timestamp has no entry for the suspect path AND that path is NOT in `modules/app/agent-repl/`. §7-style instrumentation does not apply because the failing code is 3rd-party.
- The user describes a symptom they saw flash in the echo area but the log file doesn't show.
- Investigating package version drift, byte-compile breakage, or upstream API changes during a refactor or dep bump.

### 9.2 What NOT to use this for

- Replacing missing `agent-repl--log` instrumentation in agent-repl code. If the gap is in `modules/app/agent-repl/`, fix the instrumentation per §7; do not paper over it with a `*Messages*` dump.
- "Just in case" dumps when you already have log evidence. The dump is an extra round-trip; prefer the log when it has the answer.
