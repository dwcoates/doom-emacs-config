# Design: single-source-of-truth status system (sidecar + SQLite + daemon-derived state)

Status: **PLANNED.** Nothing below has shipped. This document is the agreed
design record produced from an extended design conversation; it captures the
motivation, the current-state audit, the target architecture, and the
explicit deferrals.

Scope: `modules/app/agent-repl/` (`shim/`, `daemon/`, `sidecar/` (new),
`webapp/`, the Emacs elisp, and `shared/protocol.md`).

All `path:line` anchors are relative to `modules/app/agent-repl/` unless
absolute. Grounding for the SDK surface: `@anthropic-ai/claude-agent-sdk@0.1.77`,
read from the resolved install at
`~/.cache/agent-repl/node-store/shim-d871d1a51d2fab56/node_modules/@anthropic-ai/claude-agent-sdk/`.

---

## 0. Motivation

### 0.1 The concrete bug that started this

A workspace (`drop-watched-channels-bum`) sat pinned at `:thinking` (red tab)
long after its turn had visibly finished (the GUI showed a settled response
bubble). Root cause:

- The `:thinking -> :done` transition is gated by
  `agent-repl--fully-stopped-p` (`status.el:455`), which requires BOTH
  `:stop-received` t AND `:pending-subagents` = 0.
- The Stop hook fired while `:pending-subagents` was 1 (a `SubagentStart`
  had incremented it at `sentinel.el:511`), and the matching `SubagentStop`
  that would decrement it (`sentinel.el:518`) never arrived.
- So the conjunction never resolved, `agent-repl--handle-agent-finished`
  was never called, and the tab stayed `:thinking` forever.

The counter itself is fragile by construction: see the block comment at
`status.el:375-413`, which documents an empirical hook asymmetry (one
unpaired `SubagentStop` per turn) and a load-bearing floor-at-zero hack in
`agent-repl--ws-decf-pending-subagents` (`status.el:438-453`) to keep the
counter from drifting to negative infinity across a session.

### 0.2 The deeper problem: status handling is fragmented

Status is derived independently in at least three places, along divergent
mechanisms:

1. **`:pending-subagents`** (Emacs, `status.el`) — a hook-fed counter that
   gates turn finalization. Knows only about subagents. Edge-triggered and
   drift-prone (§0.1).

2. **`:async-live`** (Emacs, `frontend-client.el:802`) — a single integer
   captured from the daemon's `async_live` field on the `GET /sessions`
   poll, driving the amber `:idle-async` tab state
   (`workspace.el:588-594`, `:704` vs `:713-714`). Level-triggered and
   self-healing, but only consulted AFTER `:idle`, so `:thinking` (from the
   fragile counter) beats it and it can never fire while the counter is
   stuck.

3. **The daemon's `classifyAsyncSource`** (`daemon/internal/session/asyncsource.go:70`)
   — the ONLY total, typed model of async work (kinds `agent`, `shell`,
   `workflow`, with a status enum), derived from the SDK's structured
   `tool_use_result`. It is collapsed to a bare integer before Emacs sees
   it.

The webapp derives its own status separately from the same daemon frames,
so Emacs and the webapp can disagree. The representation ranges from a typed
Go struct to a bare elisp int to a counter-plus-boolean, with no shared
model. Reconciliation models are opposite: the counter drifts, `async_live`
self-heals.

### 0.3 The goal

One source of truth. All async/turn state is tracked durably in a database
the daemon owns; the daemon derives the single resolved render-state; every
consumer (Emacs tab-bar, sidebar, webapp bubbles) reads that resolved state.
Emacs becomes a dumb renderer with NO say in state.

---

## 1. What the SDK does and does not give us

Verified against the installed SDK typings.

### 1.1 Tool execution is the SDK's, not ours

The `claude` subprocess and its tool-execution loop are owned by the Agent
SDK, driven by the shim (`shim/src/main.ts:141-151`, `sdk.query(...)`). The
Go daemon supervises the shim and consumes its Layer-1 NDJSON stream; it
never executes a tool. Everything we own is downstream of the loop.

Programmatic surfaces available to observe the loop (no log scraping):

- The `query()` message stream (async iterator), consumed by the shim, with
  `includePartialMessages: true` (`shim/src/main.ts:128`).
- `canUseTool`, an in-process callback fired before each tool with full
  name+input (`shim/src/main.ts:127`).
- The SDK `hooks` mechanism, loadable in-process or from `settings.json` via
  `settingSources` (`shim/src/main.ts:131`).

### 1.2 The stream does NOT push detached-work completion

The full `SDKMessage` union (`coreTypes.d.ts:553`) has no "background task
finished" variant. Specifically:

- `SDKResultMessage` (`coreTypes.d.ts:441`) fires once at end-of-turn only.
- `SDKToolProgressMessage` (`coreTypes.d.ts:536`) exists but is a bare
  elapsed-time heartbeat (`tool_use_id`, `tool_name`,
  `elapsed_time_seconds`) with no output payload and no terminal status.
  *Kept in mind as a future signal for staleness/timeout detection.*
- A detached task's output re-enters the stream only when the model itself
  polls it (`sdk-tools.d.ts:54` for Agent, `:83` for Bash), which is
  model-driven, not pushed.
- The `Query` control interface (`runtimeTypes.d.ts:87`) exposes no
  background-output/status accessor either.

**Consequence:** proactive, reliable detection of detached-work progress and
completion comes only from tailing the harness spool/journal files. This is
the actual hard part, and no part of this design removes it — it only
relocates and hardens it.

### 1.3 Claude outlives a dead daemon (the drift window)

On a daemon crash, the shim sees only stdin EOF; `handleStdinEnd`
(`shim/src/session.ts:188`) ends the prompt input but does NOT interrupt the
in-flight turn. So the current turn's `claude` child, plus any background
tasks, keep running and keep writing their spool files with no tailer
watching. This is why the file-watcher must live in a process whose lifetime
is independent of the churning daemon (§2.1).

---

## 2. Target architecture

### 2.1 A dedicated sidecar owns file-watching and DB writes

A new, small, rarely-changed process (`sidecar/`) is the sole observer of
harness spool artifacts and the sole writer of the status database.

- **Independent lifetime.** Its own `launchd`/`systemd` user unit,
  single-instance via lockfile, so it survives every daemon restart and
  covers the §1.3 drift window.
- **Stability by design.** Kept tiny and change-averse. Churn lives in the
  daemon; the sidecar is meant to almost never change so it is maximally
  reliable at catching updates.
- **Sole writer.** Only the sidecar writes the DB (WAL mode). The daemon is
  a reader.

### 2.2 The two spool roots the sidecar watches

Grounded in `daemon/internal/session/tailer.go`:

- **Root A — the per-session tmp task spool:**
  `/tmp/claude-<pid>/tasks/*.output` (`taskSpoolRe`, `tailer.go:49`). Holds
  background shells AND background agents (the agent's file is its full JSONL
  transcript, `tailer.go:50`).
- **Root B — the workflow journals:** under the session's own Claude config
  root, `.../projects/**/subagents/workflows/<run>/journal.jsonl`
  (`allowedJournalPath`, `tailer.go:115`).

Each detached task instance is one artifact (a file, or a workflow's
directory with `journal.jsonl`), keyed by task id.

### 2.3 SQLite schema (event-log, NOT table-per-task)

Table-per-task is rejected: SQLite wants few tables and many rows, and
per-task `CREATE TABLE` churns the schema and defeats the cross-task "is any
async live for this session?" query that consumers need. Instead:

- **`task`** — one row per detached task instance.
  - `task_id` (pk), `session_id`, `kind` (`agent`|`shell`|`workflow`),
    `label`, `output_path`, `started_at`, `ended_at` (nullable),
    `terminal_status` (`done`|`error`|`killed`, nullable while running).
- **`task_event`** — append-only log of every observed update.
  - `task_id` (fk), `seq`/`offset` (for idempotent replay), `ts`, `kind`
    (`start`|`progress`|`end`|`unparsed`), `payload`.
- Indexed by `(session_id, task_id)` and by `ended_at IS NULL` for the
  live-async query.
- Merge-state columns/rows are added here too (§3.4).

Idempotency: events carry a byte offset / sequence so a sidecar restart
re-reads without double-inserting.

### 2.4 The daemon derives resolved state; Emacs renders it

- The daemon reads the DB and computes the single closed-set render-state
  keyword per workspace, folding in turn-state (derived from the SDK stream
  it already holds) and live-async (from the DB).
- The precedence ladder currently in `agent-repl--ws-render-status`
  (`workspace.el:596-715`) moves into the daemon so `:idle-async` becomes
  the daemon's conclusion, not Emacs's.
- Emacs (and the webapp) map a state keyword to a color. One derivation, one
  place, killing the Emacs-vs-webapp divergence.

---

## 3. Component designs

### 3.1 Sidecar watch loop

- Bootstrap: glob both spool roots (§2.2) on startup to catch tasks created
  while the sidecar was down, then switch to `fsnotify`-style watching.
- Per file: read from last recorded offset, parse incremental records,
  append `task_event` rows, and upsert the `task` row.
- **Resilient-but-loud (§4):** one bad line or file is logged loudly and
  recorded as an `unparsed` event, never fatal. An external kill is covered
  by supervised restart with offset-replay.
- **Completion detection:** "ended" is inferred from harness markers (the
  `.output` terminal marker / journal terminal record / process exit) and
  must be an explicit, logged decision, never assumed. An `unparsed` row
  must never be interpreted as a real transition such as `done`.

### 3.2 Malformed data is persisted, not skipped

When a record cannot be parsed, write a `task_event` of kind `unparsed`
carrying the raw bytes, the offset, and the parse error, so the daemon still
knows *something arrived* even when we cannot classify *what*. Guardrails:
bound the stored size, redact secrets from spool bytes, and never let an
`unparsed` row count as a real state transition.

### 3.3 Sidecar <-> daemon integration

- **SQLite is the entire contract:** the sidecar writes, the daemon only
  queries. The daemon's own tailer (`tailer.go`) is deleted, not moved (the
  logic is carefully reimplemented in the sidecar so only one process
  watches the spool — no double-read race).
- **Path hints:** while the daemon is up, it POSTs its structured launch
  announcements (`task_id`, `output_path`, `kind` from the
  `AsyncSource`/`AsyncSourceFrame`, `asyncsource.go:70`) to the sidecar over
  a small local socket, so the sidecar stays sole writer without
  regex-guessing paths. When the daemon is down, the sidecar's own FS
  discovery covers the gap.
- **WAL** lets the daemon read concurrently with the single sidecar writer.

### 3.4 Merge-state centralization (Phase 1 resolution of the wrinkle)

Today merge/git state is produced AND consumed entirely in Emacs, which
conflicts with the dumb-renderer goal:

- Producers (move to the daemon): `agent-repl--finalize-merged-workspace`
  (`merge-handlers.el:349-372`), `:merge-failed`/`:merge-conflict`/
  `:merge-queued` setters, and the cherry-pick driver
  (`agent-repl--merge-handler-cherry-pick`, `merge-handlers.el:227-236`).
- Consumer (stays in Emacs, changes its input source): the merge arms of
  `agent-repl--ws-render-status` (`workspace.el:685-702`) read the resolved
  merge keyword from the daemon feed instead of Emacs-written plist keys.
- The daemon runs the cherry-pick via `git -C <source-dir>` and writes
  merge-state transitions to the DB, exactly as it derives agent/async
  state. The existing daemon-to-Emacs command channel already proves the
  pattern (`daemon/internal/workspacecmd/workspacecmd.go`, invoked from
  `server.go:546-567` and `:580-606`).
- **Conflict UX stays in Emacs, reactively.** On a conflict the daemon
  publishes `:merge-conflict`; Emacs opens magit on the conflicted worktree
  as a *consequence* of rendering that state. State ownership stays with the
  daemon, so one-source-of-truth holds even though the resolution UI stays
  in Emacs.
- **Open detail:** the resolve-and-continue handoff — after a human resolves
  in magit and continues the cherry-pick, who detects completion and writes
  `:merged`. To be designed (§6).

### 3.5 Daemon <-> Emacs integration

- Extend `GET /sessions` (the endpoint Emacs already polls,
  `frontend-client.el:802`) to carry the resolved per-workspace state, not
  raw facts.
- Emacs needs almost only turn-state, with two genuine exceptions that must
  still be relayed:
  1. The amber `:idle-async` state needs the live-async signal, which by
     definition is not turn-state (the §0.1 bug). Resolved by the daemon
     folding it into the reported state (§2.4).
  2. Session-start must carry its source (resume vs compact), because Emacs
     re-fires the metaprompt read-directive on those
     (`agent-repl--fire-metaprompt-read`, `sentinel.el:596`). This is an
     action, not state; its true home is whoever submits prompts (the
     shim/daemon), so it moves there too.

### 3.6 Emacs drops all status sentinels and hooks

Removed DURING the integration, not after (keeping the old path live beside
the new one only conflates two sources of truth; a buggy removal is a
follow-up fix):

- `sentinel.el`'s dispatch handlers for `stop_`, `stop_failure_`,
  `subagent_start_`, `subagent_stop_`, `prompt_submit_`, `session_start_`
  (`sentinel.el:500-596`, dispatch alist `~:690-757`).
- The `:pending-subagents` counter machinery and `agent-repl--fully-stopped-p`
  (`status.el:375-471`).
- `install.el`'s managed shell hooks written into `~/.claude/settings.json`
  (`install.el:43-51`).

Emacs becomes a pure reader of daemon-published state.

---

## 4. Logging and resilience contract

The sidecar (and the DB-writing path generally) must be VERY noisy on any
anomaly while being VERY resilient to crashes/kills:

- Hard-fail-LOG (loudly) every parse error, unknown field, and missing field
  the instant it happens, because a silently-missed read is invisible drift.
- Never let one bad line/file crash the watch loop; isolate, log, persist an
  `unparsed` event (§3.2), and continue.
- Survive external kills via supervised restart plus offset-replay
  idempotency.
- **Deliverable:** the daemon's `AGENTS.md` (to be created for
  `modules/app/agent-repl/daemon/` and/or the new `sidecar/`) documents this
  loud-logging contract and exactly how to investigate those logs.

---

## 5. Explicitly deferred (Phase 2)

- **`/create-or-update-workspace` as a bonafide MCP tool.** The SDK supports
  it (in-process `McpSdkServerConfig`, `coreTypes.d.ts:48`, which must live
  in the shim; or a daemon-hosted http/sse server by URL). This is a pure
  agent-ergonomics enhancement (workspace ops become first-class tool calls
  visible in the stream), orthogonal to merge-state centralization — Phase 1
  achieves the centralization over the existing dispatch channel without it.
- **Reentrancy hazard (Phase 2 only).** A blocking `merge`/`close` MCP tool
  call runs mid-turn while the SDK awaits its `tool_result`, but merging or
  closing your own workspace tears down the very session that is waiting.
  Today's skill sidesteps this by returning exit 0 immediately
  (fire-and-forget). Phase 1 keeps that fire-and-forget dispatch, so no turn
  ever blocks on tearing down its own session; the hazard is specific to the
  MCP model and parks with Phase 2.

---

## 6. Open questions

1. Push vs poll to consumers: does Emacs/webapp keep polling `GET /sessions`
   (level-triggered, self-healing) or does the daemon push deltas derived
   from DB reads for snappier tab colors? (Backing store stays SQLite either
   way — it is never the live channel.)
2. The merge resolve-and-continue handoff (§3.4): after a human resolves a
   conflict in magit, what detects completion and writes `:merged` — the
   sidecar observing the worktree, Emacs signaling the daemon, or the daemon
   polling git state?
3. Sidecar supervision specifics: `launchd` vs `systemd` unit, who installs
   it, and how it discovers active sessions/config roots when the daemon is
   down.

---

## 7. Known hard part (carried, not solved)

Detecting that a detached task ENDED is inferred from harness markers, not
handed to us by SQLite or the SDK (§1.2). This design relocates the
detection into a durable, independently-lived, loudly-logged sidecar and
persists even malformed observations — but the detection itself remains the
irreducible difficulty, and its correctness (§3.1 completion detection) is
the single most important thing to get right.
