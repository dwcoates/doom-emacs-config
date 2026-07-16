# Design: watcher-bubble expansion (live background-task progress in a response bubble)

Status: **proposal — nothing implemented.**
Scope: `modules/app/agent-repl/` (`webapp/`, `daemon/`, `shared/protocol.md`).

All `path:line` anchors are relative to `modules/app/agent-repl/`.

---

## 1. Motivating scenario

An agent turn ends with a final-response bubble whose prose references a
long-running background watcher, e.g.:

> "the pickup watcher is armed on its polling task and will report when the
> merge queue picks up the PR"

Behind that sentence sit harness-tracked background processes: an Agent-tool
subagent, a backgrounded Bash task polling a script, and a Monitor armed on it.
Today the bubble is inert prose. The goal: **click the bubble (or an affordance
inside it) → it expands to live watcher progress (state, elapsed, latest output
lines), updating in realtime while the watcher runs.**

---

## 2. The headline finding: most of this already exists

The single most important result of the investigation is that the end-to-end
plumbing for *live background-task data* is already built and flowing. The novel
work is almost entirely **linking the final-response prose bubble to that data
and rendering it in place** — not moving new data across the wire.

### 2.1 Signal already carried end-to-end

- Layer-2 frames already carry every relevant background-task signal:
  - `ToolUseProgressFrame` — heartbeat with `ToolName`, `ParentToolUseID`, `ElapsedSeconds` (`daemon/internal/protocol/layer2.go:239-250`; spec §2.6 `shared/protocol.md:844-861`).
  - `TaskOutputDeltaFrame` — live growth of a detached task's output file, `TaskID` + `ToolUseID` + `Text` (`daemon/internal/protocol/layer2.go:256-261`; spec `shared/protocol.md:863-879`).
  - `TaskNotificationFrame` — terminal completion, `ToolUseID` + `TaskID` + `Status` + `Summary` + `OutputFile` + `Text` (`daemon/internal/protocol/layer2.go:269-277`; spec `shared/protocol.md:881-900`).
  - `parent_tool_use_id` subagent attribution on `text/thinking/tool-use` start frames (`daemon/internal/protocol/layer2.go:144,166,189`).
- The daemon already detects spawns and tails their output files: `parseSpawnAnnouncement` extracts `task_id` + output path + spawning `tool_use_id` (`daemon/internal/session/tailer.go:53-92`), and `superviseTailersLocked` starts/stops a tailer per `TaskID` (`daemon/internal/session/tailer.go:121-133`), broadcasting `task-output-delta` (`daemon/internal/session/tailer.go:157-173`).
- The shim forwards it: `tool_progress` SDK messages → `tool_use_progress` (`shim/src/session.ts:473-481`), `<task-notification>` text → `task_notification` (`shim/src/session.ts:506-517`), and `includePartialMessages: true` streams subagent internals (`shim/src/main.ts:128`).

### 2.2 Store model already holds it

- `ToolItem` already carries the live fields: `parentToolUseId` (`webapp/src/store.ts:65`), `progress` + `progressElapsedS` (`webapp/src/store.ts:69-71`), `notification{taskId,status,summary,outputFile,text}` (`webapp/src/store.ts:73-79`), `taskOutput` (`webapp/src/store.ts:83`), `result` (`webapp/src/store.ts:84-88`).
- Reducers already populate them: `tool-use-progress` (`webapp/src/store.ts:612-619`), `task-output-delta` (`webapp/src/store.ts:620-624`), `task-notification` (`webapp/src/store.ts:625-643`).

### 2.3 Correlation already parses watcher ids

- `SPAWNED_ID_RE = /\b(?:ID|agentId):\s*([A-Za-z0-9_-]+)/g` (`webapp/src/partition.ts:34`).
- `spawnedTaskIds(item)` returns every watcher id a spawning ToolItem announced in its result **or** notification (`webapp/src/partition.ts:44-48`).
- `spawnByTaskId` builds the inverse `task_id → spawning tool_use_id` map (`webapp/src/partition.ts:87-95`).
- Children already nest under their spawning card by `parent_tool_use_id` (`webapp/src/partition.ts:84-108`), and the activity panel already renders a subagent's child feed + a live tail (`webapp/src/render.ts:471-507`, `webapp/src/render.ts:604-607`).

### 2.4 What is actually missing

1. **Bubble → watcher linkage.** There is no stored `turnId`; `items` is one flat array (`webapp/src/store.ts:219`), and the final-response bubble is only *derived* as the last main-chain `TextItem` before a `success` result (`webapp/src/render.ts:976-994`, esp. `render.ts:983-988`). Nothing today maps that bubble to the watcher ids spawned in its turn.
2. **Background AGENTS get no live tail.** The tailer only recognizes a Bash spool path or a workflow journal (`daemon/internal/session/tailer.go:43-48`); an `agentId:`-announced detached agent has no `Output is being written to:` line, so no tailer starts and `taskOutput` stays empty for it — its only signal is the terminal notification.
3. **Elapsed freezes.** `progressElapsedS` (`webapp/src/store.ts:71`) only advances while `tool-use-progress` frames arrive, which stop once a backgrounded tool's result lands — a long-lived watcher shows a frozen clock.
4. **No structured progress.** `taskOutput` is opaque bytes and `progress` is a free-text heartbeat line — there is no percent/step/state field anywhere on the wire.
5. **No roster progress field.** `CounterEntry` is a 4-state status only (`webapp/src/counter-menu.ts:44-58,27`).

---

## 3. Bubble → watcher association (investigation area 3)

Central fact shaping every option: the watcher id is **never** in a tool_use
block's *input* — it is assigned by the harness and first appears in the
spawning tool's *result* text and again in the completion notification
(`webapp/src/agents.ts:74`, `webapp/src/tasks.ts:11-16`).

### 3.1 Candidate A — SDK tool_use blocks carried through Layer-2 (pure webapp projection)

- **Exists today:** every watcher id is already in the store and already parsed (`webapp/src/partition.ts:34-48,87-95`); the turn is already derivable via the `finalResponses` pairing that resets on `user-turn` and pairs the last main-chain text with the `success` result (`webapp/src/render.ts:976-994`); an id→card reveal already ships (`planAgentReveal` `webapp/src/render.ts:1372-1406`, wired from roster clicks `webapp/src/main.ts:219-221`, `webapp/src/counter-menu.ts:186`).
- **To add:** a turn-slice helper (walk from the bubble's `TextItem` back to its `user-turn` and forward to its paired `result`), then a projection composing that slice with `spawnedTaskIds()`; optionally intersect with ids literally named in the bubble text.
- **Reliability: High** for turn-scoped mapping (all ids already present); **Medium** if strict "the text talks about id X" is demanded, because the id regexes are wording-coupled (`webapp/src/tasks.ts:11-19`).
- **Coupling: Low** — pure webapp projection, zero wire-protocol change.

### 3.2 Candidate B — explicit marker convention in the response text

- **Exists today:** only emit-side. `show-chess-game/run.sh:52` emits `---> agent-repl-chess-game-file: … <---` and `SKILL.md:34-41` tells the agent to re-emit it verbatim, but **the webapp never detects it** (zero hits for the marker in `webapp/`/`daemon/`), and `markdown.ts` runs `html:false` (`webapp/src/markdown.ts:61`) so the line renders as literal text. The genuine working precedent for "detect a pattern in response text → swap in a widget" is the metaprompt-tree fence special-case (`webapp/src/markdown.ts:84-94`).
- **To add:** a render-time pass detecting a `<watcher-ref: id …>` marker and rewriting it to an interactive chip, plus a skill/agent convention obliging the model to emit it.
- **Reliability: Low** — hinges on the model emitting the marker verbatim every time; the chess reference is itself unproven (unrendered) in this branch.
- **Coupling: Low** — self-contained in the webapp render layer.

### 3.3 Candidate C — daemon-side per-turn bookkeeping

- **Exists today:** the daemon already sees both spawn (`daemon/internal/session/tailer.go:53-92`) and completion (`daemon/internal/session/translate.go:836-844`) with `task_id ↔ tool_use_id`, but keeps only a bare `turnActive` boolean with no membership (`daemon/internal/session/translate.go:34-38,173,697`); `s.tailers` is `task_id → stop channel` and is torn down on completion (`daemon/internal/session/session.go:125-127`, `daemon/internal/session/tailer.go:121-133`).
- **To add:** a per-turn ledger on the `Translator` plus a **new Layer-2 frame** (or a `ResultFrame` extension) carrying the turn→watcher association, its `KNOWN_FRAME_TYPES` entry (`webapp/src/protocol.ts:453-458`), a store reducer, and a `shared/protocol.md` update.
- **Reliability: High** — server-side single source of truth, survives replay.
- **Coupling: High** — a cross-layer protocol change, the heaviest option.

### 3.4 Association comparison

| Candidate | What exists | To add | Reliability | Coupling |
|---|---|---|---|---|
| A: webapp projection | ids parsed + turn derivable + reveal wired | turn-slice + projection | High (turn-scoped) | Low (no protocol) |
| B: text marker | emit-only precedent, pattern-swap precedent | marker parse + agent convention | Low (model must emit) | Low (render only) |
| C: daemon ledger | daemon sees spawn+completion | Translator ledger + new L2 frame | High (authoritative) | High (cross-layer) |

**Chosen: Candidate A.** It is the only option with zero protocol coupling that
reuses already-present data, and it degrades gracefully (a bubble with no
spawned watchers simply renders no affordance). Candidate C is the fallback if a
server-authoritative, replay-durable ledger is later required.

---

## 4. Expansion UX pattern (investigation area 4)

The binding constraint is that the feed re-renders by assigning
`entry.el.innerHTML = html` whenever an item's HTML changes
(`webapp/src/render.ts:1884-1906`, the assignment at `render.ts:1903`), which
destroys all descendant DOM state. A **live** region rewrites every tick, so it
hits that assignment every tick.

### 4.1 Precedents surveyed

- `installClickExpand` stores open state as a single `.expanded` CSS class on the node (`webapp/src/expand.ts:39,104-111`, CSS `styles.css:565-569`) and survives re-render only via a fragile capture/re-apply round trip (`expandedKeys`/`applyExpanded` at `webapp/src/expand.ts:143-157`, invoked around the swap at `render.ts:1902-1904`) and only for `CAPPED_SELECTOR` elements (`webapp/src/expand.ts:26-36`).
- Thinking `<details>` blocks re-emit their `open` attribute every render as a pure function of `item.done` (`webapp/src/render.ts:394-398`), so they survive re-render without a round trip — but their open state is not user-controlled.
- The **ActivitySection fold** stores open state in a renderer-owned `openPanels: Set<string>` (`webapp/src/render.ts:1510`), read through `PanelContext.isOpen` (`webapp/src/render.ts:1644`), toggled by a delegated `data-panel-toggle` handler that mutates the Set then re-renders (`webapp/src/render.ts:502,1621-1638`), with delegated listeners attached once in the constructor (`webapp/src/render.ts:1532,1563`). Its body is re-derived from the child feed each render (`webapp/src/render.ts:496-501`).
- Liveness indicators: `.bubble.pulsing` breathes only while a turn is in flight and never on an ended turn (`styles.css:393-398`, `webapp/src/render.ts:1078`); the discrete `.tool-spinner` arc runs `thinking-spin` regardless (`styles.css:923-948`, emitted `webapp/src/render.ts:531`).

### 4.2 Chosen pattern

- **ActivitySection fold**, not `installClickExpand`.
  - Open/closed state lives in a renderer-owned `Set<string>` keyed by the bubble's item key, exactly like `openPanels` — immune to `innerHTML` by construction, so a region that updates every tick regenerates correct HTML from the Set + store with no round trip.
  - The `.expanded` mechanism is rejected: it would require inventing a new capped class and leaning on the brittle capture/re-apply round trip, which a per-tick rewrite stresses hardest.
- **Embedding in the bubble:** `TextStream` (`webapp/src/render.ts:357-381`) emits the panel markup and threads the same `PanelContext` it already receives via `renderItem` (`webapp/src/render.ts:1711`), so the panel reconciles as part of the bubble's single keyed node.
- **Liveness signal:** the `.tool-spinner` arc on the panel header (`styles.css:923-948`), **not** `.bubble.pulsing` — a final-response bubble belongs to an *ended* turn, where bubble-breathe is defined never to run (`styles.css:393-394`), whereas the arc correctly signals a still-live watcher after the turn closed.

---

## 5. Realtime transport (investigation area 5)

### 5.1 The two options against the retention model

- The WS ring is a fixed **frame count** (default 4096, `daemon/cmd/claude-repld/main.go:41,146`, applied `daemon/internal/session/session.go:284-286`), truncated per frame (`daemon/internal/session/session.go:1091-1094`), replayed to every fresh join and gap-fill reconnect (`daemon/internal/session/session.go:696-725,1038-1050`; store side `webapp/src/store.ts:435-498`). There is no ephemeral/non-retained frame class.
- **Option A — poll a new HTTP endpoint only while expanded.** Cost is one round trip per poll to a `resolveForAttach`-based handler (the `handleCommands` template: pollable, never resurrects a session, serializes `[]` when nothing is resolved yet — `daemon/internal/server/server.go:545-563,1386`). It never touches the ring, never replays, never revives a hibernated session. Client plumbing already exists: base URL at `webapp/src/main.ts:59-62`, plain `fetch` with the session id in the path (`webapp/src/account.ts:36`, `webapp/src/ws.ts:148`), and the expand state that scopes polling (`webapp/src/expand.ts`).
- **Option B — push a new high-frequency WS frame.** Every such frame enters the ring and replays on reconnect; a 1s-cadence uncoalesced tail fills the entire 4096-window in ~68 minutes and thereafter evicts real conversation frames (`daemon/internal/session/session.go:1091-1094`). This is the exact failure the tailer's discipline was built to prevent (`daemon/internal/session/tailer.go:12-15`, caps at `:31-39`).

### 5.2 The nuance: some watchers already stream

- For backgrounded **Bash** tasks and **workflow journals**, `task-output-delta` already flows over WS and into `store.taskOutput` live, even after the turn ends, until completion (`daemon/internal/session/tailer.go:157-173`, reducer `webapp/src/store.ts:620-624`). For these, **no new transport is needed** — the panel renders existing store state.
- The gaps needing a poll are: background **agents** (no tailer, §2.4.2), a **frozen elapsed** clock (§2.4.3), and any tail deeper than the tailer's 64KB total cap (`daemon/internal/session/tailer.go:37-39,163-174`).

### 5.3 Chosen transport: hybrid

- **Primary:** render from existing store state (frames already flowing) — free, no change.
- **Fill gaps with Option A:** a poll endpoint invoked only while a bubble's panel is expanded, for background-agent tails, fresh elapsed, and deep tails.
- **Reject Option B** for transient progress — it structurally evicts conversation history from the replay ring.

---

## 6. Daemon touchpoints for the poll endpoint (investigation area 2)

Correction to the task premise: **there is no chess-game HTTP file route** and
no cwd-prefix file validator in the server (routes registered at
`daemon/internal/server/server.go:362-382`; the only file server is the static
webapp bundle `daemon/cmd/claude-repld/main.go:165`). The genuine
confinement precedent is the tailer's own predicates.

### 6.1 Reusable as-is

- `allowedTaskOutputPath(path)` — `filepath.Clean` + spool-shape match + must contain `/tasks/` + end `.output` (`daemon/internal/session/tailer.go:110-115`).
- `allowedJournalPath(path, configRoot)` — `filepath.Clean` + config-root prefix + workflow-journal shape (`daemon/internal/session/tailer.go:97-105`).
- `readTailChunk(path, offset)` — bounded, UTF-8-safe chunked reader (`daemon/internal/session/tailer.go:198-222`) under existing budgets (`daemon/internal/session/tailer.go:35,38`).
- Session resolution + two-segment path shape precedent (`{id}/{taskId}`): `r.PathValue("id")` + `resolveForAttach` (`daemon/internal/server/server.go:545,1386`), mirroring `/sessions/{id}/queue/{queueId}/...` (`daemon/internal/server/server.go:372-373`). `main.go` needs no change (`daemon/cmd/claude-repld/main.go:161`).

### 6.2 The one genuinely missing daemon piece

- A **durable, session-scoped `taskID → outputPath` map.** Today the path lives only inside the live tailer goroutine's closure (`daemon/internal/session/tailer.go:53-77,141-151`) and is discarded on completion (`daemon/internal/session/tailer.go:126-129`); `s.tailers` stores only stop channels (`daemon/internal/session/session.go:125-127`) and the registry `Record` has no task field (`daemon/internal/registry/registry.go:25-49`). A route keyed by `(sessionID, taskId)` has nothing to look up.
  - Fix: record the path on the Session when the tailer starts (`daemon/internal/session/tailer.go:121-133`) and keep it after completion (post-completion reads are already expected, `daemon/internal/session/tailer.go:178-182`).
- **Explicit session scoping.** `allowedTaskOutputPath` confines the spool *shape* but does not prove the `.output` belongs to the requesting session; the cleanest guard is to serve only paths from that session's recorded map rather than any client-supplied path.
- **Background-agent path recognition.** The spawn regex matches only `Output is being written to:` (`daemon/internal/session/tailer.go:44`); a background agent announces an `output_file:` path instead, so recognizing that at spawn (or reading `notification.OutputFile`, `daemon/internal/protocol/layer2.go:269-277`) is required to tail agents at all.

### 6.3 Proposed route

- `mux.HandleFunc("GET /sessions/{id}/tasks/{taskId}/output", …)` alongside `daemon/internal/server/server.go:362-382`: resolve the session via `resolveForAttach`, look up the recorded path (§6.2), re-validate with `allowedTaskOutputPath`/`allowedJournalPath`, and stream a bounded tail via `readTailChunk` under the existing budgets, returning `{ text, offset, elapsed, done }`.

---

## 7. End-to-end touchpoint summary

| Layer | Touchpoint | Kind |
|---|---|---|
| webapp | turn-slice helper + `watchersForBubble` projection near `finalResponses` (`webapp/src/render.ts:976-994`) reusing `spawnedTaskIds` (`webapp/src/partition.ts:44-48`) | new (projection) |
| webapp | in-bubble fold via `TextStream` (`webapp/src/render.ts:357-381`) + `openPanels`/`PanelContext` (`webapp/src/render.ts:1510,1644,1621-1638`) | new (render, reuses pattern) |
| webapp | `.tool-spinner` liveness on panel header (`styles.css:923-948`) | reuse |
| webapp | poll client scoped to expand state (`webapp/src/expand.ts`), base URL (`webapp/src/main.ts:59-62`), `fetch` (`webapp/src/ws.ts:148`) | new (small) |
| daemon | `GET /sessions/{id}/tasks/{taskId}/output` route (`daemon/internal/server/server.go:362-382`) | new route |
| daemon | durable session-scoped `taskID→path` map (`daemon/internal/session/tailer.go:121-133`, `session.go:125-127`) | new state |
| daemon | reuse `allowedTaskOutputPath`/`allowedJournalPath`/`readTailChunk` (`daemon/internal/session/tailer.go:97-115,198-222`) | reuse |
| daemon | background-agent path recognition (`daemon/internal/session/tailer.go:44`) | extend |
| protocol | none for Candidate A + Option A (only if Candidate C or Option B chosen) | none |

---

## 8. Recommendation

1. **Association:** Candidate A — a pure webapp projection linking the derived
   final-response bubble to the watcher ids spawned in its turn, reusing
   `partition.ts` + `finalResponses` and the existing `planAgentReveal`
   interaction. No protocol change.
2. **UX:** the ActivitySection fold rendered inside the bubble by `TextStream`
   with renderer-owned `openPanels` state, and a `.tool-spinner` liveness arc
   (never `.bubble.pulsing`, which cannot run on an ended turn).
3. **Transport:** hybrid — render from already-flowing store frames, and poll
   the new `GET …/tasks/{taskId}/output` endpoint (Option A) only while the
   panel is expanded, to cover background agents, fresh elapsed, and deep tails.
   Never a new high-frequency WS frame (Option B) — it evicts conversation
   history from the 4096-frame ring.
4. **Daemon:** the only genuinely new server work is a durable session-scoped
   `taskID→outputPath` map plus the pollable route; everything else (confinement
   predicates, bounded reader, session resolution) is reuse.

This keeps the feature almost entirely in the webapp, adds one small read-only
daemon route, and changes no wire protocol.

---

## 9. Open questions

1. **Scope of "talks about".** Should the panel list *every* watcher spawned in
   the bubble's turn (High reliability, `webapp/src/partition.ts:44-48`), or only
   ids the bubble text literally names (Medium reliability, wording-coupled regex,
   `webapp/src/tasks.ts:11-19`)? Recommendation: turn-scoped, with text-named ids
   sorted first.
2. **Tailing background agents.** Recognize the agent's `output_file:` path at
   spawn (extend `daemon/internal/session/tailer.go:44`), or only serve it after
   the completion notification exposes `OutputFile` (`daemon/internal/protocol/layer2.go:269-277`)?
   The former enables true live progress; the latter is simpler but not "live".
3. **Frozen elapsed.** Fix client-side by re-deriving elapsed from wall-clock, or
   server-side by computing elapsed from the `.output` file mtime in the poll
   response? (`progressElapsedS` freezes today, `webapp/src/store.ts:71`.)
4. **Structured progress vs raw tail.** Ship raw tail lines only (no wire field
   exists), or add a parsed percent/step model later? Raw lines match today's
   `liveTaskOutput` rendering (`webapp/src/render.ts:604-607`).
5. **Poll lifecycle.** Stop polling on completion notification, on collapse, and
   on workspace switch-away — confirm all three are the intended stop conditions.
6. **Surface scope.** Bubble-only, or also mirror watcher progress on the roster
   row (`webapp/src/counter-menu.ts:44-58`, which today has no progress field)?
