# Design: generalized async/streaming response types in the expanded bubble view

Status: **IMPLEMENTED.** The seam, the three renderers, and the fixes below
all shipped; `shared/protocol.md` §1.2 (`structured`) and §2.6
(`async-source`) are the live contract. This document is kept as the record
of WHY, and of what was deliberately left out (§1.4, §2.5).
Scope: `modules/app/agent-repl/` (`agent-shim/claude/shim/`, `daemon/`,
`webapp/`, `shared/protocol.md`).

## Correction (post-review)

**§1.4 originally listed `TaskCreate`/`TaskUpdate` among the types the
analogy fails for. That was wrong, and the review caught it.** A
`TaskUpdate` is a stream event addressed to the `TaskCreate` that opened the
task: it satisfies both conjuncts of the eligibility test — it streams
(discrete `statusChange` transitions arriving over time, two per task in
real transcripts) and it is card-scoped (correlated by `taskId` to one
spawning call). The error was reading "owns a stream" as "emits bytes",
when a stream of structured events qualifies equally.

`TaskCreate` therefore ships as an `AsyncSource` of kind `task`, whose fold
renders its transitions as rows. One caveat separates it from the
zero-plumbing cases: the SDK gives a `TaskCreate` **no** structured result
(`toolUseResult` is `null`), so its id is prose-only and
`taskIdFromCreateResult` stays load-bearing. Its source is the one derived
client-side (`taskSourceFor`); every other kind comes off the daemon's
`async-source` frame. Everything downstream of the id is structured, so the
prose coupling stops at that one line.

The types the analogy genuinely fails for are `Skill`, `auth_status`, and
`compact_boundary` — see §1.4.

All `path:line` anchors are relative to `modules/app/agent-repl/` unless absolute.

Grounding for the SDK surface: `@anthropic-ai/claude-agent-sdk@0.1.77`, read from the
resolved install at
`~/.cache/agent-repl/node-store/shim-d871d1a51d2fab56/node_modules/@anthropic-ai/claude-agent-sdk/`
(the `agent-shim/claude/shim/` directory has no local `node_modules`;
`agent-shim/claude/shim/package.json` declares `^0.1.0` and
`agent-shim/claude/shim/package-lock.json` pins `0.1.77`). Transcript evidence is from ~60 real
`~/.claude/projects/**/*.jsonl` files modified in the last 30 days.

---

## 0. The headline finding

**`SDKUserMessage.tool_use_result` is dropped by the shim and referenced nowhere in the
repository.** `grep -rl "tool_use_result\|toolUseResult" agent-shim/claude/shim/src daemon/internal webapp/src shared/`
returns nothing.

The SDK declares it at `coreTypes.d.ts:396-413`, and its own doc comment states the purpose
verbatim:

> If present, the JSON result of a tool use that this user message is responding to. **This is
> provided to make it easier for applications to present the tool result in a formatted way.**
> The model only receives the content within the user message. The specific format is
> tool-dependent.

That is precisely the field this project needs, and it is thrown away at
`agent-shim/claude/shim/src/session.ts:497-532` (`mapUserMessage`), which walks `msg.message.content` for
`tool_result` blocks and `<task-notification>` text and never looks at the `tool_use_result`
sibling on `msg` itself.

Nearly every fragility documented below is downstream of this one drop. The structured field
carries, per real transcripts:

| Tool | `tool_use_result` keys observed | What the GUI does instead today |
|---|---|---|
| `Bash` | `stdout`, `stderr`, `interrupted`, `isImage`, `noOutputExpected`, `returnCodeInterpretation` | `renderHint` dumps the flattened text into `RenderHint.Stdout` (`daemon/internal/session/render.go:70`); `Stderr` and `ExitCode` (`layer2.go:211-212`) have **no producer anywhere** — dead wire fields that `render.ts:906` renders |
| `Agent` (async spawn) | `isAsync`, `status:"async_launched"`, `agentId`, `outputFile`, `canReadOutputFile`, `description`, `prompt`, `resolvedModel` | `agentId`/`output_file` regex-scraped from English prose (`daemon/internal/session/tailer.go:53-54`, `replay.go:360`, `webapp/src/partition.ts:34`, `render.ts:696`) |
| `Agent` (completed) | `agentId`, `agentType`, `status`, `resolvedModel`, `content`, `prompt`, `usage`, `totalTokens`, `totalDurationMs`, `totalToolUseCount`, `toolStats{readCount,searchCount,bashCount,editFileCount,linesAdded,linesRemoved,otherToolCount}` | truncated to a 200-byte string by `taskSummaryLimit` (`render.go:16,103`) |
| `Edit` | `structuredPatch`, `filePath`, `oldString`, `newString`, `originalFile`, `replaceAll`, `userModified` | daemon recomputes a unified diff itself (`render.go:77-83`, `unifiedDiff` at `:209-235`) |
| `TaskCreate`/`TaskUpdate` | `taskId`, `statusChange`, `updatedFields`, `success` | `taskIdFromCreateResult` regex-scrapes `#(\S+)` from prose (`webapp/src/tasks.ts:79-82`), with a comment admitting the coupling at `tasks.ts:13-18` |
| `Skill` | `commandName`, `success`, `allowedTools` | daemon reads `SKILL.md` off disk instead (`render.go:104-112`), which fails closed for plugin skills (`:125-127`) |
| `Read` | `file`, `type` | flattened text |

So: **the data is already flowing and is discarded at the shim.** The types that look
unsupportable are not.

---

## Part 1 — Inventory

### 1.1 The Agent reference case, end to end

This is the shape to generalize. Traced hop by hop:

1. **SDK.** `realQueryOptions` sets `includePartialMessages: true` (`agent-shim/claude/shim/src/main.ts:128`). Every
   SDK message from inside a subagent carries `parent_tool_use_id = <spawning Agent call's
   tool_use_id>`; main-loop messages carry `null`. Present on exactly four SDK types:
   `SDKAssistantMessage`, `SDKUserMessage(Replay)`, `SDKPartialAssistantMessage`,
   `SDKToolProgressMessage` (`coreTypes.d.ts:414-544`). `result`, all `system` subtypes, and
   `auth_status` lack it — they are session-global.
2. **Shim.** `mapSdkMessage` (`session.ts:429-487`) forwards `stream_event` →
   `stream-event` and `assistant` → `assistant-message`, each with `...parentField(msg)`
   (`session.ts:613-617`). `tool_progress` → `system{subtype:"tool_use_progress"}`
   (`session.ts:473-481`).
3. **Daemon.** `onStreamEvent` (`translate.go:427-560`) keys open blocks by
   `"<parent_tool_use_id>/<index>"` (`translate.go:23`) and stamps `ParentToolUseID` onto
   `text-start` (`:474`), `thinking-start` (`:483`), `tool-use-start` (`:495`). `onSystem`
   projects `tool_use_progress` → `tool-use-progress` carrying `parent_tool_use_id`
   (`:899`).
4. **Store.** `parentToolUseId` lands on `TextItem` / `ThinkingItem` / `ToolItem`
   (`store.ts:39,52,65`). The store stays one flat `items` array (`store.ts:219`).
5. **Partition.** `partitionFeed` (`partition.ts:84-108`) splits that flat list into `top` plus
   `children: Map<parentToolUseId, ConversationItem[]>`. Orphans stay top-level (`:100-106`) —
   "pollution is recoverable, silence is not" (`:12-14`).
6. **Render.** `ToolCard` (`render.ts:644-693`) pulls `panels.children.get(item.toolUseId)` and
   hands it to `ActivitySection` (`:621-637`), which renders each child through **the same
   `renderItem` the top level uses** (`:633`) inside a `Fold` (`:605-619`). Open state lives in
   the renderer's `openPanels: Set<string>` (`render.ts:1807`), read via `PanelContext.isOpen`
   (`render.ts:484`).

**The load-bearing observation for Part 2:** steps 4–6 are *already fully generic*.
`partition.ts` is parent-keyed, not Agent-keyed — its own header says so: "The mechanism is
parent-generic — an Agent's children and a Workflow's children nest identically"
(`partition.ts:10-11`). `Fold`/`ActivitySection`/`renderItem` know nothing about Agents.

What is bespoke about the Agent case is **not the nesting**. It is:

- **Correlation** — learning that a card spawned detached work, and where that work's stream
  lives. Done by regex over English prose in four separate places
  (`tailer.go:43-54`, `replay.go:360`, `partition.ts:34`, `render.ts:696`).
- **Naming** — `SUBAGENT_TOOLS = {"Task","Agent"}` (`agents.ts:29`) and `SPECIAL_TOOLS`
  (`render.ts:120-128`) hard-code tool names.
- **Per-type render** — `renderHint`'s tool-name switch (`render.go:67-115`), `agentComposer`
  (`render.ts:710-726`), `taskControls` (`render.ts:749-761`).

So nesting generalizes for free; correlation does not. **Correlation is the seam.**

### 1.2 Every async/streaming-shaped type the SDK can produce

The SDK union is closed and short (`coreTypes.d.ts:553`) — 11 members, 7 discriminants:

```
SDKMessage = SDKAssistantMessage | SDKUserMessage | SDKUserMessageReplay | SDKResultMessage
           | SDKSystemMessage | SDKPartialAssistantMessage | SDKCompactBoundaryMessage
           | SDKStatusMessage | SDKHookResponseMessage | SDKToolProgressMessage
           | SDKAuthStatusMessage
```

`system` has **exactly four** subtypes: `init`, `compact_boundary`, `status`, `hook_response`
(verified by grepping `subtype: '` across the typings). Verdicts:

| # | Type / subtype | Data available | Reaches browser? | Verdict |
|---|---|---|---|---|
| 1 | **Agent / Task subagent** (`parent_tool_use_id`) | full nested stream + structured `tool_use_result` | stream ✅, structured result ❌ | **Handled** (nesting) / **partial** (result truncated to 200 B) |
| 2 | **Backgrounded Bash** (`BashInput.run_in_background`) | `agentId`/spool path in prose; daemon tails file | ✅ via `task-output-delta` | **Handled**, but discovery is prose-regex |
| 3 | **Background Agent** (`AgentInput.run_in_background`) | `isAsync`, `outputFile`, `canReadOutputFile` structurally; output file is the **full subagent JSONL transcript** | tail only via poll route, rendered as opaque `<pre>` (`render.ts:734-737`, `798`) | **Partial — the biggest miss.** See §1.3 |
| 4 | **Workflow** | `Task ID:` + `Transcript dir:` → `journal.jsonl` (structured JSONL) | ✅ streamed, rendered as opaque `<pre>` | **Partial** — structured stream flattened to text |
| 5 | **Skill** | `commandName`, `success`, `allowedTools` | `SKILL.md` body only | **Partial**, and see §1.4 — analogy does *not* hold |
| 6 | **TaskCreate / TaskUpdate** | `taskId`, `statusChange`, `updatedFields` | prose-regex only | **Partial**; `TaskUpdate` card suppressed (`render.ts:135`) |
| 7 | **`tool_progress`** | `tool_use_id`, `tool_name`, `parent_tool_use_id`, `elapsed_time_seconds` — **and nothing else** | ✅ | **Handled.** No percent/step field exists to plumb. `ToolUseProgressFrame.Text` is *synthesized by the daemon*, not from the SDK |
| 8 | **`hook_response`** | `hook_name`, `hook_event`, `stdout`, `stderr`, `exit_code` | ❌ **dropped at shim** (`session.ts:583`) | **Dropped.** Async, has a stream, currently invisible |
| 9 | **`status`** | `status: 'compacting' \| null` | only `compacting` (`session.ts:584`) | **Handled** (the union has no other value) |
| 10 | **`auth_status`** | `isAuthenticating`, `output: string[]`, `error` | ❌ dropped (`session.ts:482-485`) | **Dropped.** Streaming-shaped (`output` grows), but see §1.4 |
| 11 | **`compact_boundary`** | `trigger`, `pre_tokens` | ✅ | **Handled** |
| 12 | **`result.structured_output`** | arbitrary JSON (`coreTypes.d.ts:455`) | ❌ never read (`session.ts:566-568` takes only `result` when `subtype==="success"`) | **Dropped** |
| 13 | **`result.modelUsage` / `errors[]`** | per-model cost breakdown; `errors: string[]` on error arms | ❌ dropped | **Dropped** |
| 14 | **`SDKUserMessageReplay`** | `isReplay: true` | shares `type:"user"` with real user messages | **Latent bug** — see §1.5 |
| 15 | **`SDKAssistantMessage.error`** | `'rate_limit' \| 'billing_error' \| …` | ❌ dropped (`session.ts:440-462`) | **Dropped** |

Beyond the SDK stream, the **daemon's replay path reads CLI transcripts directly**
(`replay.go`), where real files carry system subtypes the SDK typings do not list at all:

| Transcript subtype | Shape (observed) | Relevance |
|---|---|---|
| `turn_duration` | `durationMs`, `messageCount`, **`pendingBackgroundAgentCount`** | A live count of outstanding background agents — exactly the roster figure `watchers.ts` derives by hand |
| `agents_killed` | (marker) | Terminal state for background agents; nothing surfaces it |
| `model_refusal_fallback` | `direction`, `content`, `level`, `trigger` | Fable-5 refusal → Opus fallback, unsurfaced |
| `local_command` | `<command-name>/status</command-name>` … | **The real slash-command subtype** |
| `stop_hook_summary` | — | Hook lifecycle |

### 1.3 The single largest render-time drop

`tailer.go:126-128` states, correctly, that a background agent's output file is "its full JSONL
transcript, far too verbose to push into the retention ring" — so it is recorded but never
tailed. The poll route (`server.go:731-762`) serves it as raw bytes, `WatcherPoller` accumulates
them (`watcher-poll.ts:108-113`), and `WatcherRow` renders the result as
`<pre class="tool-output task-live-output">` (`render.ts:734-737`, `798`).

**That file is the same JSONL the daemon's own `replay.go` already parses into conversation
items.** A background agent's stream is structurally identical to an inline Agent's — the browser
is handed it and renders it as an opaque text blob. Same for a Workflow's `journal.jsonl`
(`tailer.go:45-47`), which is streamed live over `task-output-delta` and also `<pre>`'d.

This is the clearest case of "the analogy holds and the data is already there": a detached agent
*is* the Agent case, differing only in that its stream arrives as a file rather than as
`parent_tool_use_id`-tagged frames.

### 1.4 Where the analogy does NOT hold

Legitimate findings — these should **not** get nested-bubble treatment:

- **`Skill`.** A skill is not a subagent. It injects a prompt into the *main* loop, so its work
  appears as ordinary main-chain tool calls with `parent_tool_use_id = null`. There are no
  children to nest, and forcing a fold would either be empty or would have to lie about
  provenance by re-parenting main-chain items. Skill's correct treatment is what it has (an
  invocation line plus the `SKILL.md` body), improved only by using `commandName`/`success`
  instead of disk I/O. *Exception:* a skill the harness runs in a subagent does get a real
  `parent_tool_use_id` and would nest through the generic path for free — no Skill-specific code.
- **`auth_status`.** Streaming-shaped (`output: string[]` grows) but session-global: it has no
  `parent_tool_use_id`, belongs to no card, and concerns a modal login flow that `login.ts` /
  `login-terminal.ts` already own. It wants a banner, not a bubble.
- **`status` / `compact_boundary`.** Session-global; the existing indeterminate banner
  (`render.ts:226-234`) is right. `SDKStatus` is `'compacting' | null` — there is no richer
  state to render.
- ~~**`TaskCreate` / `TaskUpdate`.**~~ **Retracted — see the Correction at the top.** A
  `TaskUpdate` IS a stream event addressed to its `TaskCreate`'s card, and now folds in as one.
  The roster in `tasks.ts` stays regardless: it is a session-wide index, and the fold is
  per-card detail, exactly as the agent roster coexists with the Agent card's own fold.
- **`tool_progress`.** Already handled; not a bubble, a heartbeat on an existing card.

### 1.5 Correctness defects found along the way

Surfaced during the inventory; each is a real defect, listed here rather than fixed:

1. **`type: 'user'` is not a safe discriminant.** `SDKUserMessage` and `SDKUserMessageReplay`
   share it (`coreTypes.d.ts:414-426`); only `isReplay: true` separates them. `mapUserMessage`
   (`session.ts:497`) switches on `type` alone, so a replayed user message is processed as a real
   one — a re-emitted `tool_result` block would emit a duplicate `tool-use-result`.
2. **`tool-use-result` drops `parent_tool_use_id`.** `layer1.go:102` declares it, `replay.go:419`
   deliberately populates it with an explanatory comment, `translate.go:714-725` discards it, and
   `layer2.go:235-241` has nowhere to put it. Three layers agree it matters; the fourth loses it.
3. **`RenderHint.Stderr` / `.ExitCode` have no producer** (`layer2.go:211-212` vs
   `render.go:70`). `render.ts:906` renders `r.stderr`, which is always empty. The real
   stdout/stderr split is in `tool_use_result`.
4. **The shim's `slash_command` branch looks dead.** `session.ts:583` forwards
   `subtype === "slash_command"`; the SDK has no such subtype, and real transcripts show
   `local_command`. `translate.go:819-824` and `webapp/src/protocol.ts:361` carry it through.
   *(Unverified whether an older CLI emitted it — needs a git-history check before removal.)*
5. **Silent unmarshal failures.** `compact_boundary` (`translate.go:833`) and `tool_use_progress`
   (`:889`) use `_ = json.Unmarshal` and still emit a **zero-valued frame** on failure.
6. **Prose-regex is a silent single point of failure.** `parseSpawnAnnouncement` returns `nil`
   indistinguishably for "not a spawn" and "wording changed" (`tailer.go:70-98`). A CLI wording
   change disables tailing, the poll route, and every watcher fold — with no error.

---

## Part 2 — Design

### 2.1 The seam

> **Name it: the `AsyncSource` descriptor — a structured, per-tool-call declaration of
> "this call owns a stream", derived once at the shim from `tool_use_result`, carried as one
> Layer-2 frame, and rendered by one generic fold.**

Today "does this call own a stream?" is answered independently and differently at four layers,
each by regex over English. The proposal is to answer it **once, structurally, at the earliest
point the data exists**, and let every layer below consume the answer.

```
SDK  tool_use_result (structured, per tool)          coreTypes.d.ts:396-413
  │
  ├─ shim: mapUserMessage forwards it verbatim  ──►  NEW L1 `tool-result.structured`
  │
  ├─ daemon: classify(toolName, structured)     ──►  NEW L2 `async-source` frame
  │            { sourceId, kind, streamRef, label, status }
  │
  ├─ store:  ToolItem.asyncSource               ──►  derive-don't-track, as today
  │
  └─ webapp: ONE AsyncFold, keyed by sourceId   ──►  renderItem for parsed streams,
                                                     <pre> only as the fallback
```

The descriptor is a small closed record:

```ts
interface AsyncSource {
  sourceId: string;            // agentId | bg shell id | workflow task id
  kind: "agent" | "shell" | "workflow";
  streamRef?: { transport: "ws" | "poll"; format: "jsonl-transcript" | "jsonl-journal" | "text" };
  label: string;               // description / command / summary
  status: "running" | "done" | "error" | "killed";
}
```

Three things make this a seam rather than another special case:

1. **One producer.** `tool_use_result` is the only input. Adding a newly supported async type
   means adding one arm to the daemon's `classify` — no new frame, no new store field, no new
   render path, no new regex.
2. **`format` selects a parser, not a component.** `jsonl-transcript` and `jsonl-journal` decode
   into `ConversationItem[]` and go through the **same `renderItem`** the inline Agent's children
   already use (`render.ts:633`). `text` keeps today's `<pre>`. A background agent's fold then
   renders nested bubbles because it *is* the Agent case, not because of Agent-specific code.
3. **Nesting is untouched.** `partition.ts` already keys on parent id. The descriptor supplies
   the *correlation* that regexes supply today; it does not touch the *rendering* of children.

`SUBAGENT_TOOLS`, `SPAWNED_ID_RE`, `AGENT_SPAWN_RE`, `taskIdFromCreateResult`,
`taskSpawnIDRe`/`taskSpawnPathRe`/`agentSpawnIDRe`/`agentSpawnPathRe`, and the `replay.go`
`agentIDRe` all collapse into consumers of `AsyncSource`. `WatcherPanel`/`WatcherRow`
(`render.ts:777-831`) and `ActivitySection` (`:621-637`) converge on one `AsyncFold` — they are
already 90% the same code, both built on `Fold` (`:605-619`).

### 2.2 Retention: why this does not evict the ring

The existing constraint stands (`design-watcher-bubble-expansion.md` §5.1): the WS replay ring is
a fixed 4096 frames, so no high-frequency stream may enter it. The `async-source` frame is
**one frame per spawn** — it is a descriptor, not a stream. Bulk content keeps today's split:
`task-output-delta` for shells (coalesced + 64 KB-budgeted, `tailer.go:33-38`), and the poll
route for agent transcripts (`server.go:731`, only while a fold is open, `render.ts:1782-1794`).
Parsing moves client-side; transport does not change.

### 2.3 Per-type plumbing

| Type | Data flowing? | New plumbing needed |
|---|---|---|
| Inline Agent | ✅ fully | none for nesting. Replace the 200-byte `task` hint with structured `tool_use_result` (usage, `toolStats`, `totalDurationMs`) |
| **Background Agent** | ✅ **already at the browser, as `<pre>`** | **none on the wire.** Parse the polled JSONL into `ConversationItem[]` and render via `renderItem`. Pure render-layer change |
| **Workflow** | ✅ **already streaming, as `<pre>`** | **none on the wire.** Parse `journal.jsonl` lines into fold rows |
| Backgrounded Bash | ✅ | replace prose-regex discovery with `tool_use_result`; populate `RenderHint.Stderr`/`ExitCode` (already on the wire, no producer) |
| `hook_response` | ❌ dropped at shim | shim forward + L2 frame + fold row. Genuinely new |
| Skill | partial | use `commandName`/`success`; **no fold** (§1.4) |
| TaskCreate/Update | partial | structured `taskId`; roster only, **no fold** (§1.4) |
| `structured_output`, `modelUsage`, `errors[]`, `assistant.error` | ❌ dropped | shim forward; result-chip surface, **no fold** |

Note the shape of that table: **the two types most obviously "unsupported" — background agents
and workflows — need zero new plumbing.** Their data is already in the browser. They are dropped
at render time, exactly as the task premise suspected.

### 2.4 Suggested sequencing

1. Shim: forward `tool_use_result` (unblocks everything; ~10 lines). Fix the `isReplay`
   discriminant (§1.5.1) in the same commit — same function, same bug class.
2. Daemon: `classify` → `async-source` frame; keep the regexes as a fallback for one release so
   an older CLI still works.
3. Webapp: `ToolItem.asyncSource`; converge `WatcherPanel` + `ActivitySection` → `AsyncFold`.
4. Webapp: JSONL parsers for `jsonl-transcript` / `jsonl-journal` → `renderItem`. **This is the
   payoff commit** — nested bubbles for background agents and workflows.
5. Retire the regexes; drop `RenderHint.task`'s truncation.
6. Separately: `hook_response`, `structured_output`, `modelUsage`.

Steps 1–4 are each independently shippable and independently valuable.

### 2.6 What shipped, and what did not

Shipped, in order:

1. `agent-shim/claude/shim/src/session.ts` forwards `tool_use_result` as §1.2 `structured`, and skips replayed
   user messages (`isReplay`) — §1.5.1.
2. `daemon/internal/session/asyncsource.go` classifies it into the §2.6 `async-source` frame.
3. `webapp/src/store.ts` carries `ToolItem.asyncSource`.
4. `webapp/src/async-stream.ts` parses each `format`; `render.ts` folds it through one
   `AsyncFold`. **Background agents render as nested bubbles.**
5. `RenderHint.Stderr` gained a producer (§1.5.3); the 200-byte agent truncation is gone.

Consolidations taken: the three near-identical fold rule sets in `styles.css` (the two tickers
were byte-identical) share one selector list; `WatcherRow`'s hand-rolled tail `<pre>` calls the
shared format-driven renderer; `stringField` moved out of `agents.ts` and `tasks.ts` into the
store.

**Deliberately not done:**

- **`resultText` in `partition.ts` vs `tasks.ts`** looks duplicated but joins blocks differently
  (`"\n"` vs `""`). Merging it would be a behavior change wearing an extraction's clothes.
- **The prose regexes stay** as the fallback for a shim predating `structured` (§2.5 Q1's
  recommendation: one release, then delete).
- **`hook_response`, `structured_output`, `modelUsage`, `assistant.error`** (§1.2 rows 8, 12,
  13, 15) are still dropped at the shim. Each is a separate, independently shippable change.
- **`slash_command`** (§1.5.4) is untouched pending the git-history check.
- **`gofmt` drift** in `internal/login/login.go` and `internal/session/session.go` predates this
  branch (verified at the merge base with this work stashed) and nothing in the repo enforces
  `gofmt`. The drift is godoc heading style and struct alignment in files this work never
  touched, so reformatting them would be unsolicited churn on someone else's chosen prose.

### 2.5 Open questions (for sign-off)

1. **Fallback policy.** Keep the prose-regexes permanently as a belt-and-braces path for a CLI
   that predates `tool_use_result`, or delete them once the descriptor lands? Deleting is
   cleaner; keeping hedges an SDK downgrade. Recommendation: keep for one release, then delete.
2. **Transcript-subtype surface.** `turn_duration.pendingBackgroundAgentCount`, `agents_killed`,
   and `model_refusal_fallback` are real and unsurfaced, but they are **transcript-only** — not in
   the SDK union, so only the replay path sees them. Surface them (a second, transcript-shaped
   source of truth) or leave them? Recommendation: `agents_killed` yes (it is a terminal state a
   fold would otherwise show as forever-running); the others no.
3. **Parse depth for a background agent's transcript.** Full `ConversationItem[]` fidelity
   (reusing `replay.go`'s parsing rules client-side, risking drift), or a reduced row model
   (text + tool names only)? Recommendation: reduced first, full later.
4. **`slash_command` removal** (§1.5.4) — needs a git-history check to confirm no CLI ever emitted
   it before deleting three layers of handling.
5. **Where does structured Agent usage render?** `toolStats`/`totalTokens` on the card, in the
   topbar roster row (`counter-menu.ts` has no such field today), or both?
