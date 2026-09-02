# Shim teamlead fanout plan (binding for every shim implementation subagent)

This document is the shim teamlead's lead-level architecture for the shim
overhaul. It is an ARCHITECTURE directive per TEAMLEAD.md: follow it by
default; refine it in its own style; never dissolve a module or muddy an
ownership line. Where it prescribes a mechanism (a file name, a key format, a
retry shape) that is an implementation detail you may override with a note in
your report. The protos under `proto/src/` are the authority over everything
here; `docs/overhaul/shim.md` carries the contract context; COMMON rulings
R1–R14 apply.

## Module layout (src/), ownership, and seams

```
src/
  main.ts                 entrypoint: argv, env, locks, log-fd, signals, --version, wiring
  build-identity.ts       SHIM_BUILD_SHA + sdk/binary versions (SessionRuntime)
  log.ts                  (moved from uds/log.ts) the one canonical JSONL logging API
  locks.ts                (moved from uds/session-lock.ts) the two kernel flocks
  vendor-guard.ts         the ONLY dynamic import of the SDK; FORBID_VENDOR_CALLS gate
  metaprompt.ts           canonical metaprompt append (unchanged mechanism)
  sdk/
    types.ts              structural SDK boundary types (QueryLike, SdkMessage, CanUseTool …)
                          derived from sdk.d.ts; the upgrade canary asserts these exist
    real-query.ts         real query() factory: preset system prompt + metaprompt append,
                          settingSources user+project+local, includePartialMessages,
                          forwardSubagentText, sessionId pre-mint, resume, canUseTool
  service/
    server.ts             UDS listener (h2c + HTTP/1.1 on one socket), Connect router
    routes.ts             the shim.v1 service implementation: 17 handlers (ReadHistory included), one function each
    validate/*.ts         request validation: one base function per request message, one
                          per non-primitive field (the proto→code mapping convention)
  engine/
    session.ts            THE session engine: owns the one query, StartSession fresh/resume,
                          SetSessionModel (turn boundary), SetSessionPermissionMode, Hibernate,
                          KillSession, one-turn-in-flight, pending-callback liveness, SIGTERM path
    turn.ts               StartTurn/KillTurn/UpdateAgent delivery; spawn provenance map
    identity.ts           main AgentId minting + persistence (R9); vendor id rotation handling
    cold.ts               transcript reader: cold-gate detection, model + mode recovery
    keepalive.ts          keep-alive cadence + the yield obligation (rewind before a real prompt)
    compaction.ts         the throwaway summarizing session that rewrites the transcript
    backup.ts             bounded transcript backup at turn end + rotation
    detached.ts           task_started / background_tasks_changed / task_notification → the
                          live set; DetachedWorkId ↔ underlying identity; WatchBash/StopBash
    pushes.ts             WatchSession fan-out; diagnostics + context_usage cadence
    permission-gate.ts    canUseTool → AgentPermission | AgentQuestion; echo validation;
                          free-text residue rule; standing token round-trip
  convert/
    fold.ts               SDK message → conversation.v1 frames dispatcher (the fold)
    ids.ts                AgentActivityId / AgentQuestionId / AgentPermissionId minting
    stream-events.ts      message_start/content_block_*/message_delta → thinking/response units,
                          usage on the first-block unit, effort
    tool-calls.ts         tool_use block → the unit's start arm; tool_result → terminal arm;
                          dispatches to tools/<kind>.ts by tool name; exempt set; unmodeled
    tools/<kind>.ts       one file per modeled tool kind (read, write, edit, grep, glob, bash,
                          subagent, skill_use, send_message, task_act, web_fetch, web_search,
                          monitor, schedule_wakeup, artifact, plan_mode, report_findings,
                          worktree, cron, push_notification, hook, context_injected, unmodeled)
    session-updates.ts    system:* / rate_limit / status / fast_mode / mcp / account usage →
                          SessionUpdate arms
    terminals.ts          result → AgentSuccess/AgentFailure (the 16-arm taxonomy), api errors
    residue.ts            StoreUnservedItem { vendor_specific | unknown | unparsed }
  store/
    client.ts             store.v1 Connect client over the store UDS
    keys.ts               upsert_key + write_id minting (THE one place; base function per identity)
    writer.ts             WriteBatch with the bounded in-memory retry buffer
    reader.ts             OpenAgentSession/WatchAgentSession/ReadAgentPage → HistoryPage/EntryAt
    reconcile.ts          GetLiveWork at session start: re-adopt or close
  fake/
    index.ts              createFakeQuery(): the scenario engine behind --fake
    scenario.ts           the Scenario interface + ScenarioContext (emit, files, canUseTool, gate)
    registry.ts           prompt text → scenario selection (the AGENTS.md table's source)
    vendor-files.ts       writes vendor-shaped files on disk (transcript, subagents, spools)
    scenarios/*.ts        one file per scenario family
  capture/
    (scripts live in scripts/capture/; see below)
test/
  unit tests: one test file per src module (test/<path>.test.ts mirrors src/<path>.ts)
  integration/: the integration suite (the integration-tests agent's; the lead runs it)
scripts/capture/
  capture.mjs, prompts.json, README.md — the capture harness; NOT run this wave
```

Delete: `src/uds/framing.ts` (both halves; the Connect transport replaces it),
`src/protocol.ts` (the dead NDJSON protocol), `src/session.ts` (the dead
protocol engine; harvest what is still true into engine/ and sdk/types.ts),
`src/input-queue.ts` only if nothing uses it, `runUdsMode`, every legacy flag.
Move `src/uds/log.ts` → `src/log.ts`, `src/uds/session-lock.ts` →
`src/locks.ts`, `src/uds/proto.ts` → `src/proto.ts` (re-exports the generated
TS for shim/v1, store/v1, conversation/v1). Tests move with their modules.

## Process shell (main.ts) — the spawn contract

- argv: `--listen <uds>` `--store-socket <uds>` `--log-fd 3` `[--fake]`
  `[--version]`. Nothing else. cwd is the workspace directory.
- env read: `CLAUDE_CONFIG_DIR` (the account root; required),
  `AGENT_REPL_OWNED=1` (required; refuse otherwise), `AGENT_REPL_STATE_DIR`
  (default `~/.claude-emacs`), `SHIM_BUILD_SHA` (required; reported on
  SessionStarted), `AGENT_REPL_FORBID_VENDOR_CALLS` (the guard),
  `AGENT_REPL_FAKE_TURN_GATE` / `AGENT_REPL_FAKE_TURN_GATE_TEXT` (fake only),
  `AGENT_REPL_FAKE_SPOOL_ROOT` (fake only; default `/tmp/claude-<uid>`),
  `AGENT_REPL_STORE_SOCKET` (the store socket when `--store-socket` is
  absent; the flag beats the env).
- Startup order: parse argv → configure log on fd 3 → take the WORKSPACE
  lock (keyed by cwd) → bind the UDS → serve. The SESSION lock is taken inside
  StartSession (fresh: keyed by the pre-minted vendor session id; resume:
  keyed by the resume id), before the SDK is touched, and held for the
  process lifetime. Lock files live in `$AGENT_REPL_LOCK_DIR` (default
  `~/.cache/agent-repl/run/`; the override exists so tests run private
  locks) as `workspace-<md5-8>.lock` and `session-<vendor-session-id>.lock`,
  the convention the daemon probes.
- SIGTERM = graceful stand-down (same path as KillSession{force:true} then
  wait for all store acks, then exit 0); SIGINT refused and logged at ERROR.
- Log-fd survival: EPIPE/EBADF on fd 3 is surfaced once (a SessionFault +
  degraded window) and never kills the process.
- `--version` prints and exits before any socket, lock or SDK import.

## Transport

- Add `@connectrpc/connect@^2` and `@connectrpc/connect-node@^2` (the
  generated `*_pb.ts` are protoc-gen-es v2; `service_pb.ts` carries the
  `GenService` the Connect router consumes; no `_connect.ts` files exist or
  are needed). Pin exact versions in the lockfile.
- Server: `http2.createServer({ allowHTTP1: true })` listening on the
  `--listen` UDS path (unlink a stale socket first; refuse a live one).
  Both codecs (binary + JSON) come free with the Connect router.
- Store client: `createConnectTransport({ httpVersion: "1.1", baseUrl:
  "http://store", nodeOptions: { socketPath } })` — server streaming works
  over HTTP/1.1; h2c is acceptable if the implementer verifies UDS support.
- The three workflow verbs answer `ConnectError(Code.Unimplemented)` — the
  typed not-implemented failure (WORKFLOW IS KICKED).

## Identities (the four spaces; never interchange)

- Main agent `AgentId.value` = the ORIGINAL vendor session id (R9 default;
  the shim pre-mints it with `Options.sessionId` on a fresh start, persists
  it under `$AGENT_REPL_STATE_DIR/shim/<workspace-key>/agent-id`, and
  reports it unchanged on every later StartSession). Subagent `AgentId` =
  the vendor's `agent_id`. `identity.ts` is the one place; the engine agent
  settles the rotation/fork/resume rule empirically and writes it into
  shim.md's mock section.
- `AgentActivityId` = the vendor `tool_use_id` for a tool call; for text and
  thinking blocks `<message.id>:<block_index>` (0-based).
- `AgentQuestionId` = the AskUserQuestion call's `tool_use_id` verbatim;
  `AgentPermissionId` = the gated call's `tool_use_id` verbatim (consent
  joins to the work it gates; the two never collide because a question is
  never a permission's gated call).
- `DetachedWorkId` = the vendor `task_id`, verbatim (the mapping to the
  underlying identity is one lookup from `task_started`).
- `TurnId` is daemon-minted and adopted; the shim never mints one.
- `HistoryPointer.value` is the store's `StoreItemPointer.value` passed
  through verbatim (opaque in both directions).

## Store keys (store/keys.ts — the one place; the CROSS-PLANE rule agreed with the store lead)

- `upsert_key`: `activity:<AgentActivityId.value>` (every frame of a unit;
  the id is the tool_use_id for a tool call, `<message.id>:<block_index>`
  0-based for text/thinking blocks) · `prompt:<TurnId.value>` (AgentPrompt) ·
  `question:<AgentQuestionId.value>` (= the AskUserQuestion tool_use_id) ·
  `permission:<AgentPermissionId.value>` · `terminal:<AgentId.value>:<vendor
  record uuid>` (an agent's success/failure frame; the uuid is the SDK
  message's) · `bash:<run AgentActivityId.value>` (a detached shell run's
  lifecycle rows) · `session:<arm>:<vendor record uuid>` (SessionUpdate rows;
  the arm is the oneof field name).
- `write_id`: `sha256("<producer>|<source coordinates>|<discriminator>")`
  hex — deterministic; source coordinates are the SDK message uuid (plus the
  block index for a block-derived unit), the discriminator is the frame's
  arm path. The same frame re-sent mints the same id and the store absorbs it.
- `producer`: `claude-shim:<original vendor session id>`.
- Shim-synthesized session facts (diagnostics, context_usage pushes) are
  NEVER written to the store — they are not vendor conversation.
- Routing: `update` → page line; `success`/`failure` → page line (the store
  dual-writes the terminal columns); `detached_work` → the lifecycle arm
  (`bash` for a shell; workflow never this wave); keep-alive turns' prompt +
  frames → `unserved_item.keepalive`.
- R15: the shim's `AgentPrompt` row is the ONE served prompt row (the
  sidecar classifies transcript user records as unserved). StartTurn writes
  it and has its durable ack BEFORE the turn's first activity frame is
  written.

## The fold (convert/) — rules every converter obeys

- Units upsert by identity; every frame of a unit is self-describing.
- `update` frames are deltas (never cumulative); terminals carry wholes.
- Usage + effort ride the FIRST content block's unit of each API response
  and no other.
- Progress beats (`tool_progress`) relay as `AgentToolCallProgress` on the
  nine kinds that carry the arm; `elapsed_time_seconds` is consumed.
- The exempt set is dropped silently (never `AgentUnmodeled`): TaskStop,
  TaskOutput, TaskGet, TaskList, ToolSearch, NotebookEdit, REPL, the
  MCP-resource family (ListMcpResources, ReadMcpResource), SendFeedback,
  ClaudeDesign, Projects, ShowOnboardingRolePicker, ProposeSkills, the
  background-shell peek. One carve-out: a TaskStop RESULT is consumed as
  the owning task's stop before the call is dropped.
- `AgentUnmodeled` only for a genuinely unknown tool (an MCP tool, a name
  no converter owns); `mcp_server` is stated from the vendor's own field,
  never parsed out of the name.
- Skill units settle on the DOCUMENT (the isMeta user record joined by
  `sourceToolUseID`), never on the acknowledgement.
- IDE diagnostics join the last write/edit unit by adjacency (one
  remembered value).
- Synthesized notices: the vendor's marker fields decide authorship; the
  arm is `synthesized_notice`, never `from_model`, and absence means
  unevaluated.
- Turn terminals come from `result` only; hook activity never synthesizes
  one. The 16 failure arms map from the result subtypes + error strings.
- Anything unconvertible lands in `residue.ts` (vendor_specific | unknown |
  unparsed) as an unserved store row — never dropped, never a crash. The
  `vendor_specific.kind` spelling is shared with the sidecar: an attachment
  record is `attachment/<type>` (e.g. `attachment/deferred_tools_delta`,
  `attachment/agent_listing_delta`); a system record is `system/<subtype>`;
  any other transcript/stream record kind is `<type>` verbatim.
- Every converter logs its branch through `log.ts`.

## Streams the engine serves

- WatchSession: standing; the first push after StartSession is
  `diagnostics{healthy}` (the daemon's readiness signal); `context_usage`
  pushed at start, at every turn end, and on a slow cadence; every arm on
  change only.
- WatchAgent: opening page (OpenAgentSession with page_size/known_through
  mapped 1:1) then the store tail, each entry with its pointer. The shim
  serves history FROM THE STORE, never from memory.
- WatchBash: `start` (original instant) then `update` deltas from the
  sidecar-fed store rows, then the terminal. StopBash → `query.stopTask`.
- Every bounded stream ends with its terminal frame; standing streams never
  conclude on their own.

## Validation and errors

- Every request message has one base validate function; unset non-optional
  fields and unset oneofs answer a `ConnectError(Code.InvalidArgument)` at
  once; a stream push from the SDK/store that is missing a required field
  raises loudly (a SessionFault) rather than forwarding.
- Failure arms marked "DERIVED at the wave" in the protos: populate
  `detail`, log at WARNING, and RECORD the refusal site in your report so
  the lead can batch the proto request. Do not edit protos. Do not invent an
  arm.

## Logging

`src/log.ts` (the moved `uds/log.ts`) is the only API; every logical branch
logs (warnings at `warn`, errors at `error`); records carry
`agent_repl_session_id`/`claude_session_id` where known. The workspace sink
is fd 3; the contract is `modules/app/agent-repl/logging-contract.md`.

## The mocked vendor (fake/)

- A `Scenario` is selected by prompt text (exact `!name …` prefixes; see the
  registry). The REAL shim runs unchanged over `createFakeQuery`.
- The `ScenarioContext` offers: `emit(sdkMessage)`, `emitStream(event)`,
  `canUseTool(...)`, `files` (the vendor-files writer), `gate()` (the turn
  gate), `sessionUuid` (mutable for rotation), `model`, `permissionMode`,
  `newUuid()`, `nowMs()`, `turn`, and the live-task table.
- Every scenario writes what the real binary would write: transcript lines
  for the user prompt, each assistant message, each tool result (with
  `toolUseResult`), system lines; subagent transcripts + `.meta.json`; spools
  terminated by `EXIT=<code>`. Shapes come from `testdata/corpus` and
  `sdk.d.ts`, never invented.
- File layout the mock writes (relayed to the store/sidecar lead):
  - `$CLAUDE_CONFIG_DIR/projects/<cwd-slug>/<vendor-session-id>.jsonl`
  - `$CLAUDE_CONFIG_DIR/projects/<cwd-slug>/<vendor-session-id>/subagents/agent-<agent-id>.jsonl`
    and beside it `agent-<agent-id>.meta.json`
  - `<spool-root>/<cwd-slug>/<vendor-session-id>/tasks/<task-id>.output`
    where `<spool-root>` = `$AGENT_REPL_FAKE_SPOOL_ROOT` or
    `/tmp/claude-<uid>`; task ids are `b<hex>` (shell), `a<hex>` (agent)
  - `<cwd-slug>` = the absolute cwd with EVERY byte not in `[A-Za-z0-9]`
    replaced by `-` (underscore included; case preserved), verified against
    the live tree: `/private/var/folders/_m/x` → `-private-var-folders--m-x`.

## Tests

- Unit: vitest, one `test/<module>.test.ts` per `src/<module>.ts`; the
  proto→code mapping means one test per base function and per use-site
  function. `AGENT_REPL_FORBID_VENDOR_CALLS=1` is set by `test/setup.ts`.
- Integration (`test/integration/`): spawns `dist/main.js --fake` (or the
  built entry) against an in-process fake store.v1 server
  (`test/fakes/store-server.ts`, shared with the store-client agent) and
  drives shim.v1 with a Connect client over the UDS. No other real system.
- The SDK upgrade canary (`test/sdk-canary.test.ts`): asserts every relied-on
  SDK export, Query method, Options key, message subtype and tool
  input/output type name exists in the pinned `sdk.d.ts`/`sdk-tools.d.ts`
  by reading the declaration files; fails loudly on removal or reshape.

## Commit and report discipline

Atomic commits on your branch; tests ride with the change they cover;
`npm run typecheck`, `npm test`, `npm run build` green before you return.
Report: what landed (commit range), suites run and results, every
prescribed detail you overrode and why, every derived-arm refusal site,
every UX or contract gap you surfaced instead of improvising.

## Rulings adopted 2026-08-29 (after kickoff; these win over anything above)

- PROTO LANDING (merged from overhaul/integration): every shim.v1 failure
  message now carries its derived `kind`/`cause` arms (StartSession,
  SetSessionModel, SetSessionPermissionMode, Hibernate, KillSession,
  StartTurn, UpdateAgent, KillTurn, StopBash, DetachForeground, ReadHistory)
  and `SessionFault` has a `kind` oneof. Populate the arm; `detail` stays a
  human string. WatchBash refusals are transport-level Connect errors (a
  stream has no failure message). The workflow trio answers
  `Code.Unimplemented`.
- `AgentUpdate` gained two page-line arms the shim PRODUCES: `context_cut`
  (conversation.v1.ContextCut — /clear, compaction, compaction_failed) and
  `api_error` (ApiRequestFailed as MID-TURN evidence; a turn terminal is
  still `AgentFailure.api_request_failed`).
- KEEP-ALIVE MARKER: every keep-alive prompt the shim submits BEGINS with
  the literal `<!--agent-repl:keepalive-->` (mirrors the existing
  `<!--agent-repl:meta-->` marker). The store/sidecar treat a turn opened by
  such a prompt as keep-alive until the next non-keep-alive prompt. shim.md
  publishes this.
- Q1: /agents and /help are recognized by the DAEMON and never reach the
  shim; the shim probes no catalogs.
- Q5 APPROVED: the one-time real capture run happens; the lead dispatches it
  once the capture harness reports ready. Until the captures arrive the mock
  is built from `testdata/corpus` + `sdk.d.ts`; afterwards its scripts and
  the goldens are rebuilt FROM the captures.
- LOCKS: the convention in "Process shell" is the cross-system contract; the
  daemon probes the workspace lock only.
- VERSIONS: `@connectrpc/*` and `@bufbuild/protobuf` are pinned to exact
  versions in package.json.
- LOCK DIR OVERRIDE: env `AGENT_REPL_LOCK_DIR` overrides the lock directory
  (default `~/.cache/agent-repl/run/`); the real shim honors it.
- STALE BINDINGS: the three stale generated files under proto/gen/ts/shim/v1
  are deleted on overhaul/integration; nothing depends on them.
- FANOUT REGROUPED under the session-wide subagent cap (at most three
  implementation agents running at once): wave 1 is four briefs — ENGINE
  (engine/*), RECORD PLANE (convert/* + store/*), MOCK VENDOR (fake/* +
  AGENTS.md table + shim.md mock section), INTEGRATION SUITE
  (test/integration) — the first three first, the suite in the first freed
  slot, auditors after.
- RPC COUNT: shim.v1 has SEVENTEEN rpcs (shim.md's "16" undercounts;
  ReadHistory is the seventeenth); audits count 17.
- PAGE SIZE ZERO IS REFUSED (project lead ruling, supersedes an interim
  lead ruling): `page_size` 0 on StartTurn/WatchAgent/ReadHistory is
  InvalidArgument — presence, never sentinels; the daemon always passes an
  explicit size. The scaffold's validator stands.
- LOG SESSION ID (project lead ruling): the daemon exports
  `AGENT_REPL_SESSION_ID=<HostSessionId>` in the shim's spawn env for LOG
  CORRELATION ONLY (never a session fact; StartSession stays the only
  carrier of session facts). main.ts passes it to configureLog as
  `agent_repl_session_id` when present and falls back to the self-name
  `shim-<workspace-md5-8>-<pid>` otherwise; the vendor session id attaches
  once known.
- LEFTOVER MODULES: src/api-usage.ts, subscription-usage.ts, usage-log.ts,
  model.ts survive from the old shim; the record-plane agent adopts (as
  harvest for TokenUsage / account-usage mapping) or retires each, with a
  stated reason; model.ts's normalizeOptionalModel may serve the engine.
- STANDING-STREAM TRANSPORT RULES (store lead, connect v1.17.0 semantics;
  recorded in the shim's AGENTS.md): (1) the shim.v1 server FLUSHES RESPONSE
  HEADERS the moment it accepts WatchSession/WatchAgent/WatchBash, so
  acceptance is observable before the first frame (on top of the immediate
  diagnostics push); (2) the store client ends WatchAgentSession by
  CANCELLING its context (an AbortSignal on the call) — connect's stream
  Close drains the body and blocks forever on a standing stream; never Close
  alone. The store flushes on accept too.
- E2E MOCK ADDITIONS (project lead, for the e2e suite): three scenarios
  join the mock and the table — (1) a `get_context_usage` driver whose
  answer CHANGES between turns so a distinct `context_usage` push is
  observable (cadence rule, engine-side: context_usage is pushed at session
  start and at every turn end regardless of scenario); (2) a session FAULT
  with a degraded window on `diagnostics` (a vendor message missing a
  required field → converter_defect) and a recovery (healthy again, the
  window closed with a dropped_count); (3) an UNSOLICITED `model_changed`
  (the vendor's own fallback — the model_refusal_fallback / session state
  path), distinct from SetSessionModel's confirmation.
- LANDING 3: `DetachedLost {file_vanished | went_silent | swept_up}` is
  threaded as `lost` arms on AgentBashInterrupted.cause,
  AgentSubagentFailure.cause and AgentFailure.failure; the stream plane
  produces them from its own LOST judgments (a spool the store stops
  feeding, a task that vanished from the level signal without a terminal).
- E2E MOCK ADDITIONS, continued (integration author finding): the first
  mock roster has no `!usage-*` or `!fast-*` producers, so the additions
  also cover (4) FAST MODE on/off/cooldown (`!fast-on`, `!fast-off`,
  `!fast-cooldown` via the init / session-state fast_mode_state fields) and
  (5) ACCOUNT USAGE with every window populated plus one scenario per
  unavailable reason (`!usage-full`, `!usage-service-unavailable`,
  `!usage-window-unavailable`, `!usage-utilization-unavailable`).
- SESSIONWATCHER RULINGS (project lead, binding; landing 4 carries the
  proto comments): (1) `SessionStarted.live_work` re-adoption announces
  EVERY live item with the `created` origin — `work_created` states the kind
  and description — never `detached{...}`; the description is sourced from
  the shim's own record (the `detached:<work id>` page line when it was
  created-origin, else the unit's start: its `activity:<id>` page line in
  the announcing agent's book, the bash run's start row via WatchBashRun, or
  the subagent's own book opening); an unreadable case is REPORTED, never
  restated as `detached{requested}`. (2) `DetachedWorkId.value ==
  AgentActivityId.value` — the spawning call's tool_use_id (for a subagent
  also its AgentId); the vendor task_id is an internal lookup (task_started
  maps it) used only for stopTask/level signals, never on the wire.
  (3) `SessionUpdate.context_budget_warning` (24) is retired in landing 4;
  the warning becomes `AgentUpdate.context_budget_warning = 7 {text}`, a
  page line the SIDECAR produces from the transcript attachment; the shim's
  converter maps to that arm and never emits it live.
- DISPATCH TIERS (user ruling): every NEW implementation dispatch is
  `opus-low` (Opus at LOW effort); agents already running or being resumed
  keep their tier. Implementers MAY offload mechanical, fully-specified
  writes (boilerplate, tests from a settled table, rote conversions, doc
  sections) to `sonnet-medium` at their judgment; the offloading agent stays
  accountable — it reviews the output, runs the suites, and reports the
  offload. Both statements ride every brief. If the types are not offered,
  `subagent_type: "claude"` with `model: "opus"` / `"sonnet"` and the effort
  stated in the brief.
- COMPACTION RULE (user ruling): the moment the lead's context is compacted
  it drops to LOW effort (`/effort low` if available; else explicitly — no
  re-derivation, no exploration; act on the summary + these docs + the
  ledger below) and says "compacted" to the project lead.

## LIVE LEDGER (shim lead; updated at every dispatch/merge)

- Branch `overhaul/shim`, worktree `~/.config/doom-overhaul/shim`. Merged and
  retired: scaffold, capture harness, engine, mock (first roster).
- RUNNING (opus-medium, keep tier): RECORD PLANE agent `a699887424d695c83`
  in `~/.config/doom-overhaul/shim-agents/record` (branch
  `overhaul/shim-record`; steps 1–10: StoreLineAt pointers, WatchBashRun,
  lost arms, producer re-key, engine extras page/not_deliverable/unsupported,
  owed unit files, created-origin live_work, DetachedWorkId == activity id,
  budget-warning mapping); INTEGRATION AUTHOR `a4e55fa0df5a8d8c9` in
  `shim-agents/itests` (branch `overhaul/shim-itests`; seven suites; never
  runs them); MOCK ADDITIONS `a4c3a36d86587908d` in `shim-agents/mock2`
  (branch `overhaul/shim-mock2`; fast/usage/context-drift/fault/fallback).
- NEXT: merge each landing (verify typecheck/test/build/smoke); run
  `npm run test:integration` (pretest builds); remediation dispatches at
  `opus-low` in hand-made worktrees `overhaul/shim-<slug>`; fresh fable
  auditor (`subagent_type: "claude"`, `model: "fable"`) over docs/overhaul/
  {shim,daemon,store,sidecar}.md vs test/integration; loop; landing 4 merge
  (retired context_budget_warning arm; daemon error arms); final report per
  COMMON.md (must include the R9 rule + evidence, the mock file layout, the
  prompt→scenario table location = agent-shim/claude/shim/AGENTS.md).
- ESCALATIONS OUTSTANDING: none (all seven answered in landing 3).
- Project lead address for SendMessage: `main`.
- LEDGER: mock additions landed on overhaul/shim-mock2 (2babe459c..e017fe398,
  awaiting one fix: `!usage-window-unavailable` must null five_hour). ENGINE
  GAPS queued for an opus-low remediation agent after the record plane lands
  (they touch engine/pushes.ts, session.ts, the fold seam): (a) a fold
  converter defect (StoreUnparsed with parse_error "converter defect: …")
  must raise SessionFault.converter_defect + a degraded window and recover
  (a FoldContext.reportFault channel or the Persistence fault source);
  (b) model_changed must also be derived from the next assistant message's
  message.model (unsolicited vendor fallback), not only init/SetSessionModel;
  (c) fast_mode has no producer — the engine drops the fold's fast_mode
  update as an owned arm and pushes none; produce it from init/result
  fast_mode_state.
- LEDGER (after outage 2): landing 4 merged at d83fdb250 (rate_limit_status
  28; tag 24 retired → AgentUpdate.context_budget_warning 7; DetachedWorkId
  == AgentActivityId in the proto comment). All three implementers resumed
  by message with the relay: record plane (from 2f3dcb22e; rate_limit_event
  mapping + budget-warning retarget added to its list), integration author
  (from 7e60e77f8; rate_limit_status assertion, budget-warning test
  retired), mock additions (from e017fe398; five_hour fix pending). Next:
  merge each, run `npm run test:integration`, opus-low remediation for the
  three engine gaps + suite failures, fable audit loop.
- LEDGER: mock additions merged at cd73f3c3b (worktree/branch retired;
  table 138 rows incl. `!usage-opus-absent`). Running: record plane
  `a699887424d695c83` (steps 1–10 + landing-4 mappings), integration author
  `a4e55fa0df5a8d8c9`. One slot free; the engine-gap remediation waits for
  the record plane to land (shared engine files).
- LEDGER: integration suite merged (worktree/branch retired): seven suites
  under test/integration (~185 tests, 4 todos) + test/integration-support;
  `npm run test:integration` (pretest builds). REMEDIATION LIST for the
  opus-low agent after the record plane lands: the three engine gaps above;
  a keep-alive interval override honored ONLY under --fake
  (`AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS`) so the two keep-alive todos
  become real tests; a read-call ledger on test/fakes/store-server.ts
  (GetLiveWork/OpenAgentSession/ReadAgentPage/WatchBashRun counts) for the
  detached todo; then every integration failure the first run surfaces.
- LEDGER: fable auditor #1 `aa66e99f6df4f018c` running read-only over the
  merged suite (04703c1c3) vs docs/overhaul specs; its critiques feed the
  first remediation dispatch together with the first integration run.
- REMEDIATION PRIORITY (project lead): FIRST (a) fold converter defect →
  SessionFault.converter_defect + degraded window on diagnostics, recovery
  closes it (`!fault-converter` / `!fault-recover`); SECOND (b)
  model_changed ALSO derived from the next assistant message's
  message.model when it differs from the session's current model
  (`!model-fallback`; RULING: the daemon must see unsolicited model changes —
  the model chip is truth, not the last SetSessionModel); then (c) the
  fast_mode producer, the keep-alive override, the fake-store read ledger,
  the auditor's critiques, and the first integration run's failures. The
  producer re-key (record plane step 4) is confirmed to the lead on landing.
- LEDGER: record plane merged at 731aa5f00 (worktree/branch retired; 2205
  unit tests, smoke 10/10). Producer re-key confirmed to the lead.
  Remediation #1 worktree `shim-agents/remed1` (branch
  `overhaul/shim-remed1`) cut for the opus-low agent: (a) converter_defect
  fault + degraded window + recovery, (b) unsolicited model_changed from
  message.model, (c) fast_mode producer, (d) `AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS`
  under --fake, (e) fake-store read ledger, then integration failures and
  auditor #1 critiques as they arrive.
- RECONCILIATION RULINGS (project lead): (1) a live unit the record cannot
  DESCRIBE is never left open — its KIND is known from where GetLiveWork
  found it (bash rows → bash, agent book → subagent, workflow table →
  workflow); reconciliation CLOSES it with `lost.swept_up` and logs at WARN
  naming the id; no kind-free terminal exists. (2) `file_vanished` is the
  sidecar's arm only. (3) landing 5 adds a `not_observed` arm to
  `AgentBashInterrupted.output`; until it lands the current shape stays,
  then lost/reconciled runs state `not_observed`.
- BUDGET-WARNING KEY (project lead, cross-plane): the shim's
  AgentUpdate.context_budget_warning page line is keyed
  `session:context_budget_warning:<record uuid>` (the transcript line's
  uuid, identical to the sidecar's), never `budget:<uuid>`; a test pins the
  spelling. Assigned to remediation #1 as item (g).
- BASH ROW KEYS (project lead amendment, binding): the start row is
  `bash:<run id>`, each output delta `bash:<run id>:<from_offset>`, the
  terminal `bash:<run id>:terminal`; WatchBashRun replays in write order;
  one row never supersedes another. Assigned to remediation #1 as item (h).
- RESIDUE KEY (project lead ruling): `residue:<vendor record uuid>` on both
  planes when the record has a uuid; without one the shim mints
  `residue:stream:<sequence>` and the sidecar `residue:file:<path>:<offset>`
  (they must not collide). Assigned to remediation #1 as item (i); item (j)
  adds `!rate-limit-five-hour` / `!rate-limit-seven-day` to the mock for the
  e2e footer-join case.
- STORE FAILURE ARMS now typed (invalid_request | stale_pointer |
  storage_failure on Open/ReadAgentPage; storage_failure on GetLiveWork;
  invalid_request | storage_failure on WriteBatch): the shim's reader must
  switch on them, never on detail prose (remediation #2).
- INTEGRATION RUN #1 (tree 04703c1c3, pre-record-plane; stale): 98 failed /
  91 passed / 4 todo in 1623 s (many 60 s timeouts). Persisting signals to
  carry into remediation #2: six `[internal] internal error` Connect
  responses (a shim exception escaping as internal — every handler must map
  known refusals to arms and unknown exceptions to a logged
  Code.Internal WITH detail), socket hang-ups on kill/stand-down paths,
  DetachForeground `unsupported` on the `!ctrl-b` unit (the mock must make
  the vendor detachment BEFORE the call, or the test must wait for it), and
  the timeouts. Run #2 on the merged tree is the authoritative inventory.
- LEDGER: landing 5 merged at 05046cd34. Remediation #1 resumed (items
  d/e/f committed; i mid-flight; new k not_observed, l context_tip
  disclaimer, m agent-spool EXIT bug). INTEGRATION RUN #2 (tree 731aa5f00+):
  99 failed / 90 passed / 4 todo, 76 of 99 are 60 s timeouts — systemic;
  an Explore agent is bucketing the log by root cause for the remediation
  #2 brief. Known already: SHIM_BUILD_SHA must be read from the SPAWN ENV
  at runtime (build-identity bakes a define; harness spawns with =itest and
  the shim reports ''), six handler exceptions escape as Code.Internal,
  the fake store lacks the typed refusal arms, !ctrl-b vs DetachForeground
  confirmation mismatch, a cold-seed ENOENT path mismatch.
- LEDGER (resumption 2026-09-01, recreated fable-low lead): tip f70941f0c
  (pause tip + four capture-harness fixes by the project lead). Rulings
  received: R15 wins the fresh-page contradiction (StartTurn's page holds
  exactly the prompt row); store relays (WatchBashRun CodeNotFound before the
  first row; post-terminal deltas served then end; re-upsert absorbed); 67
  real goldens at ~/.config/doom-overhaul/captures (mock scripts rebuilt FROM
  them; `_failed/` never fixtures); harness defects (parked-gate interrupt
  trigger; sweep-end second reclaim) to fix and report; dead-code pass
  (knip/tsc unused/vitest coverage) before the final report; re-check audit
  reds for stale-payload shape before production fixes. Dispatched opus-low
  in hand-made worktrees shim-agents/{harness,engine,fakes} (branches
  overhaul/shim-{harness,engine,fakes}): harness = the two capture defects;
  engine = buckets 1,2,3-rem,5,8(engine),10,12(engine) + R15 amendment +
  store relays + audit A2–A5,A7,A11,A13,A15,A22,A23,A25,B4–B6,B11,C7,C12,
  E1,E5–E8; fakes = buckets 4,6,7,8(mock),9,11,12(rest) + the remaining
  audit items. Integration run #3 started on f70941f0c for the record
  (scratch `shim-itest-run3.log`). Queued next: captures-rebuild brief
  (scenario scripts + converter goldens from the captures; checklist
  answers read out of them), fable auditor #2, dead-code pass.
- AMENDMENT (user ruling, TEAMLEAD.md on overhaul/integration @ 25bf69341):
  the dead-code pass is dispatched to a `sonnet-medium` agent (never opus-low,
  never the lead) in its own worktree, tool output as its brief; the lead
  reviews, merges, and carries the deletion + kept-with-reason lists into the
  final report.
- LEDGER: harness fixes merged at f12b754c3 (`on_control` trigger kind;
  sweep-end late reclaim; capture subset 315 green; `--check` 75 scenarios);
  project lead notified for re-capture. Run #3 on f70941f0c: 101 failed /
  92 passed / 1 todo. A held-turn-gate trigger follow-up is in flight on
  overhaul/shim-harness.
- LEDGER: held-turn-gate on_control trigger merged at 5554d2d0b. CAPTURE
  READOUT (read-only Explore over the 67 goldens): no context_tip and no
  budget-warning attachment anywhere (nearest carrier observed once:
  attachment `total_tokens_reminder`); no failed subagent; no
  compact_boundary; no /clear rotation — compaction-directed and
  identity-rotation-clear recorded only turn 1 (later turns submitted after
  the query closed; 14 `control_failed` "ProcessTransport is not ready for
  writing" corpus-wide) → harness fix in flight, re-capture owed. Real
  subagent files are keyed by the vendor agentId (17-hex task id) with
  meta.toolUseId the only link to the spawning call; bucket 9 corrected to a
  reader-side join (mock layout stays vendor-faithful). The spawning tool is
  `Agent` on the wire, `Task` in init.tools. toolUseResult is polymorphic
  (object | bare string on denials/StructuredOutput | list for MCP).
- LEDGER: harness batch merged at 48ed642eb (multi-turn drain held across
  turns via drainToResult; transport-closed control_failed quarantines;
  permission-undecidable-parked expects_error_subtypes; denied-by-user
  prompt rewrite); harness worktree/branch retired. Re-capture list (12)
  sent to the project lead: compaction-directed, identity-rotation-clear,
  fan-wide-cancel, permission-mode-changed, model-changed, account-usage,
  bash-detached, subagent-detached, context-usage, mcp-server-healths,
  permission-undecidable-parked, permission-denied-by-user.
- LEDGER: slow-down lifted (user go); project lead re-capturing the 12.
  Running: engine (shim-agents/engine), fakes (shim-agents/fakes, resumed
  twice after rate-limit/watchdog stalls), GOLDENS (new, shim-agents/goldens,
  branch overhaul/shim-goldens off 691982ddf): commits the 55 stable
  captures selectively under agent-shim/claude/shim/testdata/captures/ +
  MANIFEST, converter golden suites through fold-harness, converter fixes
  only in src/convert. Queued: scenario-script rebuild from captures (after
  fakes merge + re-capture), fresh integration run, fable auditor #2,
  sonnet-medium dead-code pass.
- LEDGER: re-capture sweep complete (project lead): 16 ran, none
  quarantined; 13 goldens replaced (the 12 + held-turn-gate); 69 goldens
  total. Caveat to verify: compaction-directed may still carry no
  compact_boundary under the cheap model. Goldens agent told to include all
  69 and to report the compaction and /clear shapes; the scenario-script
  rebuild waits on the fakes merge.
- LEDGER: goldens agent interim: 69 goldens folded; compaction-directed
  holds NO compaction record ("Not enough messages to compact" local_command)
  → gap stays open; /clear rotates to the post-clear init's session_id (three
  ids, three files, no closing record) → rule written into shim.md R9 and
  sent to the engine agent; two converter defects fixed (task_notification
  kind, per-block assistant line before content_block_stop). The scenario
  rebuild brief must emit assistant lines per block BEFORE content_block_stop
  and the observed /clear shape.
- LEDGER: goldens merged at eb7930399 (worktree/branch retired): 69 real
  captures under agent-shim/claude/shim/testdata/captures/ (7.8 MB; the
  241 MB turn-stop-max-turns spool excluded) + MANIFEST with an
  evidence-gaps section; golden suites test/convert/goldens/*; converter
  fixes in convert/detached.ts (task kind only on task_started) and
  convert/stream-events.ts (per-block assistant line before
  content_block_stop); unit 3682 green. Arms no capture exercises (asserted
  nowhere, not faked): glob, grep, artifact, scheduleWakeup, worktree,
  contextInjected; no failed subagent; no compact_boundary. Queued fix:
  unify the two `SourceCoordinates` types (store/keys.ts vs
  store/persistence.ts) into one shared declaration (rides the scenario
  rebuild brief).
- LEDGER: landing 6 (overhaul/integration d46e601e7/8a98e4fca/bc0a07bae)
  merged at c9280627f, no conflicts; no shim.v1/conversation.v1 change;
  typecheck/unit(3682)/build green. Notes from the project lead: the
  daemon's SubmitPromptError.turn_already_open is retired — the shim's
  UpdateAgentFailure refusal of a busy subagent is the ONE producer and stays
  typed and named; the compaction writer stays graded against the corpus
  fixture and marked synthetic until a longer-history capture is approved.
- DIRECTIVE (user, via project lead): the project pauses once every lead has
  resolved. Shim finishes its recorded queue — remediation #2 merges,
  integration rerun, scenario rebuild from the 69 goldens, auditor #2 loop,
  sonnet-medium dead-code pass, final report with both dead-code lists and
  the capture-checklist answers — then parks: no dispatch after the report,
  STOP-shim.md rewritten to the resumption state, lead stays resident.
- LEDGER: engine remediation #2 merged at 062aaeda5 (worktree/branch
  retired): buckets 1,2,3-rem,5 fixed; 8/10/12 engine halves 3/4 each; R15
  applied; store relay (a) via per-row backgroundTasks; rotation = post-clear
  init id; Persistence.onFault/onDegradedWindow were never subscribed (fixed);
  dead query now writes the open turn's failure terminal; ReadHistory store
  outage → session fault. Integration on the merged tree: 169 / 25 / 1 in
  184 s. RULING PENDING (project lead): denied tool's START frame — defer
  start until the gate speaks (a) vs eager start + denied terminal (b).
  Hand-over to the fakes/rebuild brief: `!bash` foreground park scenario for
  the unsupported-detach test; `!cold-seed` must stamp the last ASSISTANT
  line old; convert/detached.ts hard-codes `requested` on task_started /
  task_notification (ctrl-b by_user preceded by a requested announcement;
  outputReadable unset on !bash-detach); the fold drops the usage carrier.
- RULING (project lead): denied tool's start frame — do NOT defer starts;
  denial RETIRES the unit: settle its `failure` arm with the denial as cause
  (AgentToolFailure naming the permission id), no success/output; the gate
  test asserts start-then-denied-failure. Recorded in shim.md's gate section;
  code change rides the fakes/rebuild converter brief. No proto change.
- RULING CLARIFIED (project lead, final): denied call settles `failure` with
  content UNSET + settled_at; drawn denied via the permission unit's shared
  id; no AgentToolFailure denied cause; starts never deferred. shim.md gate
  section amended. Converter change + gate test amendment ride the
  fakes/rebuild brief.
- LEDGER (2026-09-02): fakes remediation merged at 1768697c7 (worktree/branch
  retired): buckets 4,6,7,8(mock),9(reader-side join),11,12 done; unit 3764
  green; SMOKE red at the seam (no store in the smoke + new session-row
  producers + ruled exit-1 on unacked rows) → item 0 of the rebuild brief.
  Integration run #4 started on 1768697c7 (scratch shim-itest-run4.log).
  Dispatched REBUILD (opus-low, shim-agents/rebuild, overhaul/shim-rebuild):
  smoke fake store; scenario scripts rebuilt from the 69 goldens with a
  golden-conformance test (declared-only scenarios marked in AGENTS.md;
  compaction helper stays synthetic); converter: denied tool → failure with
  no content, usage carrier, detached causes, SourceCoordinates unification;
  engine hand-over levers (parking foreground bash; !cold-seed assistant
  stamp); then every remaining integration failure.
- LEDGER: integration run #4 on 1768697c7: 180 passed / 16 failed / 1 todo
  in 184 s (detached 8, turn 4, session 3, gate 1); relayed to the rebuild
  agent as its baseline.
- LEDGER: rebuild merged at 3dc64a89b (worktree/branch retired): smoke runs
  the fake store + a no-store exit-1 step; golden-conformance suite (59
  mapped rows, 27 shape-exact, 32 pinned with reason; AGENTS.md marks
  capture-grounded vs declared-only); denied tool → failure(no content);
  /clear = observed shape (invented closing record removed); detached causes
  remembered; SourceCoordinates unified; subagent start frame restored;
  StartTurn reads its page BEFORE the submit (R15). Unit 3916, integration
  187 / 9 / 1. RULINGS (lead): WatchAgent serves ONE book — "immediate
  children" = the agent's own units, subagent frames on their own
  WatchAgent; fan-wide cancel is KillTurn/KillSession{force} issued by the
  caller; INTERIM unknown-target refusal at the shim (Code.NotFound when the
  id is unknown to the shim AND the store page is empty) pending a landing-7
  `unknown_agent` arm on OpenAgentSessionFailure (proposed). Dispatched:
  remediation #3 (opus-low, shim-agents/engine3) for the nine engine
  failures; fable auditor #2 (read-only, fresh) over 3dc64a89b.
- LEDGER: auditor #2 returned 61 items over 3dc64a89b; triaged at
  docs/overhaul/reports/shim-audit-2.md (accept all; dispositions on 32/57
  standing lever, 58 three declared-only terminals to mark, 59 label
  synthetic, 56 never invent a cut, 61 keep the bounded re-drain and fix its
  docstrings). Many are audit-1 carry-overs remediation #2 did not reach.
  Remediation #4 = two opus-low agents (S: session/process/record/transport;
  T: turn/gate/detached + goldens) dispatched after remediation #3 merges
  (shared session/detached tests and engine files).
- LEDGER (2026-09-02): remediation #3 merged at 5721237ec (worktree/branch
  retired): INTEGRATION GREEN 196 / 0 / 1; routes.ts boundary maps every
  handler (ConnectError through, else logged Internal with detail);
  stoppedBashTerminal written by StopBash/KillTurn/teardown; interim
  unknown-target refusal; held context cuts written once identity settles;
  teardown snapshots the live set before stopping; knowsAgent records every
  announced created_agent_id. Override accepted (implementation detail):
  LiveWorkTable keeps a fixed 64-entry ring of retired handles so
  StopBashFailure.already_ended is answerable; beyond it `unknown_work`.
  Mock now models a vendor process surviving its shim (survivingShellRuns)
  for the re-adopt path — invented, not capture-grounded; marked. Dispatched
  remediation #4 (audit #2): agents S (shim-agents/audit-s) and T
  (shim-agents/audit-t) per shim-audit-2.md's split.
- RULING (project lead, cross-system): the WORKSPACE lock moves from startup
  into StartSession beside the session lock; an inert shim holds neither;
  probe semantics unchanged; workspace conflict → StartSession
  conversation_owned. Folded into remediation #4 agent S (owns main.ts lock
  path + process.test); shim.md KERNEL LOCKS amended; "Process shell" startup
  order above is superseded (locks are no longer taken before bind).
- LEDGER: remediation #4 agent S merged at faef4c312 (tip 7aa77d52d):
  workspace lock inside StartSession (be119abbf; lead notified); signal
  handlers before the "serving" record; host_shutdown terminal now produced;
  mock result/turn record share one uuid; harness dialed the wrong socket
  for a second shim (fixed); `AGENT_REPL_FAKE_REFUSE` lever; integration
  238 / 0 / 2 todo. Rulings by the lead: KillSession query_refused_to_end
  stays a todo (no producer; fatalizing a refused interrupt is a contract
  change); retry-buffer overflow survivors-order test deferred to the resume
  queue (no drain hook this wave); identity_rotated key has no file-plane
  join (accepted). S resumed for one fix: fresh StartSession retry after
  vendor_start_failed must not escape as Internal. Agent T still running.
- LEDGER: remediation #4 agent T merged (tip f7051db55, no conflicts):
  standing grants validated against the OFFERED standing (altered/unoffered
  → answer_mismatch); UpdateAgent resolves a subagent target by its wire
  AgentId (tool_use_id) via the spawn map; non-zero bash exit → completed +
  exited(code); mock: parent_tool_use_id honored on stream events, error
  results keep their terminal_reason, api_error_status rides error results;
  levers !perm-allow-standing-mode, !perm-no-standing, !perm-hold,
  !query-eof-mid-ask, !subagent-detached-live; permission mode conditions
  the mock gate. Lead accepts T's deviations: 40 (Ctrl-B turn terminal is
  completed, backgrounded is the whole-turn arm), 17 (a stopped subagent's
  terminal is a page line of the spawning agent's book), 15 vendor_refused
  todo (no lever: a closed prompt queue is query_dead), 8 (AgentUpdate.
  api_error has NO stream-plane producer — system:api_error is a transcript
  record, so it is the SIDECAR's arm; shim.md's "the shim may produce both"
  is corrected to: context_cut yes, api_error no). RESUME QUEUE: item 2 —
  the fake store must model relay (b) (post-terminal deltas served, then
  end) and the test re-asserted against it. Recorded gaps: no stream-plane
  producer for AgentResponseFailure.reason on refusals (fallback content
  block, no response unit); billing/oauth_org/max_output_tokens
  indistinguishable by HTTP status; continuation_prevented ungrounded.
  h1 multi-stream compressed-envelope defect handed to agent S.
- LEDGER: remediation #4 agent S follow-ups merged at 3a60a11b5
  (worktree/branch retired): failed StartSession undoes itself
  (Persistence.clearProducer, identity removed only if this attempt minted
  it; held cuts land after the query is up; `AGENT_REPL_FAKE_REFUSE=start-once`);
  h1 multi-stream compressed-envelope defect = early-head flush absorbing the
  adapter's connect-content-encoding head → response compression OFF
  (compressMinBytes max; request decompression kept; a post-head non-identity
  encoding is reported at error). FINAL CODE TIP 3a60a11b5: typecheck,
  unit 3745, build, smoke, integration 302 / 0 / 3 todo (30 s). Dead-code
  pass (sonnet-medium, shim-agents/deadcode) next; then the final report.
- LEDGER: dead-code pass (sonnet-medium) interim merged at 174a2e995:
  knip unused exports in src 187 → 0, unused types 91 → 21 (sdk/types.ts
  upgrade-canary aliases, kept), tsc unused 7 → 1 (kept), zero-hit src
  functions 73 → 29; deleted src/subscription-usage.ts (+test),
  crossCheckDenials, READER_COMPONENT, dead re-exports, unused imports,
  ~245 needless `export`s; pinned: unavailablePersistence, the engine
  dispatch arrows, FAILURE_ARMS, InvalidModeledUsageError, REAL_SCHEDULER,
  noteVendorDenial/deniedCall, pushes return() path, reader
  transportFailure/concludeThrough/awaitFirstRow, stoppedBashTerminal,
  writer clearProducer/liveWork, main logCorrelation/queryFactory, server
  listen bind-failure, AsyncQueue.return, vendor-files
  vendorSessionId/survivingShellRuns, three scenarios. Agent resumed for the
  29: lead rulings — delete the four unused ScenarioContext accessors; pin
  the 21 engine facade functions through the engine harness (or name the
  integration coverage that hits them); pin the three interrupt-driven
  scenarios in-process; add knip.json naming scripts/dist-smoke.ts.
