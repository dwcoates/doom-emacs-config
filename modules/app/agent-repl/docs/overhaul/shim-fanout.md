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
