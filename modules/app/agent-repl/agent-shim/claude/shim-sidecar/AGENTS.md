# agent-shim/claude/shim-sidecar/

The Claude file-plane reader (Go, singleton, launchd-managed). Responsibility:
observe the Claude harness's on-disk artifacts (session transcripts, agent
sidechain transcripts, workflow journals, `/tmp` task spools), parse them with
cursored, truncation-aware tailing, convert records into agent-shim protocol
events (same loud-validation contract as the shims), infer terminal `LOST`
transitions per the staleness policy, and write everything to the shim-store
with atomic cursor advancement.

The sidecar is 100% specific to Claude's file formats BY DESIGN; its entire job
is converting that vendor reality into the (treated-as-)vendor-agnostic
protocol. It interprets no resolved state and owns no database.

## Total-ingestion mandate

The sidecar handles ALL JSONL objects in the files it reads. If a JSON object
exists in a file on disk, it MUST end up written to the shim-store as a
protobuf shape — somewhere in the SQLite database, ultimately. No exceptions.
Ever.

- No sampling, no skipping, no "not visually interesting" filtering — curation
  is a downstream (daemon/frontend) concern, never an ingestion concern.
- The mandate binds INGESTION only. Downstream consumers (daemon, frontends)
  are free to never read a stored record, and to ignore records they do
  read — irrelevance to the user is a legitimate consumption-side judgment.
  It is never a legitimate reason to skip parsing a record or to leave it
  out of the database.
- A shape the schema cannot express is a SCHEMA GAP to be surfaced loudly and
  fixed (via the extras-enforcement contract that fails the build on
  undocumented extras), never a record to silently drop.
- The zero-`UnparsedEvent` golden-corpus contract is the executable form of
  this mandate; weakening it violates this document.
- DEFERRING a record is not skipping it. A record whose meaning depends on the
  line after it (today: the compaction boundary and its summary) may be left
  unconverted at the end of a batch — but only with the reader's cursor parked
  BEFORE it, so the next scan, and a restart, both read it again. A deferral
  that could lose the record is a violation of this mandate; see
  `tail.Context` ("deferred frames") and `internal/handler/clearcompact.go`.

## Ingestion is connection-scoped, never boot-scoped

The sidecar reads a watched file ONLY while a store connection is established,
and the FIRST act of every established connection — boot and reconnect alike —
is recovering that store's cursors. There is no boot path: boot is simply the
first time the link is not up yet. See `link.go`.

- While no connection exists the sidecar produces NOTHING, loudly. A store that
  has not started yet is a down dependency, never a reason to read anyway.
- A tailer's read position may ONLY come from a cursor the store handed us on
  the live connection. `rescan` is the only thing that builds a tailer and it
  fails hard if that is not true.
- "Cold" means exactly one thing: a CONNECTED store that genuinely holds no
  cursor for a file. That is the backfill path and it reads from offset 0
  honestly.
- Recovery that fails is never softened into a cold start. The predecessor of
  this design recovered cursors once at boot and, on failure, re-read every
  watched file from offset 0 — a fallback masking a down dependency, which
  re-ingested whole conversations and drove an SSM task-count clamp storm.

## Vendor carry-over (viral)

Any future vendor-equivalent sidecar (e.g. a codex sidecar) MUST inherit this
AGENTS.md's mandates into its own AGENTS.md — including the total-ingestion
mandate above AND this carry-over clause itself, so the directive propagates
to every subsequent vendor equivalent in turn.

Dependencies: `proto/agentshim/` (generated Go), the shim-store UDS socket,
the Claude harness file formats it parses.

## Logging

- The sidecar owns one canonical JSON logging API in `internal/logging`,
  divided between normal and verbose emission functions. New or changed
  sidecar code uses that API only.
- Sidecar lifecycle and ingestion-service records are genuinely global and
  persist in `~/.cache/agent-repl/log/shim-claude-sidecar.log`. A diagnostic
  conceptually attached to a session is persisted through the store, shim, and
  daemon into `<workspace>/.claude/emacs/sidecar.log`. Do not duplicate that
  session narrative in the global sidecar log.
- Every new or materially changed nontrivial function logs its entry. Every
  meaningful branch that selects a different nontrivial block, call, state
  transition, or outcome logs its selection.
- The normal helper persists and emits to the terminal. The verbose helper
  reaches neither the durable sink nor the terminal unless
  `AGENT_REPL_LOG_VERBOSE` is enabled.
- Each error is logged exactly once by its owning layer with store socket,
  transcript path, cursor, session, operation, branch outcome, and cause.
  Error-path tests assert the canonical record and its context.
- Frequent or hot diagnostics use the verbose helper. Do not bypass logging.
  Direct diagnostic output through `fmt`, `log`, `slog`, or an ad hoc logger is
  forbidden except a documented pre-logger bootstrap failure or logger-sink
  emergency path.

## Verification

- `make coverage` runs the full suite with `-coverpkg=./...` and reports
  per-function and aggregate statement coverage.
- `modules/app/agent-repl/bin/test-all.sh` (from the repository root) runs
  every tracked suite across the module.
- Maintain at least 90% statement coverage. Until the measured sidecar baseline
  reaches that target, never reduce it, report the gap explicitly, and add
  focused tests for every critical branch and every error path changed.
- Run `modules/app/agent-repl/bin/report-logging-density.sh sidecar` and report
  its source-line and canonical-call counts as a rough review aid. It is not
  semantic logging coverage, so directly audit all critical branches and
  errors even when the ratio rises.
- After a commit lands on `master`, run
  `modules/app/agent-repl/bin/test-all.sh --record`, inspect
  `modules/app/agent-repl/test_time.csv`, and surface every reported timing
  regression.

## Conversion rules

Owned by `internal/convert` and `internal/handler`. The reader (this package's
root, `internal/tail`, `internal/discover`, `internal/storeclient`) decides which
files exist, where it has read to, who owns a spool and when it stopped seeing a
run; the conversion decides what a record MEANS. Everything crossing between them
crosses `tail.Handler`, `tail.Context`, and the two optional methods in
`internal/handler/seam.go`.

### The four outcomes a record can have

A page line, a detached run's frame, an unserved item, or — for the exempt set
alone — a drop. There is no fifth, and nothing on disk is ever silently lost.

- A **page line** names its book (`StorePageLine.page_agent_id`). A subagent's
  constituents form ITS OWN book; the SPAWN that created it is a line in the
  parent's.
- A **run frame** (`StoreAgentBash`) wraps the spawning call's unit id and is
  structurally unpaginatable.
- An **unserved item** is a keep-alive turn's item (no book), `vendor_specific`
  (understood, deliberately not carried — the follow-up is a CONVERTER),
  `unknown` (parsed, not modeled — the follow-up is a MODEL), or `unparsed`
  (unreadable — a FAILURE, carrying source, offset, parse_error and bounded raw).
- A **drop** is the exempt set only. Never residue, never `AgentUnmodeled`.

A recognizable modeled kind reaching `unknown` is a PRODUCER DEFECT. The
golden-corpus test asserts the `unknown` set is EMPTY and the `vendor_specific`
set is exactly the declared list, so a mapping that degrades into residue fails
the suite rather than quietly shrinking what the feed can show.

### Identity (R9)

- MAIN AGENT: `AgentId.value` == the transcript FILE's session uuid (the
  `<session>.jsonl` basename). NEVER the per-record `sessionId`, which diverges
  from the runtime's answer in ~22% of records; that divergence never rides the
  wire.
- SUBAGENT: the vendor `agentId` of sidechain records, which the `agent-<id>`
  file name repeats.
- `top_level`: the main agent for main-agent and sync-subagent frames; the
  subagent ITSELF when the spawn was backgrounded (its stream outlives the turn).
  UNSET only when genuinely unresolvable — residue naming no agent.
- Vendor identity (uuids, message ids, task ids) never crosses the contract.
- Every identity is READ FROM `tail.Context` (`MainAgentID`, `AgentID`,
  `SpawnBackgrounded`, `FileID`), defensively — empty means the reader has not
  supplied it — with path-derived fallbacks. Re-deriving what the reader already
  resolved is how the two halves of the seam come to disagree about whose book a
  record lands in.

### Keys and the write identity

- `write_id` = hex sha256 of `"shim-claude-sidecar|" + path + "|" + offset + "|"
  + discriminator`. DETERMINISTIC: randomness is forbidden, because replay
  idempotence at the store rests entirely on the same bytes minting the same id.
  The discriminator separates the several entries one record mints (a block
  index, `settle:<id>`, `terminal`, `diag`).
- `upsert_key`, all of it in `internal/convert/keys.go` because the shim must
  mint the IDENTICAL key for the same unit:
  - `activity:<AgentActivityId>` — the vendor `tool_use_id` for a tool call;
    `<message.id>:<block ordinal>` for a text or thinking block.
  - `question:<tool_use_id of the AskUserQuestion call>` — its own identity space.
  - `terminal:<AgentId>:<record uuid>`, `bash:<run activity id>`,
    `session:context_cut:<uuid>`, `session:api_error:<uuid>`.
  - Residue with no unit identity is keyed `residue:<write_id>`, so a re-read
    supersedes its own row instead of appending a second copy of the same bytes.
- BLOCK ORDINALS RUN ACROSS THE LINES SHARING ONE `message.id` and reset only
  when it changes, so they equal the SDK's `content_block_start.index` the shim
  sees. Every block consumes an ordinal — including a `tool_use` block with its
  own id and an exempt block producing no unit — or the positions drift from the
  API message and the two planes stop agreeing.

### Frames, upserts and instants

- A frame is an UPSERT OF ITS WHOLE UNIT. A tool_result re-emits the unit's
  settled state under the same activity id, never as a child; nothing in this
  contract has a tool call as a parent.
- An assistant message becomes SEVERAL units (thinking, each text block, each
  tool_use), never one row.
- EXACTLY ONE UNIT PER API RESPONSE carries `usage` and `effort`: the unit for
  block 0 of the response. Every other unit leaves both UNSET, or a consumer
  summing units over-counts the bill by the number of blocks.
- INSTANTS COME FROM THE FILE, never a clock here. Every `started_at` /
  `settled_at` is the record's own timestamp, so a re-read after a restart mints
  byte-identical frames under byte-identical write ids.
- PRESENCE, NEVER SENTINELS: an unreported figure stays UNSET. An absent effort
  is not "low"; an absent retry hint is not "retry now"; an absent sandbox report
  is not "sandboxed"; an absent subagent total is not zero.

### Joins — one indexed lookup each, never a lineage walk

- Tool RETURN → its call by `tool_use_id`. One remembered entry per OPEN call
  (name, input, start instant), deleted on settle, so the map is bounded by
  in-flight calls rather than transcript length.
- SKILL document → its call by `sourceToolUseID` on the isMeta user record.
  Direct and structural; the old skill-name-versus-whatever-arrives-next
  correlation is explicitly retired.
- IDE diagnostics → the last write/edit unit by ADJACENCY. One remembered value,
  sanctioned at the schema because the vendor's record carries no call id.
- Spawned agent → its spawn via `AgentSubagentStart.created_agent_id`.
- An ORPHAN tool_result (its call is behind the cursor) lands as
  `vendor_specific{kind:"orphan_tool_result"}` with a WARNING. It is a genuinely
  lost settle after a restart, so it is loud, not verbose.

### Deliberate departures worth knowing

- THE SPAWN UNIT IS ANNOUNCED AT ITS RESULT, not at its call.
  `created_agent_id` is the join key the flat model rests on and is not optional,
  but the vendor names the created agent only in the launch's answer. Announcing
  with it unset breaks presence; inventing one breaks identity. The settled frame
  carries the ORIGINAL call instant so a drawn clock measures the spawn.
- A SKILL and a MONITOR settle later by design (`settlesLater`): a skill's own
  return is a bare acknowledgement and the DOCUMENT settles it; arming a monitor
  does not end it. This is kept distinct from a settle the converter FAILED to
  perform, or a handled record would be reported as a mapping gap in the very
  query built to find real ones.
- A TASK ACT is instantaneous at this tier: what the tracker did and where it
  left the task both come from the result, so the call announces nothing.

### The exempt set, and its one carve-out

Dropped entirely: `TaskStop`, `TaskOutput`, `TaskGet`, `TaskList`, `ToolSearch`,
`NotebookEdit`, `REPL`, `ListMcpResources`, `ReadMcpResource`, `SendFeedback`.
A drop is not residue: filing a known built-in as `unknown` would pollute the
query that finds real modelling gaps.

CARVE-OUT: the `TaskStop` CALL stays dropped, but its RESULT is CONSUMED as the
owning task's CANCELLED terminal before the drop — a bash task becomes
`AgentBash.success.interrupted.by_user` on a `bash:` frame, an agent task
becomes `AgentSubagent.failure.stopped_by_user` on the spawn unit.
Deliberately-stopped work must resolve cancelled, never LOST.

### Withholding classes (`vendor_specific`)

CLI bookkeeping and machinery (`mode`, `permission-mode`, `queue-operation`,
`last-prompt`, `ai-title`, `pr-link`, `frame-link`, `file-history-*`,
`attribution-snapshot`, `system/local_command`, and the informational /
turn_duration / stop_hook_summary / away_summary / scheduled_task_fire /
model-refusal / agents_killed system lines); context-cut exclusions and the other
attachment machinery as `attachment/<type>`; the synthetic
`"No response requested."` assistant record; unmodeled content blocks as
`content_block/<type>`; workflow journal records and spools (workflow is KICKED
this wave — discovered and tailed so nothing is lost, converted to nothing yet).

R15: a FILE-PLANE USER PROMPT is `vendor_specific{kind:"user_prompt"}`, never a
page line. `AgentPrompt` carries a `TurnId` and a `PromptOrigin`, both
daemon-minted; a file reader holds neither, so no history page can regrow a fake
prompt bubble from this producer. The shim's `AgentPrompt` is the one served form,
and a subagent's commission rides `AgentSubagentStart.prompt`.

### Keep-alive

A user prompt whose first text block BEGINS with
`<!--agent-repl:keepalive-->` marks the turn keep-alive until the next
non-keepalive prompt (one remembered bool per file). Every record converted while
the bit is set lands on `unserved_item.keepalive` — structurally unable to appear
in any page, so no read filters them out and no activity routes onward.

### Context lifecycle, and the one legitimate hold

- `/clear` is detected by UNWRAPPING the expanded command envelope; the literal
  never appears on disk. The envelope must be the ONLY content — an argument or
  surrounding prose means the prompt quoted a command rather than invoking one.
- Compaction COALESCES the `system/compact_boundary` record with the FOLLOWING
  summary line in FILE ORDER. Never timestamp order: the harness composes the
  summary before writing the boundary, so the summary's timestamp is EARLIER, and
  a timestamp-ordered assembly pairs every boundary with the wrong summary in a
  session that compacted twice.
- Both land as `AgentUpdate.context_cut` — a page line of the MAIN agent's book,
  keyed `session:context_cut:<record uuid>`. A clear carries no token delta,
  because the vendor's reset record states none.
- THE HOLD is the only one: a trailing boundary is deferred (cursor parked before
  it) when the reader redelivers, bounded to ONE redelivery, and converted
  without its summary on the forced delivery — loudly.

### API errors

`system/api_error` → `AgentUpdate.api_error` = `ApiRequestFailed{message, kind}`,
a page line keyed `session:api_error:<uuid>`. EVIDENCE, NEVER A TERMINAL: the
turn's end is the frame-level failure arm and nothing else. The kind is the
VENDOR'S taxonomy, read from its `type`, falling back to its numeric status; an
unmodeled type is carried by name, and a transport failure (which has no vendor
type at all) is named `connection/<code>` rather than guessing a modeled kind.

### Detached shell spools

Bytes → `StoreAgentUpdate.bash` with `AgentBash.update{new_output, from_offset}`,
keyed `bash:<run>`. `from_offset` is a GAP DETECTOR, not addressing: it must
equal what the consumer has already accumulated, and a mismatch means the
consumer REFUSES the frame rather than concatenating across a hole.

`EXIT=<code>` → `AgentBash.success.completed` with `termination.exited`. The
matching is strict (last line of the batch, newline-terminated, line-start, at
most three digits) because `EXIT=` is common as ordinary output — 23 of the 44
real spools carrying it have it only mid-line.

LOST (`file_vanished` | `went_silent` | `swept_up`) →
`AgentBash.success.interrupted` with NO cause arm. `by_user` and `timed_out` are
the only causes available and neither is what happened, so setting one would be
an accusation with no evidence. HOW we concluded it survives only in the log
record. The wire has no `DetachedLost` this wave — a stated contract gap. The
LOST terminal shares the exited terminal's write identity, so a late LOST verdict
is absorbed rather than appended beside an observed exit.

A non-zero shell exit is COMPLETED, not a failure arm; empty search results are
SUCCESS with an empty answer.

### Logging

Every logical branch logs. CORRELATION KEYS RIDE DEDICATED `logging.Context`
FIELDS AND NEVER MESSAGE TEXT — a record whose identifiers are interpolated into
a sentence cannot be filtered or joined by the integration loop that reads these
logs. One base helper per package sets producer, path, file id, offset, agent,
vendor session and task (`Attribution.ctxFor/ctxWarn/ctxError` in `convert`,
`handleCtx/handleWarn/handleErr` in `handler`); a call site adds only what is
specific to its branch (`ActivityID`, `UpsertKey`, `WriteID`, `BookAgentID`).
Per-record success paths are VERBOSE; refusals, invariant violations, withheld
classes reaching a warning threshold, and every failure are normal verbosity.

### Suites

```
go test ./internal/convert/... ./internal/handler/...
```

`internal/handler/golden_test.go` is the contract test: it drives every
file-plane corpus fixture under `testdata/corpus/` plus the real transcript under
`projects/` and asserts zero `unparsed`, zero `unknown`, the declared
`vendor_specific` set, all four envelope duties on every entry, write-id
uniqueness, and byte-identical output across two runs. `testdata/corpus/stream/`
is excluded deliberately: those are SDK probes, the shim's input, and converting
them here would test a path production never takes. A shape gap found later
becomes a fixture there FIRST, then a fix.
