# SIDECAR digest

DERIVED from figma-to-idl-redesign.md at 86fd2b543 — the canonical record WINS on any conflict. Do not edit; regenerate.

Scope: every standing decision, principle, consequence and gotcha bearing on the
sidecar — file tailing and discovery, transcript / journal / spool / meta
ingestion, conversion into `conversation.v1`, per-agent workflow transcripts,
detached shell spool semantics, `store.v1` writing and cursors, the
no-variable-state principle, and sidecar-tier producer notes. Facts are stated
as they stand at the freeze; where a later entry superseded an earlier one, only
the surviving fact is recorded.

## 1. THE GOVERNING PRINCIPLE: no variable-size state in the sidecar

**The shim, store and sidecar hold NO VARIABLE-SIZE STATE.** The user's rule:
these three components must not manage stacks, queues, trees or any structure
whose size depends on the data. Every observation must cost a CONSTANT number
of single indexed lookups — one parent-id lookup is static and fine; a variable
number of lookups (walking a lineage) is not. The DAEMON is explicitly exempt:
it is the one component allowed to hold state.

**What the sidecar is, in one word: a COPIER.** Stated at the `GetLiveWork`
landing — the sidecar is "a copier whose only recovery is cursors." It never
calls `GetLiveWork`; live-work reconciliation at session start is the SHIM's
job alone. The sidecar's entire recovery story is its cursor position.

**The constant-lookup resolutions the sidecar is permitted (and required) to do
at write time**, each audited against the principle and found static: a tool
return finds its call by `tool_use_id`; a skill document finds its call by
`sourceToolUseID`; a spawned agent finds its spawn; a re-announced start finds
its original instant by unit id; `AgentSuccess.answer` (the turn's last
response); the keep-alive rollback point; one pending ask per agent; a page of
an agent's children.

**Lineage is a DENORMALIZED ROOT, never a walk.** `KillTurn`'s transitive set
was once recorded as "the one bounded exception" and that exception is
WITHDRAWN. Instead every row is stamped at insert with its top-level ancestor:
one lookup of the parent's stored root, then stamp — an inductive invariant,
constant per insert, so "everything spawned under X" is one indexed query.

**Ruling: `update` frames carry DELTAS, never cumulative text.** The producer
"should only be returning what CHANGED"; coalescing or accumulating is the
daemon's choice, not the producer's. `AgentResponseUpdate` carries
`new_markdown`; `AgentThinkingUpdate` carries a text delta. NO offset on
either — a lost fragment is evident in the response and the terminal frame
carries the whole text, so the settled bubble self-corrects. Bash keeps its
offset because its spool has no settled whole to recover from.

## 2. WHAT THE SIDECAR READS — exactly four kinds of file

**The four kinds, all written by the agent binary** (verified at
`internal/discover/discover.go:83-91`): session transcripts, subagent
transcripts, workflow journals, and `tasks/*.output` spools — the last being
PER-TASK files, so ONLY background work has one.

**The transcript is strictly append-only on disk.** Established at the
RETRACT-1/2 ruling: the vendor's retraction fields (`supersedes`,
`retracted_message_uuids`) have zero observed occurrences across the
14,349-file corpus and are declared-only; nothing rewrites earlier records.

**FOREGROUND SHELL OUTPUT IS OBSERVABLE NOWHERE, verified against what the
sidecar can actually reach.** A foreground command's output exists in NO file
while it runs; it reaches the transcript only inside the completed tool result.
We cannot fix this from our side — the agent binary runs the process, so a
foreground spool would be a vendor change. Consequence: `AgentBash`'s `update`
arm is structurally DETACH-ONLY and its comment says so, because an integrator
would otherwise wait for frames that never come.

**`persistedOutputPath` is a POST-COMPLETION artifact — measured, not
inferred.** A foreground command producing 603,300 bytes over ~75s (inline cap
~30KB crossed within ~4s), polled at ~1s intervals across two independent runs:
NOTHING appeared in the `tool-results` directory while it ran; the file
materialized at process exit ALREADY AT ITS FINAL SIZE. Two limits on the
evidence: the write path lives in a compiled binary and was not inspected, so
this rests on external behaviour; and a first attempt was a FALSE NEGATIVE (a
shell-aliased `find` errored on the poller) and was correctly discarded.
RECORDED SO IT IS NEVER RE-PURCHASED: there is no incremental producer for
foreground shell output; the only live path is backgrounding.

## 3. DETACHED SHELL SPOOLS — the sidecar is the sole producer of the bytes

**THE OUTPUT PRODUCER IS OURS, NOT THE VENDOR'S — verified and load-bearing.**
The SDK exposes NO route returning a background task's output; the whole
28-method control surface was enumerated and nothing fetches it, and
`task_progress` for a shell carries `{total_tokens, tool_uses, duration_ms}` and
no bytes. Every byte of detached shell output therefore comes from the SIDECAR
tailing the spool file the agent binary writes
(`shim-sidecar/internal/handler/shell.go`), terminated by its `EXIT=<code>`
line. THE ACCOUNTABILITY COST, stated deliberately: a vendor route breaking
surfaces at the type surface on the next SDK upgrade, while a sidecar route
breaking is a file format or path we do not own changing underneath us, with no
type to fail against. The wire shape is unaffected — only who is accountable.

**The exit code may legitimately be absent** — `FeedShell.exit` is optional
precisely because the spool terminator may be missing; a non-zero exit is still
`completed` (the chip carries the verdict; "failure" is the reader's judgment),
and there is no `failed` outcome arm. On the frontend the spool is a
WHOLE-REPLACED TAIL: the daemon caps and replaces it whole, the client appends
nothing, the old offset-append machinery stays dead, and the "page older spool
inside the bubble" affordance is DROPPED (tail plus a composed omitted line).

**The background-shell PEEK is EXEMPT (TOOLIO-27).** A `Bash` call re-addressed
at a running shell's id, returning a snapshot of its output so far, is dropped
at the producer and never emitted as `AgentUnmodeled` — the same bytes already
reach the bubble via the spool. `TaskOutput` is exempt for the identical reason.

**`DetachedWorkOutput` — the spool's host-local PATH — is carried in
`conversation.v1` but frontend EXPECTED UNMAPPED** (the bubble already shows the
CONTENT); a bash SPILL path is appended by the daemon to the composed omitted
sentence ("1.2 MB more not shown · full output at /path"), shown not linked.

## 4. WORKFLOW JOURNALS AND PER-AGENT META — the sidecar's richest ingestion

**THE EXHAUSTIVE JOURNAL SURVEY.** 132 journals, 1,013 records, EXACTLY TWO
SHAPES: `{type: started, key, agentId}` and `{type: result, key, agentId,
result}`. NOTHING in a journal is non-agent-scoped — no phases, no run-level
events, no completion, no errors. So the workflow update arm covers the whole
of it and no category of update is missing.

**A LANDED CLAIM WAS WRONG, kept visible.** `detached_work.proto` asserted a
journal "states a step's label, its detail and its status separately." That is
FALSE against the real file, and the sidecar's own converter concedes its
rendering is lossy.

**PHASE GROUPING HAS NO PRODUCER.** A script declares its phases in `meta` and
each agent call may name one, but neither reaches any readable artifact: the
journal has no phase, and the per-agent meta does not carry one. So no phase
vocabulary exists on the wire.

**AN AGENT'S FIRST APPEARANCE IS ITS ANNOUNCEMENT, and it is a REAL RECORD.**
A workflow's agents are created BY THE SCRIPT, so no activity unit announces
them the way an agent-spawned subagent's `created_agent_id` does. The journal's
`started` record IS the announcement — THE SIDECAR CONVERTS A REAL RECORD
RATHER THAN SYNTHESIZING ANYTHING. This closes the one implicit layer: every
agent in the tree is announced and nothing is inferred from arrival order.

**THE PER-AGENT META FILE IS WHY THE ANNOUNCEMENT CAN BE POPULATED AT ALL.**
Every workflow agent has an `agent-<id>.meta.json` beside its transcript:
`agentType` and `spawnDepth` ALWAYS, plus `model`, `spawnedWithWorktree` and
`worktreePath` on 158 of 524. Verified separately: the agent's PROMPT is the
FIRST USER MESSAGE of its own transcript.

**THE SIDECAR CONSEQUENCE, stated at the entry.** Ingesting the per-agent
transcripts is NOT SUFFICIENT on its own — the meta file MUST be read alongside
each one, since it is the only source for type, model and worktree.

**`description` HAS NO PRODUCER on this path, so it is optional.** A script's
`agent()` call may label a step, but that label reaches no recoverable
artifact. Rather than synthesize one, the field states its absence and the
comment tells a consumer to fall back to the subagent type or the instruction's
opening.

**TWO SURVEY FACTS THAT REMAIN UNADDRESSED.** (1) 524 `started` records against
489 `result` records — 35 agents started and never produced one, and nothing in
a journal distinguishes "still running" from "died," so a container for such an
agent would stay open indefinitely. (2) NOTHING IN A JOURNAL EVER SAYS THE RUN
FINISHED — the workflow `success`/`failure` arms have no producer there at all;
their only possible source is the run leaving the vendor's live-background set.
That dependency is real and was not previously written down.

**A workflow is ALWAYS DETACHED** — the producer's only status values are
`async_launched` and `remote_launched`, with no synchronous form — and the run
stream is a STATELESS LEVEL: every frame carries the WHOLE subagent list
(REPLACE semantics), each entry a start plus a two-arm liveness oneof. The
shim/sidecar/store "should be very stupid": they say an agent EXISTS and the
daemon opens whatever connection it sees fit. The level is stored NOWHERE — it
is the join over the agent table.

**`run_id` is a RESUME handle, not a stream handle.** `taskId` is the background
task's identity (what the live set tracks, what a stop aims at); `runId` is the
local resume handle and ALSO THE RUN'S DIRECTORY NAME — which is how the
producer knows which run an ingested record came from. It is absent for a remote
run (the session URL serves that role), so it lives on the local placement arm.

**Workflow FRONTEND support is DEFERRED WHOLESALE** — no footer, no
`frontend.v1`, no daemon handling. Ingestion is unaffected; only the drawn
surface does not exist yet.

## 5. TRANSCRIPT INGESTION → conversation.v1

**THE PROTOCOL MODEL IS NODES, NOT A LOG.** Identity is PER THING, not per
arrival: a response holds one identity from its first fragment to its last,
growth is a re-send of that node, and a tool return is an ARM of its call, not a
separate entry. `ContentArriving` and standalone `ToolReturned` DIE; the
producer performs the fold (accumulating fragments, attaching a return to its
call, applying usage corrections); the daemon stops inventing identities for
records it has not seen.

**EVERY STREAM THAT CARRIES A UNIT OPENS WITH THAT UNIT'S `start`, repeating the
ORIGINAL instant** so a drawn clock does not reset when work moves between
streams; `start` means "this stream now carries this unit," NOT "work began,"
and a kind has an `update` arm IFF something produces growth for it. The
re-announced instant is RECOVERED FROM THE STORE rather than held in memory —
the store is indexable by unit id, so the second announcement reads the first;
the path this serves is a producer RESTART, not the ordinary detach.

**SKILL DOCUMENTS: `sourceToolUseID` is the structural link, and the current
sidecar correlation is unnecessary.** The real four-record sequence: the
`tool_use` block (id T); a `tool_result` for T whose content is the bare string
"Launching skill: <name>" — THE DOCUMENT IS NOT HERE; a `user` record with
`isMeta: true` and `sourceToolUseID: T` whose text block IS the document
verbatim; and an `attachment` carrying `command_permissions.allowedTools`. The
sidecar today correlates by keeping a skill-NAME map and matching what arrives
next (`convert/detached.go:65`) — POSITIONAL AND FRAGILE, where
`sourceToolUseID` makes the correlation structural and free. Recorded because
the fragile version is already in production and will look deliberate.

**NOTHING DELIMITS A SKILL'S SCOPE.** There is no skill-ended record, no
boundary marker, no producer statement about where a skill's influence stops —
verified by direct transcript inspection. So the unit settles on the DOCUMENT
(the producer holds the unit open across the two records) and there is no
nested arm; post-skill nesting is a presentation choice only.

**IDE DIAGNOSTICS join by ADJACENCY, not by id.** The vendor's diagnostics
record carries NO tool-call id, so the producer joins it by remembering ONE
last-write/edit-unit value — constant, and recorded as a producer note. The
report arrives AFTER the terminal as its own `diagnostics` arm; no frame ever
says "none are coming."

**`edited_text_file` attachments are DROPPED.** Measured across the corpus (470
notices: 450 for files the agent itself edited/wrote, 121 previously read, 20
neither; preceded by Bash 369 times, a fresh prompt 98). It is the vendor
refreshing its own MODEL's stale copy — cache-invalidation plumbing between the
vendor and its model, drawn nowhere even in the vendor's UI. "The user" in its
name is the vendor's authorship guess.

**The undocumented `mode` disk line is EXEMPT AS A RECORD (SESS-13).** Always
"normal," 2,669 observed, meaning unknown: the store's unparsed-residue arm
keeps the raw line and nothing else ever sees it.

**Model and permission mode ARE recorded in the transcript, even though the SDK
does not restore them.** Every `user` record carries `permissionMode` at top
level (4,722 occurrences in real sessions) and every `assistant` record carries
`model`; a resume with no options runs on the default model regardless. The
producer reads the last of each back and passes them. ROOT CAUSE of the
earlier error: reading "not restored" as "not recorded."

**The transcript's vendor session id is NOT authoritative.** `SessionStarted
.vendor_session_id` is THE RUNTIME'S OWN ANSWER; the transcript's divergent
spelling (observed differing in ~22% of records) stays producer-side and never
rides the field.

**Usage carriers are exactly three, recorded so nobody re-derives them.**
`assistant` messages → `message.usage` (13,057 occurrences, ONE PER API
RESPONSE — what the `AgentActivity` envelope field carries); `user` messages
carrying a subagent's completion → `toolUseResult.usage`/`totalTokens` (5
occurrences, the subagent's OWN consumption); `SDKThinkingTokensMessage` →
estimated tokens (zero occurrences observed). TOOL CALLS THEMSELVES CARRY NO
USAGE — a tool result is a `user` message.

**THE ONE-UNIT-PER-RESPONSE RULE, which the producer must honour.** Exactly ONE
unit per API response carries `usage` — the unit for the response's FIRST
content block, block order being deterministic; stamping every unit of a
three-tool-call message would make any consumer summing units over-count
threefold. Absence means "not the unit carrying its response's usage," never
"this cost nothing." (44% of assistant messages are pure tool calls and every
one carries usage, which is why the field sits on the envelope at all.)

**Converters must NOT route knowable material to the unknown arms.**
`UnsupportedBlock` and `AgentUnmodeled` are NOT FALLBACKS — they exist for
material genuinely unknowable in schema, and a recognizable kind found in one is
a PRODUCER DEFECT; the wave audits every `UnsupportedBlock` construction site in
the sidecar and shim against the vendor's block kinds.

**The FIDELITY PRINCIPLE: `conversation.v1` carries the vendor's fields even
when NO UI maps them,** marked EXPECTED UNMAPPED at the field (UI-relevance
gates `frontend.v1` only). It licenses NO relay of vendor identity spaces
(`uuid`, `message.id` stay producer-side) and no JSON-in-a-string.

**The EXEMPT SET is a third category beside modeled and unmodeled**: a known
vendor built-in the contract deliberately does not carry, whose calls are
DROPPED at the producer, never emitted as `AgentUnmodeled`, never tripping the
topbar's unmodeled warning. Members: TaskStop, TaskOutput, TaskGet, TaskList,
ToolSearch, NotebookEdit, the background-shell peek, REPL,
`skip_transcript`-marked ambient tasks (dropped ENTIRELY — ambient work is
accepted as invisible, and the vendor's level set still governs liveness so no
indicator wedges), and the MCP-resource / console-misc families
(ListMcpResources, ReadMcpResource, RefreshMcpTools, SendFeedback, ClaudeDesign,
Projects, ShowOnboardingRolePicker, ProposeSkills).

**The IDENTITY RULING.** Vendor uuids never cross the contract; the producer
translates where a unit exists. `activity_id` is producer-minted, one per unit
for its whole life, sourced from `tool_use_id` where the vendor has one and from
message id plus block index for text and reasoning. `agent_id`, `tool_use_id`,
`activity_id` and `TurnId` are FOUR SEPARATE SPACES: an agent is not its
spawning call, a unit of work is not the agent doing it, and a turn is neither.

**Producer notes corrected at the vetting pass:** `ArtifactOutput` is TYPED
(read its fields, never parse prose); grep's omitted figures are
producer-subtracted; hook duration and SPAWN DEPTH are producer-derived (spawn
depth off the per-agent meta file).

## 6. WRITING TO THE STORE — `WriteBatch`, spill, and the ack

**The store is a CONNECT SERVICE (`ShimStore`), and its only callers are the
shim and the sidecar.** It earned its process boundary precisely by having TWO
producer processes (contrast the daemon's own state, which has one and
therefore stays an in-process library).

**`WriteBatch` gives the write the ack the old socket never had.** Request:
producer plus an `EntryBatch`. SUCCESS MEANS DURABLE — records plus cursor
advance, ONE TRANSACTION; FAILURE MEANS NOTHING COMMITTED, so the producer's
spill holds and replays. The old UDS protocol acked nothing (a producer learned
failure only by connection death); the success arm is what lets a spill retire
batches on acknowledgment. Replay absorption is by `write_id` at the same arm.
`StoreEntryWrite` is DELETED — under Connect the rpc IS the envelope.

**The rename `WriteBatch` → `WriteSidecarBatch` was proposed and DECLINED:** the
shim also writes batches (stream-plane facts, spill replays); only
`cursor_advance` is sidecar-specific, and it already states its own absence.

**STANDING POLICY: the store is NUKED, NEVER MIGRATED.** No backfill, no
hydration, no schema migration; a durable-compatibility argument is NOT a reason
to keep a shape. An implementation agent must NOT write migration code and must
NOT preserve a message, field or arm on durable-compatibility grounds; where
existing contents are in the way, the store is DROPPED and recreated.

## 7. CURSORS — the sidecar's whole recovery story

**The high-level, settled first.** The cursor is invisible machinery whose UX
is: "after any crash or deploy, history has no gaps and no repeated messages."

**It rides the BATCH, not the entry.** One read position yields many entries;
it is a FILE BOOKMARK rather than a conversation fact; and STREAM-PLANE WRITES
HAVE NO FILE TO BE POSITIONED IN, which is why only the sidecar's batches carry
a cursor advance.

**File placement.** `CursorState`, `CursorQuery` and `CursorList` folded into
`write.proto` (one concern: how a producer writes and resumes), and `write`,
`entry` and `unsupported` later folded into a single `store.proto`.
`cursor.proto` is deleted.

**Deleted with the fold, each by name.** `OpenTaskState` — a timestamp that
could not name WHICH task; live-work recovery now reads the run rows.
`CursorList.open_tasks` and `open_tasks_authoritative` — an old-store
compatibility crutch, dead under nuke-never-migrate. `CursorQuery.file_id`
becomes `optional` (empty-means-all was a sentinel).

**`GetSidecarCursors` is the RECOVERY verb.** Request/response are
canonical-outcome shaped (`success { cursors } | failure { detail }`), on the
service under a RECOVERY section. EMPTY SUCCESS IS DOCUMENTED as the
fresh-store answer — not an error.

## 8. THE SHAPE THE SIDECAR WRITES INTO

**THE STORE WRITES conversation.v1 — the user's ruling, superseding a flat-log
leaning.** Rows are `conversation.v1` messages; NO record-granular vendor
vocabulary is persisted. The shim and the sidecar resolve each frame at write
time with a constant number of single lookups, and the store holds the resolved
frames. "The store persists whatever streams carry" STANDS.

**`StoreEntry` is the storage envelope**: a plane, a `write_id`, an
`upsert_key`, and a oneof selecting a `StoreAgentUpdate` or a
`conversation.v1.SessionUpdate`. `StoreAgentUpdate` carries the top-level agent
plus a oneof over a serveable page line, an unserved item, a bash run wrapper,
or a workflow run wrapper. `Entry`, `InternalEntry` and the old
`shim.v1.ExternalEntry` import are DELETED.

**PAGEABILITY IS DECIDED BY THE PRODUCER.** A page is the contiguous items of
ONE book; non-item frames (a bash update) must be STRUCTURALLY unable to appear
in a page. So a page line NAMES its book (`page_agent_id`), unserveable
material has no book, and run frames are not page lines at all.

**`upsert_key` is ONE OPAQUE PRODUCER-MINTED KEY.** The store holds one row per
key and a write supersedes it whole. The user's rule: "the store should have a
single place it looks for a given property, never multiple places for a given
column" — the mapping (TurnId, unit id, run id) is the SHIM'S and the
SIDECAR'S, never the store's. This is also what makes the store indexable by a
unit's id, which the re-announced-start recovery depends on.

**`StoreEntry` carries NO parent column at all** — the agent is read from
`AgentFrame.agent_id` or `AgentPrompt.agent`, and a `SessionUpdate` row is the
main agent's (superseding the earlier envelope-`parent` sketch, which duplicated
the frame's own field). `AgentPrompt` is the ONE form of a delivered prompt:
returned by the delivering rpc, persisted by the store, replayed by history;
`HistoryPrompt` is deleted as a respelling.

**`top_level` is the nearest NON-SYNC ancestor** — the turn's main agent or a
detached-work agent, never a sync subagent; equivalently, which live stream
carried the work. Denormalized at insert by the one-lookup induction, DOCUMENTED
UNSET when unresolvable (an unparsed record may name no agent), and NEVER used
for paging — only for kill scope and session scope.

**Keep-alive turns must be FIRST-CLASS IN THE STORE AS NEVER-SERVED**: indexed
so no page returns them and no activity is routed to the daemon, with a real
prompt rolling context back to just after the last real prompt. The producer
determines the keep-alive text under the hood; nothing keep-alive-shaped appears
on the wire at all.

## 9. STORE SCHEMA ARCHITECTURE (recorded as guidance, not proto)

**Four tables, each the ONE canonical home of one kind of fact.**

- `agent` — one row per `AgentId`, main included; `spawned_by` (an agent, a
  workflow, or nothing for main), unpacked spawn fields, `started_at`,
  `ended_at` (NULL = live).
- `workflow` — one row per run, keyed by the announced handle; `spawned_by`,
  origin unit, unpacked start fields, terminal once ended. THE SUBAGENT LEVEL IS
  NEVER STORED: it is the join, so agent liveness has exactly one home.
- `entry` — the page lines: a QUERYABLE SPINE (`upsert_key` PK, `book_agent_id`
  indexed and NULL for unserveable, `write_id` unique, plane, first-insert
  position) around a SERIALIZED frame the store never opens.
- `detached_work` — one row per detached non-agent run (bash today): handle,
  kind, origin unit, owner agent, unpacked latest state, `ended_at`.

**The columns-vs-blob line, the user's rule.** "Not ALL shapes need to map …
it's the queryable and joinable stuff." `agent`, `workflow` and `detached_work`
are UNPACKED to columns; `entry`'s frame stays SERIALIZED, because the activity
vocabulary is content and unpacking it would put every `conversation.v1` churn
into DDL for nothing the store ever queries. Mapping tests guard the unpacked
three.

**The frame's oneof IS the datalayer route.** `update` → the entry table, as a
page line of the agent's book. `success`/`failure` → BOTH entry (the stop
notice has no other source) AND the agent row's terminal columns, one
transaction. `detached_work` → the lifecycle table for its kind, NEVER a page
line (the spawning call is already one). Every write lands in exactly ONE
table, decided by its wire arm, plus that success/failure dual-write.

**Prompts and frames share ONE entry table** — one position space is what makes
"everything after the last real prompt" (the rollback) a range query; a prompts
"table" is a partial index, never a second position space. Every foreign key is
stamped at insert with ONE lookup, per the no-variable-state principle.

**Ordering is by the unit's FIRST INSERT, never its last write**, so a unit
settling mid-walk cannot teleport across a continuation; `StoreItemPointer` is
opaque, store-minted, and stable across upserts.

**The store deliberately tracks NOTHING about what it previously served.** Reads
are an OPEN (a bounded unary first page plus a watch token) and a WATCH (a pure
tail pinned to begin exactly after the page); every streamed line carries its
own pointer, and `known_through` is the CALLER's high-water mark (unset =
repaint, set = catch-up). A progress frame needs no store change at all — it is
an ordinary upsert of its unit's page line.

## 10. UNCONVERTIBLE AND RESIDUE MATERIAL

**The unserved oneof.** `StoreAgentUpdate.unserveable_frame` is replaced by a
`StoreUnservedItem` whose ARM IS WHY the item cannot be served: a keepalive
item, vendor-specific material, unknown material, or unparsed material. The
residue bodies carry over verbatim under `Store*` names.

**Consequence: the residue rides the SAME ENVELOPE as everything else** — one
`upsert_key` space, one write path — and the old separate `unconverted` table's
reason to exist goes with it.

**STILL HOMELESS, flagged not landed:** the old `source_record` kept-whole field
(a faithful conversion that was nonetheless LESS than the source). The user has
not said where or whether it returns.

## 11. BACKFILL

**`BackfillState` converts enum → oneof and becomes `HostBackfill`** with arms
none | pending | done | failed{detail}, riding the host workspace stream Emacs
watches. ITS KNOWN LIMITATION IS AN OWED DEBT, carried in the comment: A SIDECAR
READ ERROR THAT IS NOT A MALFORMED LINE MANIFESTS AS `PENDING` FOREVER — carried
forward explicitly rather than fixed, and still owed.

## 12. STANDING PROCESS RULES THAT BIND SIDECAR WORK

**EVIDENCE STANDARD: corpus absence is NOT deletion evidence.** "Not seeing it
in the transcripts could just be because I simply have not used the feature yet"
— absence from the personal corpus proves NON-USE, never NON-SUPPORT. Deletion
and no-producer verdicts require DOCUMENTATION-GRADE evidence (SDK doc comments,
official docs, release notes, research); corpus absence is supporting colour.

**THE SIDECAR IS A SECOND PRODUCER NEEDING ITS OWN VERIFICATION PASS** —
recorded on the vetting register at its opening. (Sibling caveat:
`fake-query.ts`, the shim's hand-written stand-in for the SDK's `query()`, can
only ever confirm our own reading; the fix is to build its scripts FROM captured
transcripts.)

**LANDED-COMMENT STANDARD.** Every landed message and field is documented for a
future integrator — domain meaning, why the shape exists, producer obligations,
integration gotchas — and NEVER references the development process.

**VALIDATION INVARIANT — unset non-optional fields are ILLEGAL, everywhere,
immediately.** A REQUEST carrying one is answered with an ERROR to the producer
at once (never "handled," never defaulted), so integration tests DETECT
producer gaps; an unset non-optional field on a RESPONSE or STREAM PUSH makes
the CONSUMER raise a loud error itself, there being no producer to answer.

**LOGGING INVARIANT.** DEBUG on every logical branch, warnings at WARNING,
errors at ERROR; integration/e2e orchestration enables >=WARNING BEFORE tests
run and PERUSES the logs EVEN WHEN TESTS PASS, remediating every warning to
zero (fixed or deliberately downgraded); remediation runs enable DEBUG to trace.

**PROTO→CODE MAPPING.** Every message has ONE core "base" function per language
where validation lives once (unset non-optional fields and required-semantics
empty strings are ERRORS; an unset oneof is an ERROR BY DEFAULT); every
non-primitive use site gets its own dedicated TESTABLE function delegating to
that base; primitives get no wrappers; the producer side is symmetric. No
class-per-message mandate — the anti-goal is a million unnamespaced
`Handle<A><B><C>` functions.

**THE SIX-ORCHESTRATOR PROTOCOL.** Five per-system orchestrators (the sidecar
is one) plus a lead; everyone reads the design record and their system's
implementation doc before any work. IMPLEMENTERS NEVER CHANGE PROTOBUFS — a
needed change is a request to their orchestrator, triaged to the lead, who
broadcasts PAUSE, lands the change, regenerates bindings, and broadcasts RESUME
carrying the NEW FOUNDATION COMMIT SHA; every request and ruling gets a line in
the design record.

**RECONCILIATION TEST RULE.** Any test referencing a DELETED or RESPELLED
symbol (one pointing at a genuinely different structure, not a mere rename) is
DELETED, never adapted; pure renames adapt mechanically. Only INTEGRATION specs
(into the subsystem's planning doc) and E2E specs (into the main doc) are
prescribed as replacement coverage — UNIT coverage is deliberately not
prescribed, because it falls out of the proto→code mapping convention.

## 13. STATUS AT THE FREEZE

- Design froze at `2d79f7501`; the Makefile's proto list went dynamic and Go/TS
  bindings regenerated from clean for all six packages.
- The SIDECAR reconciled GREEN in an isolated worktree and merged (all packages
  passing); its dead-code inventory, blockers and integration replacement specs
  are seeded into `docs/overhaul/sidecar.md`.
- Dead code the redesign stranded is NAMED WORK in that document — never left
  for discovery.
- The daemon alone remains a fanout subject (it was never repointed off the old
  packages); the other five systems are reconciled.
