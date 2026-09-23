# DESIGN: the figma→idl redesign of the agent-repl contract

## The problem, in the user's terms

`frontend.v1` is the figma→idl specification for the frontend: one file per UI
component, each component's props one message, resolved by the server and
rendered verbatim. `agentrepl.v1` is the API the webapp actually talks to.
Therefore `agentrepl.v1` should be COMPOSED of `frontend.v1` messages, passed
along component-dedicated endpoints — the sidebar talks to an endpoint that
ships `frontend.v1` sidebar messages, likewise the topbar, the footer, the feed
— not a god-endpoint that handles all shapes.

That is NOT how it works today. `service AgentRepl` is a pure command surface:
34 unary RPCs, 30 of them returning an empty `Success`, none returning a
component view. With the one multiplexed push stream deleted (see the
superseded record), no `frontend.v1` component data has any path to the webapp
at all. The composition rule is nowhere in force.

The redesign walks every surface, ONE `.proto` FILE AT A TIME, in the order
settled under "The iteration sequence, as walked" below (`conversation.v1`
first, then `frontend.v1`, `agentrepl.v1`, `shim.v1`, `store.v1`, `state.v1`). For `agentrepl.v1` the RPC inventory is hashed out explicitly first,
then the shapes.

## The record this supersedes

`proto/DESIGN-protobuf-surfaces.superseded.md` is the previous design record,
kept as a backup. Every decision it records STANDS unless an entry below
reopens it by name. In particular the following are settled and are NOT
re-litigated here:

- The six surfaces and the package-boundary-is-surface-boundary rule.
- Package names encode ownership, not routing; the drawn/called test between
  `frontend` and `agentrepl`.
- `agentrepl.v1` is a Connect service, not gRPC, because of the clients (an
  xwidget WebKit view and elisp).
- Every endpoint owns its request and response, one `endpoint_` file each.
- Every response is a two-arm `oneof { success; error }`; errors are typed
  messages in band, never status codes; the async boundary keeps
  `FailureCardView` as a push.
- One canonical form per message; import the encompassing message; re-spelling
  in its four forms is a defect; depth is not synthesis.
- The one multiplexed push stream is REVERSED: one server-streaming endpoint
  per UI component, with the two surviving ordering failures (typing-cut vs
  conversation-delta; cross-stream workspace references) accepted by the user
  as the price and owed a conventions-stage answer.

## The iteration sequence, as walked

**Settled.** Six stages, one per surface, walked ONE `.proto` FILE AT A TIME
within each. A later stage opens only after every earlier one is settled; any
stage may be reopened at any time, and reopening reopens every decision
downstream of it, enumerated by name.

1. **`conversation.v1`** — COMPLETE at `363323f1b`. Walked `message` →
   `tool_call` → `agent` → `user` → `content_blocks` → `context_cut` → `api`
   → `detached_work` → `session_command` (the per-concern file set after the
   folds and split recorded under Landed changes; `tokens` folded into `api`
   on its turn; originally `message` → `payloads` → `content` → `tokens` →
   `session_command`).
2. **`frontend.v1`** — `sidebar` → `topbar` → `footer` → `failure` → `feed`.
3. **`agentrepl.v1`**, from the empty service, in three sub-stages:
   - 3a. RPC inventory — every method by name and one-line purpose, no
     shapes, each ruled on one at a time.
   - 3b. Cross-endpoint conventions — including the invariants the deleted
     `service.proto` header carried and this record did not carry forward.
   - 3c. Per-endpoint shapes — one new `endpoint_*.proto` at a time.
4. **`shim.v1`** — `core` → `entry-delivery` → `message-page` → `external` →
   `bookkeeping`.
5. **`store.v1`** — `write` → `entry` → `cursor` → `unsupported`.
6. **`state.v1`** — `durable.proto`.

**Amendment (2026-08-22): a FRONTEND REMEDIATION PASS follows stage 6.**
The user: "after state, we've got a lot of remediation to do for frontend
… lots of new things have been made available for UX in our progress. Those
need to have reflections in frontend.proto, AND in the UI itself." Stage 2
is reopened BY NAME after stage 6, walked figma→idl with the ASCII drawing
agreed before each shape, covering (a) the owed repoints (Owed F) and (b)
every UX fact stages 1, 4 and 5 made available that `frontend.v1` does not
yet draw: the permission gate as its own unit and the question batch; a
subagent as a first-class view with its own feed, composer and held
prompts; the cold-context warning and the user's remediation choice; kill
and stop refusals that NAME live work; session diagnostics and degraded
windows; per-kind detached-work streams; history addressed by agent;
fragment-fed live bubbles. The UI implementation of each is fan-out work,
not contract work, and is carried to `/cross-system-fanout` with the drawings.

**Within a stage, files are walked TOP-DOWN BY CONTAINMENT.** The user's
second amendment, made after the sequence was first accepted: the file
declaring the highest-level (outermost) message goes first, then the files it
imports, down to the leaves, so a leaf is never designed before the message
that embeds it has said what it needs. The concrete order is MECHANICAL — read
off the intra-package import graph, importer before imported — and was
recomputed for stages 1, 4 and 5 above (`message.proto` imports `payloads`,
which imports `content` and `tokens`; `core`/`entry-delivery`/`message-page`
all import `external`, which imports `bookkeeping`; `write` imports `entry`
and `cursor`, `entry` imports `unsupported`). Stage 2's component files have
no containment relation, so their order stays as the user accepted it.
`tokens` before `session_command` was arbitrary between two leaves (moot once
`tokens` folded into `api`). This convention was added to the
`/create-or-update-protobufs` skill by a one-shot subagent — PR "walk one
package's proto files top-down by containment" (explanation-engine #7447,
MERGED). Two sibling conventions from this session landed the same way:
"add group-by-concern convention" (#7449, MERGED) and "add
adjacent-exclusivity enum-to-oneof test" (#7448, open — CI green, merge-queue
add pending a GitHub outage).

**Retracted by this amendment.** The `content.proto` increment sketched
before the amendment (ThinkingBlock as a two-arm oneof, ImageBlock's location
split into path/url arms, UnsupportedBlock.raw's exception stated at the
field, ToolCallBlock.arguments deferred to its own increment) is WITHDRAWN
unagreed and returns when `content.proto` comes up in the top-down order — by
then `payloads.proto` will have said what it needs from it. (It did return, as
`content_blocks.proto`, and landed at `3b55689e3` with the same three
decisions.)

**The amendments the user made, and why.** The first proposal put
`frontend.v1` before `conversation.v1`, and the file order within stages 3–6
was proposed by the orchestrator. The user moved `conversation.v1` FIRST
because the consumer-first order would have had every `frontend.v1` message
that embeds a `conversation.v1` type reopened by the leaf changing underneath
it — the leaf every other surface imports is settled before its importers.
The user also ordered the `agentrepl.v1` clean slate BEFORE the sequence was
settled (see the landed change below), so stage 3 begins from
`service AgentRepl {}` rather than from the old method table. The remaining
orders were accepted as proposed. The stage-1 transport decision from the
superseded record — Connect, one server-streaming endpoint per component —
is carried in, not re-walked; the sequence's stage 3 designs endpoints on
that transport.

## Core design principles

Project-wide principles the user stated as FACT. Each is an authority over
every later sketch: a shape that contradicts one is re-partitioned before it
is offered, or the contradiction is put to the user as a deliberate question.

### A subagent is handled EXACTLY as the turn is — by the daemon, and on the daemon↔shim API

**Stated 2026-08-22, in the user's terms.** "A core design principle here is
that subagents are internally handled exactly the same as turn from the
daemon's perspective and daemon<->shim api." The motivating UX: click a
subagent in the frontend and the webapp renders THAT subagent as the
first-class view — its feed, its composer, its held prompts — and submitting
a prompt routes to the subagent, with queued prompts presented exactly as they
are for the main agent from the main view.

**What it implies for the contract.**

- The write surface to a live agent is ONE type (`AgentInput { stop | answer
  | prompt }`), carried identically by the turn's update rpc and the
  subagent's; the requests differ ONLY in the address.
- Prompt QUEUING is daemon-side and kind-agnostic: a prompt to a busy
  subagent is held, classified, and delivered (interrupting if that is what
  delivery takes) by the same machinery as for the turn. `prompt` is
  therefore legal on both writes; "send a message to a subagent" is the
  daemon deciding when to deliver, not the vendor's steer-at-next-tool-round,
  which is the shim's mechanism underneath.
- The read surface is likewise one type: `AgentFrame` is the frame of the
  turn's stream and of a subagent's stream.
- The same holds for workflow and bash in principle — the same update API as
  in the turn band — and is MOOT in practice: a workflow is never carried by
  a turn, and a process has no input but stop.

**What it does NOT claim.** Nothing about bash or workflow becomes
agent-shaped; a process and a script are not agents.

**Reopened by name.** The DETACHED WORK inventory's `UpdateSubagent` as a
superset of `UpdateTurn` (withdrawn: they are the same type); the orchestrator's
reading that a main agent takes no prompt mid-turn while a subagent does (both
follow one rule, whichever it is).

**Open questions it raises, to settle at the DETACHED WORK shapes.** Whether
"no prompt to a busy agent; the daemon holds it" is the one rule for both;
whether a subagent's stream is addressed by `DetachedWorkId` or by `AgentId`;
and who delivers a held prompt to an IDLE subagent — today nothing starts a
turn on a subagent, so the queue has no drain there.

### Sessions and turns OUTLIVE the daemon — so SPAWNING, ATTACHING and ENDING are three decoupled acts

**Stated 2026-08-22, in the user's terms.** "These sessions/turns exist outside
of the daemon's lifetime, and therefore the spawning of them, and the
attaching to them, should be decoupled so the daemon can record the
session/turn identifiers in session state manager, such that on restart it
can simply run the corresponding Watch to handle."

**What it implies for the contract.**

- SPAWN is unary and returns (or adopts) an identity the daemon persists:
  `StartSession`, `StartTurn`. A Watch is never how something is created.
- ATTACH is a Watch stream that creates nothing and ENDS NOTHING: a consumer
  closing it — gracefully, which a shutting-down daemon must do, or abruptly,
  which the shim tolerates the same way — leaves the work running and the
  information accumulating. `WatchTurn` opens by REPLAYING from the turn's
  beginning, so attaching late or after a restart misses nothing.
- END is a Kill that refuses while work is live unless forced, and names
  what it killed: `KillSession` (the turn if open and EVERY live task,
  whichever turn spawned it) and `KillTurn` (the main agent and what THIS
  turn spawned, transitively, nothing else). The narrow stops
  (`UpdateTurn.stop`, `UpdateSubagent.stop`, `StopBash`) remain single-target
  interrupts.

**What it does NOT claim.** No `CloseXConnection` rpcs: cancelling the Watch
stream IS the graceful close in Connect, and a verb would duplicate the
transport's own act.

**Reopened by name.** 4b's "submit and the turn are ONE rpc" — the fused
`StartTurn` stream. The accepted-vs-attached gap it existed to eliminate is
closed by `WatchTurn`'s replay instead. Also the bounded-stream rule that "a
client-side close is a transport failure": for Watch streams a consumer
closing is a normal act; the rule now binds the PRODUCER side only (a stream
the producer ends without a terminal frame is the failure).

### The shim, store and sidecar hold NO VARIABLE-SIZE STATE — every observation costs a constant number of single indexed lookups

**Stated 2026-08-22, in the user's terms.** "We shouldn't be managing state
in shim/store/sidecar implicitly or explicitly … variable amounts of
information (stacks, queues, trees, etc) rather than static information (e.g.
doing a single parent id lookup is fine, that counts as static; having to do a
variable number of such lookups, e.g. to determine lineage, is not static)."
The daemon is NOT bound by this; it is the one component allowed to hold
state.

**What it settles for the store — the user's ruling, superseding the
orchestrator's flat-log leaning.** "The store should be writing
conversation.v1 to the database." The shim and the sidecar resolve each
`conversation.v1` frame at write time with a CONSTANT number of single
lookups (a tool return finds its call by `tool_use_id`; a skill document its
call by `sourceToolUseID`; a spawned agent its spawn; a re-announced start its
original instant by unit id) — which is static by the principle's own test —
and the store holds the resolved frames, indexed as below. Rows are
conversation.v1 messages; no record-granular vendor vocabulary is persisted.
The WatchSession entry's "the store persists whatever streams carry" STANDS.

**Indexing, the user's architectural notes (not schema).** Every stored
message carries its PARENT id and its TOP-LEVEL id, and both are indexed. The
top-level id is set AT INSERTION to the parent's stored top-level id — one
lookup — so by induction every descendant, however deep, carries the very
top level, and no insert ever walks more than one step. Prompts are stored
as messages in their own right, with parent and top-level recorded the same
way.

**PAGINATION HAS TWO KEYS — AGENT for lineage, PARENT ITEM for assembly —
and the page query is anchors-then-children ("exactly what I meant, and
important to get right").** A bare `agent_id = X limit N` falls into the trap
the user named: one vendor API response arrives as SEVERAL block units
(thinking, prose, tool calls), every one a row under X, so N rows is one
response's first blocks rather than N items. So every row carries two keys,
both static at insert: its AGENT (who did it — lineage), and its PARENT ITEM
— the response it is a block of, keyed by the response's ANCHOR unit (the
same first-block unit the usage rule singles out; one lookup by the vendor's
`message.id`). Prompts and anchors are PARENT-LESS. A page of agent X is: the
N most recent parent-less rows of X, then every row whose parent item is one
of those — two indexed queries, constant per row, no walk; the page reads as
"N responses, each with all its blocks", the shape `FeedAgent` already draws.
Containers (subagent, workflow run) remain doors: their contents are rows
under the CHILD's own agent id, never swept into X's page. `top_level` is
never used for paging. The "no collective noun" ruling stands: the response
is an index key in the store, never a message on the wire.

**THE THREE LINEAGE KEYS, the user's taxonomy.** (1) `top_level` — the
nearest NON-SYNC ancestor: the turn's main agent or a detached-work agent,
never a sync subagent; equivalently, which live stream carried the work.
Denormalized at insert by the same one-lookup induction as the root stamp:
top_level(X) = X when X is main-or-detached, else the parent's stored
top_level. It makes "kill a detached subagent's whole subtree" and
"attribute a sync subagent's work to its stream" one indexed query each.
(2) The PARENT AGENT — the immediate agent running the thing, sync subagents
included — read from the frame (`AgentFrame.agent_id`, `AgentPrompt.agent`),
indexed, never restated on the envelope; it is THE pagination key, so a sync
subagent is exactly ONE item in its parent's page (its spawn unit) while its
own constituents carry it as their parent agent. (3) `parent_item` — the
within-response anchor grouping, per the two-key entry above. An earlier
`top_level`-as-main-agent column is superseded: the main agent is constant
per logical-session scope (Owed H) and names no column. OPEN: whether
detachable spawn rows also carry a TURN stamp so `KillTurn`'s transitive
refusal list is one query, or the daemon resolves that from its own state.

**A PAGE IS "IMMEDIATE CHILDREN OF X", the user's definition, and nothing
has a tool call as parent.** A history page is the rows whose IMMEDIATE
parent is the addressed container, newest first; `top_level` is never used
for paging, only for kill scope and session scope. A unit's later frames
(a bash update, a tool return) are not children of anything — they are
upserts of the same unit — so the tree is agent → units. The one container
that is not an agent is a WORKFLOW RUN, whose agents the script created; so
the stored `parent` is a oneof { AgentId agent | DetachedWorkId workflow_run },
and `top_level` stays an `AgentId` (the main agent). SETTLED, the user: "workflows aren't loaded by page, the SUBAGENTS WITHIN
are" — `ReadHistory` stays addressed by `AgentId`; a run's agents are
announced on the run's stream and each is paged like any other agent; the
`workflow_run` parent arm exists only so those rows have a non-nil parent
for scope, and no query pages by it.

**Audit of the settled design against the principle.** Static already: tool
return → call, skill document → call, created agent → spawn, re-announced
start instant (Owed A), `AgentSuccess.answer` (last response of the turn),
keep-alive rollback point, a page of an agent's children (Owed H), one pending
ask per agent.

**Ruling 1 — lineage is a DENORMALIZED ROOT, the user's design.** `KillTurn`'s
transitive set was recorded as "the one bounded exception"; it is a lineage
walk and is withdrawn as an exception. Instead every row is stamped with its
top-level ancestor at insert: one lookup of the parent's stored root, then
stamp — an inductive invariant, constant per insert, so "everything spawned
under X" is one indexed query. VERIFIED at the type surface (KillTurn entry):
the vendor names a task's owning AGENT but never its owning TURN, so the stamp
is required, not optional.

**Ruling 2 — detachment is read from the EDGES; the LEVEL is relayed, never
diffed.** CONFIRMED at the type surface (`sdk.d.ts:2915`, contradicting the
user's name-reading of both as deltas): `task_started`/`task_notification` are
EDGE bookends and `background_tasks_changed` is "the full set … REPLACE
semantics … do not correlate it with the edge stream". A diff against the
previous level is a retained set, so the shim takes "entered" from
`task_started` and "left" from `task_notification` — one frame per message —
and relays the level verbatim as a session fact for the daemon, which may
hold the set. `KillSession`'s "every live task" is the vendor's own
`backgroundTasks()` answer, not a shim set. The no-edge-pairing rule binds the
daemon's INDICATOR only. VETTING: whether a foreground agent backgrounded by
Ctrl+B also fires a `task_started` edge (the level's doc names it; the edge's
does not).

**Ruling 3 — `update` frames carry DELTAS, never cumulative text.** The
user: "the shim should only be returning what CHANGED … how those deltas are
handled is up to the daemon (maybe it coalesces them, maybe it forwards them
as they arrive) — not the shim." This REOPENS the thinking/response entry's
"prose-so-far, not a delta" and retypes `AgentResponseUpdate` and
`AgentThinkingUpdate` on `AgentBashUpdate`'s existing delta-plus-offset
pattern; the start arms' comments, which justified themselves by the
cumulative rule, are reworded. The accumulator moves to the daemon, where
state is allowed.

## Landed changes
### CTRL-8 closes — seven command panels land, vendor-verified; the daemon owns the fill, the webapp owns the rendering

**The user's model.** frontend.v1 declares the panel SHAPES; "the daemon at
implementation time can determine how to fit to them" (which vendor route
fills each is a daemon implementation decision, recorded here as guidance,
never contract); and the WEBAPP has the agency to determine each panel's
rendering — the shapes carry resolved data, not layout.

**Vendor verification (Opus, SDK types + corpus + docs) trimmed the sketch's
13 arms to 7.** PRODUCIBLE and landed, each its own component file:
CostPanelView + UsagePanelView (label/value rows; source: the control
channel's usage answer — EXPERIMENTAL, see the vetting item), TodosPanelView
(pending|running|completed rows; filled from the DAEMON'S OWN tracker state,
no vendor route needed), AgentsPanelView (name + optional description;
supportedAgents), McpPanelView (name + connected|failed{detail}|needs_auth|
pending; mcpServerStatus), ContextPanelView (heading'd sections of
label/tokens/share/depth rows; getContextUsage), HelpPanelView (command +
optional description; supportedCommands — the full help text has no route).
SubmitPromptCommandPanel.panel grows arms 2-8.

**DROPPED as interactive-only/unproducible headlessly, each verified**:
/doctor, /hooks, /release-notes, /export (no SDK surface at all), /memory
(only paths recoverable, an editor otherwise), /permissions (no read route —
setPermissionMode is write-only). The sketched DoctorPanelView and
DocumentPanelView die with them. A dropped command that later gains a vendor
route is a new arm.

**No-panel commands restated**: /clear + /compact (the context-cut row is
the outcome), /model (topbar), and the act/flow commands (login, logout,
exit, resume, add-dir, config, output-style, vim, statusline,
terminal-setup, privacy-settings, rewind, bug).

### RESTRUCTURE batch 2 lands — trigger facts co-exist; the tracker forks its own vocabulary; live/stopped-work lists are CONNECTION facts; the session id names its space

**CTRL-15 ("(a) is fine").** AgentPermissionTrigger's either/or becomes
THREE INDEPENDENT OPTIONALS (blocked_path, ask_rule, note) — the vendor can
report all together and dropping any hid part of the reason.

**AGENT-5 ("they should be separate vocabularies. we are not beholden to
bad api names of the vendor").** The task tracker forks its own status set:
AgentTaskState keeps pending | running | completed | deleted and RETIRES
failed/killed/paused (tags 7-9) — a checklist item is a plan entry, not a
process; the footer checklist trims to the same three drawn treatments
(tags 4-6 retired). The vendor's shared background-task vocabulary no
longer leaks into the tracker.

**AGENT-7/8 (the user's design).** TurnLive/SessionLive.live_work and
TurnKilledForced/SessionKilledForced.stopped_work KEEP their id lists, with
producer notes landing the user's point: for every async item the daemon
holds a connection to the shim, so the live set is implicit in the OPEN
DETACHED-WORK STREAMS — the lists are filled from the shim's own tracking,
never from vendor records (whose signals carry counts, not ids).

**IDENT-18 ("a is fine").** SessionStarted.vendor_session_id is documented
as THE RUNTIME'S OWN ANSWER; the transcript's divergent spelling (observed
differing in ~22%% of records) stays shim-side and never rides the field.

**THE RESTRUCTURE BUCKET IS CLOSED.** All 17 survivors are landed, ruled,
or deferred; remaining before the design-complete gate: CTRL-8's
bespoke-per-command program and the SIMPLE-ADD wave's orchestrator review.

### RESTRUCTURE batch 1 rulings land: middle-slice reads; optional policy decider; the identity bucket is DEFERRED; the command enum and glob extent stand

**TOOLIO-6 ("let's fix that at schema level, and we should support at the
frontend level as well").** AgentReadSuccess.extent gains `range` (10):
AgentReadRange { contents (line-cut slice); first_line (1-based);
line_count; total_lines } — an offset read (observed ~6,000 times) is now
honestly representable. Frontend needs NO schema change: the tool bubble's
code output is daemon-resolved spans of whatever text was read, so the
daemon highlights the slice and words the composed omitted line ("lines
400-499 of 4,312") — the shipped text IS the text actually read, per the
user's requirement.

**TOOLFAIL-3 ("sounds good").** AgentPermissionDeniedByPolicy.decider goes
`optional` — the vendor may name no deciding component, and absence is a
legal answer, never an empty string; the value stays the verbatim vendor
word.

**IDENT-1 and its bucket (IDENT-2/3/4, STOP-9, RETRACT-5/8, COMPACT-4/6)
DEFERRED.** The identity ruling stands — vendor uuids never cross the
contract; the shim translates where a unit exists — and the permanently
uncarried remainder (which messages a compaction preserved; the ancestry
link across a compaction; vendor request/message ids on failure evidence)
is logged in figma-to-idl-redesign.deferred.md for a possible future PR.

**SESS-4 ("dont care... keep it").** SessionCommand stays a closed enum; no
plugin/genericization support; an unrecognized command simply is not a
session command.

**NOPROD-2 remainder ("dont want to support older vendor binary").** Glob's
required completeness choice STANDS — the modern binary always writes the
completeness fields, and pre-field CLI versions are explicitly
unsupported; the re-vet's optional-pair remediation is withdrawn.

### TOOLIO-27 and SESS-13 join the exempt set — the ruling batch closes

**TOOLIO-27 ("let's NOT model this").** The background-shell peek (a Bash
call re-addressed at a running shell's id, returning a snapshot of its
output so far) is EXEMPT: dropped at the shim, never AgentUnmodeled — the
same bytes already reach the bubble via the spool, the TaskOutput
precedent. The stale "no producer states an exit code" comment correction
is folded into the wave review's scope.

**SESS-13 ("let's forget it then").** The undocumented `mode` disk line
(always "normal", 2,669 observed, meaning unknown) is EXEMPT as a record:
the store's unparsed-residue arm keeps the raw line and nothing else ever
sees it.

**The 14-item ruling batch is CLOSED.** Every item is landed, recorded as a
drop, exempt, or routed to the wave review (USAGE-1's already-deleted money
fields; the exit-code comment). Remaining before the design-complete gate:
the 19 RESTRUCTURE gaps, CTRL-8's bespoke-per-command program, and the
SIMPLE-ADD wave's orchestrator review.

### MONEY LEAVES THE API (USAGE-1); IDENT-17/AGENT-12/CTRL-13 settled; USAGE-9 found wave-resolved

**USAGE-1, the user's ruling ("delete the cost field... It shouldnt be
represented in the API at all").** Money is deliberately not represented:
RunCost is DELETED, AgentRunAccounting.cost (tag 1 RETIRED) and
ModelUsage.cost (tag 5 RETIRED) with it. It had never reached frontend.v1 —
no surface draws a currency figure — so the deletion is conversation-only.
The rest of the wave's run accounting (durations, round trips, per-model
usage, denials) stands.

**IDENT-17 ("okay").** Replay's parent_agent_id does not reopen the
no-ancestry ruling: the store stamps each row's parent agent at insert, so
history reconstruction needs nothing from the replay surface. No change.

**AGENT-12, the user's ruling.** skip_transcript-marked (ambient/
housekeeping) tasks join the EXEMPT SET as records: the shim drops them
entirely — no detached-work announcement, no bubble, never AgentUnmodeled.
The earlier "rides the stream as a rendering property" ruling is
SUPERSEDED. Consequence accepted: ambient work is invisible on our
surfaces; the vendor's level set still governs liveness shim-side, so no
indicator wedges.

**CTRL-13, the user's ruling ("this is exempt").** The interrupt answer's
still_queued/cancelled lists are dropped: the daemon is the only queue, so
the vendor's queue functionally never holds anything of ours; the earlier
"defensive evidence, a fault to surface" reading is downgraded — no
SessionFault kind is added.

**USAGE-9 found ALREADY WAVE-RESOLVED.** TokenUsage.cache_miss
(TokenCacheMissDiagnostics: missed_input_tokens + the six-arm reason) landed
with the wave, so the vendor's cache-miss reason IS carried; the only open
remainder is whether the COLD GATE's drawn card states it (frontend
question, pending the user).

### Ruling batch: STOP-4's feed specializations land; RETRACT-10 recorded as a drop; four items found already wave-resolved

**STOP-4 ("you can take it home yourself").** ALL THREE conversation-side
arms turned out already wave-landed (ApiBillingError 11,
ApiOauthOrgNotAllowed 12, ApiModelNotFound 13); what was genuinely missing
was the frontend respell — FeedTurnEndedErrored gains billing_error /
model_not_found / oauth_org_not_allowed (14-16): no new UI family, just new
cause arms on the existing turn-terminal error card, per the user's rule
that specializations of existing feed error cards ride the existing route.

**RETRACT-10 recorded.** The vendor's cancel_async_message control verb is
DROPPED under the standing "the daemon is the only queue" ruling — the
vendor's own queue is never ours to manage, so a verb cancelling one of its
entries has no consumer; recorded against this verb by name.

**Found already resolved by the SIMPLE-ADD wave during this batch**:
USAGE-8 (TokenFallbackCredit), SESS-12 (fast-mode cooldown), AGENT-6
(SessionBackgroundTasks 19), and STOP-4's conversation half. USAGE-1
(money) is NOT resolved in the user's eyes — the wave LANDED RunCost while
the user's position is that money should not be in the API at all; routed
to the wave review.

### The DEFERRED metadocument opens — the new-family singles are parked, not judged

**The user's instruction.** Surfaces learned about but deliberately not
implemented now go to `figma-to-idl-redesign.deferred.md` — a third sibling
document beside the record and the vetting register — so a later PR starts
from evidence rather than re-surveying. Parked there: CTRL-6 (rate-limit
push, full quota picture), CTRL-12 (elicitation + user dialog blocking
kinds), CTRL-16 (the 13 unreachable control verbs — a rulings pass),
TOOLIO-23 (plan mode's product), IDENT-16 (tool-run summary,
uuid-addressed), USAGE-11/12/14 (/context, /cost, cost-behaviour
attribution), SESS-15 (memory recall's live channel).

**Also settled from the NOPROD research (three concurrent Opus researchers,
documentation-grade only).** NOPROD-3 CONFIRMED for our stack (the live
gate is the only in-band producer of prompts and allow outcomes; OTel
telemetry and rejoin replay are the only outside routes) — the landed
permission shapes stand. NOPROD-12c REFUTED (BashOutput.timedOutAfterMs
carries the configured limit at auto-backgrounding; backgroundedByUser
marks Ctrl+B) — the cause arms stand and gain producer notes: the shim
harvests cause from the BASH TOOL RESULT, never the task stream, which
carries no cause. NOPROD-6 CONFIRMED (async completion carries at most one
OPTIONAL total_tokens scalar vs the sync path's full four-field usage) —
remediation agreed: the split lives at AgentSubagentTotals' usage FIELD as
a two-arm oneof (full | total-only), the shared api.proto TokenUsage
untouched; LANDED: AgentSubagentTotals.usage becomes oneof { TokenUsage
full (sync) | AgentSubagentAsyncUsage total_only { optional total_tokens —
absence means unreported, never zero } (async) }, the set arm stating the
spawn path's honesty at the field.

### ATTACH-7 lands — the vendor's context-budget warning as a footer activity line

**What changed ("looks good").** conversation.v1: SessionUpdate gains
context_budget_warning (24) — SessionContextBudgetWarning { text verbatim;
the vendor composes it, no structured figure rides the record (1,519
observed total_tokens_reminder injections) }. frontend.v1:
FooterStatusActivity gains context_budget (10) { composed text } — the same
treatment as the rate-limit allowances line, per the user: "same thing."

### ATTACH-8's mode transitions are DROPPED — no topbar badges, no conversation arm

**The ruling ("let's just drop support for this. i dont care enough").** The
vendor's auto_mode/plan_mode/effort notices are EDGE records addressed to
the model — a shitty API to work around — while the underlying fact is one
exclusive selector already carried as CURRENT STATE by
SessionPermissionModeChanged (arm 7); frontend.v1 cares only about current
mode, never deltas. The drawn topbar badge cluster and the sketched
SessionModesChanged arm are both withdrawn unlanded; the effort level
remains carried per-response (AgentActivity.effort) and in the model
catalog, with no session-level badge. If mode surfacing is ever wanted, it
re-enters as a current-state projection of the permission mode + effort,
never as edge relay.

### ARTIFACT lands — a response-styled publish bubble; the LONG TAIL is fully dispositioned

**The drawing agreed ("looks good"), landed verbatim:**

```
+-FeedArtifact (purple, response-styled)-------------------+
| 📊 Merge Queue Report                        published  |   <- favicon emoji + title; state badge
| claude.ai/artifacts/abc123…                             |   <- the published URL, clickable
+---------------------------------------------------------+
```

**conversation.v1.** AgentActivity.item gains artifact (27). AgentArtifact
{ start { act publish { file_path; optional favicon/title; optional
updates_url (presence = a REDEPLOY); label/description/force EXPECTED
UNMAPPED } | list { limit/scope, all EXPECTED UNMAPPED — a quiet read } ;
started_at_ms } | success { published { url — PRODUCER NOTE: the vendor's
result is untyped prose, the shim extracts the URL; optional title } |
listed (empty on purpose) } | failure }. Evidence tier stated plainly: NO
Artifact call exists in this machine's corpus (the audit's 2 sightings were
aggregate) and the output is untyped, so the result shape rests on the
input types + doc surface — the URL-extraction producer note is the debt's
marker.

**frontend.v1.** FeedTurnActivity gains artifact (8): FeedArtifact
{ composed heading (favicon + title, filename fallback); publishing |
published { clickable url } | failed { composed reason } }. Only a publish
draws; a list produces no row. A redeploy upserts the same bubble.

**THE LONG TAIL (TOOLIO-28) + NOTEBOOKEDIT (TOOLIO-24) ARE NOW FULLY
DISPOSITIONED**: WebFetch/WebSearch modeled+drawn; Monitor modeled, footer
chip; ScheduleWakeup modeled, footer fallback state; Artifact modeled,
response bubble; TaskStop/TaskOutput/TaskGet/TaskList/ToolSearch/
NotebookEdit exempt.

### SCHEDULEWAKEUP lands — the footer's waiting · wakeup FALLBACK state with a client-ticked countdown

**The drawing agreed ("looks good then"), landed verbatim:**

```
│ waiting │ wakeup │ wakes in 4:32 · watching CI run │      │              │
  Status    SubStatus    StatusActivity (ticks every second)  Clock  Tokens
```

**The user's design.** A pending self-wakeup is a status+substatus footer
state, NOT a chip, and it is a FALLBACK: shown only when the footer would
otherwise read idle/done — any real status (thinking, merging, ...) wins —
with the fallback resolved DAEMON-SIDE ("the frontend should be a stupid
state renderer in this respect"). The activity line is a DURATION, so the
daemon ships the deadline INSTANT (wake_at_ms) and the client re-derives
the remaining time at a one-second tick — the clock convention pointed the
other way (countdown, not count-up).

**conversation.v1.** AgentActivity.item gains schedule_wakeup (26).
AgentScheduleWakeup { start { act schedule { delay_seconds; reason (the
vendor states it is shown to the user — the activity line draws it);
prompt (EXPECTED UNMAPPED) } | stop (exclusive by the vendor's own rule);
started_at_ms } | success { scheduled { wake_at_ms — THE drawn fact;
clamped_delay_seconds / was_clamped EXPECTED UNMAPPED } | stopped
{ cancelled_wakeups EXPECTED UNMAPPED } } | failure (shared vocabulary) }.

**frontend.v1.** FooterSubStatus gains the waiting family (8) — waiting's
FIRST sub-status — with the wakeup step carrying the daemon-side fallback
rule at the arm; FooterStatusActivity gains wakeup (9) { wake_at_ms;
optional reason }.

### MONITOR lands footer-only — the 👁 chip and panel; ToolSearch and NotebookEdit join the exempt set

**The drawing agreed ("design looks good"), landed verbatim:**

```
strip:  │ … status … │        ⚙ 2  ☑ 3/5  👁 2  $ 1 │   <- new 👁 monitors chip (live count),
                                                          presence-gated like the others
expanded (👁 selected):
├──────────────────────────────────────────────────────┤
│ 👁 tail e2e log for FAIL lines              12:04    │   <- description · runtime clock
│ 👁 watch PR #7565 checks        persistent  1:32:11  │   <- persistent watches marked; no
├──────────────────────────────────────────────────────┤      jump target (monitors have no bubble)
```

**What a monitor is.** The agent arms a background watcher (a shell command's
stdout lines or a WebSocket's frames) whose events wake it as ordinary turn
input; always detached, never blocking the turn.

**conversation.v1.** AgentActivity.item gains monitor (25); DetachableWork
gains the monitor arm (4) so the lifecycle rides the detached-work machinery
like a workflow's. AgentMonitor { start { description; lifetime deadline
{timeout_ms} | persistent (exclusive by the vendor's own rule); source
command | websocket (EXPECTED UNMAPPED — the description is the drawn
account); started_at_ms } | ended (no cause taxonomy claimed — the vendor
reports only leaving the live set) | failure (shared AgentToolFailure) }.
The vendor's taskId stays shim-side per the identity ruling (the
DetachedWorkId envelope is the handle).

**frontend.v1.** FooterLiveWorkChips gains optional monitors (👁 + count,
FooterChipMonitors on the existing chip pattern); FooterExpanded gains the
monitors panel (5): FooterMonitorRow { description; runtime clock (client
ticks from started_at_ms); optional persistent marker } — deliberately NOT
a jump target, monitors have no bubble; events are drawn nowhere special.

**Also settled ("we can consider toolsearch exempt"; "NotebookEdit is
useless, consider it exempt").** ToolSearch (deferred-tool schema loading —
vendor plumbing) and NotebookEdit (declared-only, would need fabricated
hunks to ride AgentEdit) join the EXEMPT SET: dropped at the shim, never
AgentUnmodeled, never the topbar warning.

**Queued next per the rulings**: ScheduleWakeup (footer status waiting ·
wakeup with a daemon-side fallback against idle/done and a client-ticked
countdown from a shipped deadline instant) and Artifact (a dedicated
response-styled feed bubble).

### THE EXEMPT SET is specified — known built-ins deliberately NOT modeled; TaskStop and TaskOutput join it; tasks go FOOTER-ONLY (FeedTask dies)

**The EXEMPT SET, the user's spec ("things that dont make it in the protos,
but also aren't 'unmodeled', they're known 'not going to model'").** A third
category beside modeled and unmodeled: a KNOWN vendor built-in the contract
deliberately does not carry. An exempt tool's calls are DROPPED at the shim —
they must NOT be emitted as AgentUnmodeled (that arm keeps meaning "genuinely
unknowable", its producer-defect stance intact) and must NOT trip the
topbar's unmodeled warning. The fidelity principle (vendor fields always
carried) governs fields OF MODELED tools; whole tools can be exempt. Members
so far: **TaskStop** (kills background work; the stopped work's own stream
already settles cancelled and its bubble/footer rows reflect that) and
**TaskOutput** (reads a background task's spool, observed shape
{retrieval_status, task{task_id, task_type, status, description, output,
exitCode}} — the spool is already drawn in the work's bubble). The sketched
AgentTaskStop/AgentTaskOutput arms are WITHDRAWN unlanded ("I dont like
updating the conversation protos with dead stuff").

**Tasks are FOOTER-ONLY (the user's simplification).** The tracker draws
solely as the footer's ☑ chip + expanded checklist; a single board-bubble
(tabs per task, keyed by the session's one implicit board — TaskListInput is
{} and no set id exists at the type surface) was drawn and set aside for
simplicity. Inventory mapping: TaskCreate = new row + denominator bump;
TaskUpdate/TodoWrite = row upsert (deleted removes); TaskGet/TaskList =
quiet reads, drawn nowhere; TaskStop/TaskOutput = exempt per above.

**What died.** FeedTurnActivity's task arm (tag 4 RETIRED) and the whole
FeedTask family (12 messages); FooterTaskRow.target (tag 1 RETIRED — no
bubble to jump to; agent and shell rows keep theirs). WatchFeed/GetFeedPage
simply stop carrying task rows; no endpoint shape changes.

**Also clarified on the way.** "Task" in TaskStop/TaskOutput is the vendor's
background-work sense (shells/subagents), NOT the tracker; tracker items are
stopped only by TaskUpdate's status. SETTLED ("yes exempt list"):
TaskGet and TaskList join the exempt set — quiet tracker reads, dropped at
the shim, never AgentUnmodeled. (TodoWrite stays MODELED: it is a producer
route of AgentTaskAct, a batch of tracker acts.)

### CORE PRINCIPLE: conversation.v1 carries the vendor's fields even when NO UI maps them — marked EXPECTED UNMAPPED at the field

**The principle, in the user's terms (2026-08-26).** "Let's always include
fields in the conversation protos, even if they are unsupported in the UI,
and just include comments for the protobufs explaining that they are
expected to be unmapped." conversation.v1 is a fidelity layer; UI-relevance
gates frontend.v1 only.

**Consequences.** A vendor field with no drawn consumer still lands, with an
EXPECTED UNMAPPED comment naming that no surface draws it; "nothing draws
it" is no longer a reason to drop from conversation.v1 (it remains one for
frontend.v1). **Reopens by name**: the recorded drops justified solely by
"nothing draws it" (grep appliedLimit/appliedOffset, glob durationMs, the
send-message pin.ref, bash structured fields, and kin) — to be re-judged as
a sweep at the audit's end, not silently.

**What it does NOT claim.** No relay of vendor identity spaces (uuid,
message.id stay shim-side), and no license for JSON-in-a-string — unmapped
fields are still fully typed.

### WebFetch and WebSearch land — typed arms end to end; the feed gains a LINKS output form and a clickable input line

**The drawings agreed first, landed verbatim per the new rule:**

```
+-FeedSimpleToolCall (WebFetch)----------------------------+
| WebFetch                                     ok 200 · 2s |
| fetch anthropic.com/news/claude-fable-5                  |  <- input line, now a hyperlink
| - - - - - - - - - - - - - - - - - - - - - - - - - - - -  |
| Claude Fable 5 and Mythos 5 are Anthropic's new...       |  <- existing text form, capped
+----------------------------------------------------------+

+-FeedSimpleToolCall (WebSearch)---------------------------+
| WebSearch                                        ok · 4s |
| search "claude agent sdk retraction"                     |
| - - - - - - - - - - - - - - - - - - - - - - - - - - - -  |
| Claude Agent SDK Reference — anthropic.com/docs/agent    |  <- NEW links form: title rows,
| Model refusals in the API — anthropic.com/refusals       |     each a clickable hyperlink
| 9 more not shown                                         |  <- composed omitted line
+----------------------------------------------------------+
```

**conversation.v1 (TOOLIO-17/18 discharged).** AgentActivity.item gains
web_fetch (23) and web_search (24), both on the family's standard
start | progress | success | failure shape with the shared AgentToolFailure
payload and AgentToolCallProgress beat. Fetch: target URL on every frame;
success carries the HTTP status (code + text — an error page is a SUCCESS,
the status is the badge), the vendor-rendered markdown result, and the
EXPECTED UNMAPPED trio bytes / duration_ms / artifact_read (the last =
the URL resolved to a Claude Artifact; declared-only at the type surface).
Search: query on every frame; success carries the heterogeneous result list
typed as link{title,url} | note{narration} entries in served order, plus
EXPECTED UNMAPPED search_count / duration_seconds. This entry is the first
application of the fidelity principle above.

**frontend.v1.** FeedToolCallInput gains optional link (the composed line
draws as a hyperlink — the user: all URLs are clickable); the returned
form oneof gains FeedToolCallLinksOutput (tag 8): link rows (text +
optional url — narration rows have none) + the shared composed omitted
line. WebFetch needs no new form: body = the text form, status = the badge.

**Dropped nowhere**: under the fidelity principle nothing from the two
vendor outputs was dropped; durations/bytes/counts ride conversation.v1
unmapped, and only frontend.v1 omits them (clocks tick from the start
instant).

**Next per the walk**: the "long tail" (TOOLIO-28: ToolSearch, Monitor,
ScheduleWakeup, TaskStop, TaskOutput, worktree/artifact tools) and
NotebookEdit (TOOLIO-24), each awaiting its ruling.

### RETRACT-1/2 DROPPED — the refusal-fallback retraction is not modeled

**The ruling ("lets forget about it").** The vendor's retraction fields
(`SDKAssistantMessage.supersedes`, `SDKModelRefusalFallbackMessage
.retracted_message_uuids`) are declared-only at the type surface, fire only
on the model-refusal fallback path, and have zero observed occurrences across
the 14,349-file corpus (the transcript is strictly append-only on disk); the
earlier batch-2 lean toward an AgentRetraction frame is withdrawn. If the
fallback path is ever observed emitting them, the question reopens with a
real producer in hand.

### ATTACH-3's out-of-band user edit is DROPPED — vendor cache plumbing, not a conversation fact

**The ruling ("this seems like something we dont want to support then"),
after the evidence settled what the record IS.** The `edited_text_file`
attachment fires when the vendor composes the agent's next API request: it
re-checks every file the agent has read or written this session and, for any
that changed on disk since, injects the fresh contents. Measured across the
corpus (470 notices): 450 were for files the agent itself had edited or
written, 121 previously read, 20 neither; the preceding record is whatever
tool result came next (Bash 369, a fresh user prompt 98) — never coupled to
Read/Write/Edit. Its payload is the file's fresh numbered text: it is the
vendor refreshing the MODEL's stale copy so edits do not land against stale
text (the same tracking behind the "File has been modified since read" Edit
error). "The user" is the vendor's authorship guess — a linter, a git
checkout, or another session trips it identically.

**Why dropped.** It is cache-invalidation plumbing between the vendor and its
own model, drawn nowhere even in the vendor's own UI; relaying it would model
the pipeline, not the domain. This closes the ATTACH-3 remainder (the
SessionUpdate-arm sketch and the later AgentContextInjected user_edit
grouping are both withdrawn — the grouping was refuted as
mechanism-not-semantics before the drop settled the question entirely).

### INJECTED CONTEXT lands — AgentContextInjected + the footer's LOADING status family

**The iteration, each turn the user's.** A feed card was drawn and
REJECTED for ephemerality; a footer 📎 chip + panel was proposed and
KILLED by the concurrency test — the user's criterion sharpened twice
(per-turn, then STRICTLY SIMULTANEOUS), and the honest answer is that an
injection has ZERO DURATION, so nothing is ever concurrently in flight
(per-turn multiplicity is real — 70 of 442 injecting turns carry ≥2, max
11 — but as residency, not activity). The user then ruled: a DEDICATED
status + sub-status, no extended-footer support.

**What changed.** conversation.v1: AgentActivity.item gains
`context_injected` (tag 22): AgentContextInjected { memory { path;
content } | skills { repeated { name; optional path; optional content } } }
— arrives whole, no lifecycle. frontend footer: FooterStatus gains
`loading` (11, MOMENTARY per the interrupted precedent); FooterSubStatus
gains the loading family { memory | invoked | discovered | listing }
(underscoreless per the user); FooterStatusActivity gains
`context_injected` { composed text — the specific item }. A moment reads
"loading · memory · webapp/CLAUDE.md", falling back on the next frame.

**Also dispatched**: the encompassing-message-with-path-comment sketch
convention to the skill (one-shot), after the user's formatting correction.

### IDE DIAGNOSTICS land as a POST-TERMINAL CONSEQUENCE ARM on write/edit — not an activity kind, not an update

**The iteration, each turn the user's.** (1) First sketched as a dedicated
activity kind; the user asked WHEN it happens (after Write/Edit, IDE
connected, new findings only) and ruled it belongs to the change's own
unit. (2) Success-only was rejected — the diagnostics arrive on a SEPARATE
record, so the unit demonstrably has post-result composition. (3) An
update arm was considered (with a one-record lookahead to keep success
final) and REFINED by the user: `update` keeps its pre-terminal meaning;
the report is its own `diagnostics` ARM arriving AFTER the terminal — a
CONSEQUENCE frame the consumer applies to the settled card; no frame ever
says "none are coming".

**What changed.** AgentWrite.result and AgentEdit.result each gain
`AgentDiagnosticsReport diagnostics = 5`; the report family lands
(per-file findings: severity enum (LSP's closed scalar set), message,
optional source/code, start/end lines — character precision deliberately
dropped). PRODUCER NOTE stated at the family banner: the vendor's record
carries NO tool-call id — the join is by ADJACENCY, one remembered
last-write/edit-unit value in the shim, constant and recorded. frontend:
FeedToolCallReturned gains optional diagnostics { composed lines }, drawn
below the output on a later re-push of the settled card.

**Also settled in this exchange (the user's formatting rule, saved to
memory):** sketches always show the ENCOMPASSING message with a path
comment; fields may be elided within it; never floating fields.

### The HOOK FAMILY lands (orchestrator's hand) — quiet by default, loud on refusal; stop hooks never touch the turn terminal

**conversation.v1.** AgentActivity.item gains `hook` (tag 21): AgentHook
{ start { hook_name; AgentHookEvent (the vendor's 31 HOOK_EVENTS literals
verbatim, a genuine closed scalar set); optional gated_call; started_at }
| succeeded { command; exit_code; duration_ms; optional output
{stdout, stderr} } | blocking_error { command; blocking_text } |
non_blocking_error { command; exit_code; duration_ms; optional output } |
cancelled }.

**The UI ("sounds great", drawing agreed).** A SUCCEEDED hook draws
NOTHING (35k of them — quiet automation stays quiet; an audit view is a
later panel, never feed noise); live runs fill the footer's existing
hook{name} activity; FAILURES draw: frontend FeedTurnActivity gains `hook`
→ FeedHook { headline; optional gated_call (FeedId link to the refused
call's card); blocked { the hook's refusal text — loud } | failed
{ exit chip; capped output } }.

**The user's correction, superseding the sketch's roll-up.** Stop hooks
fire AFTER a stop and never determine how the turn ended — "that's the
turn api's responsibility (and we should trust that it handles it)". No
FeedTurnEnded hook arm exists; the AgentStopHookSummary payload idea is
WITHDRAWN; benign stop summaries get no frame (the per-hook units already
carry each run). The SIMPLE-ADD wave's AgentFailure stop_hook_prevented/
hook_stopped arms are on the orchestrator-review list under the same
scrutiny.

### PROCESS RULING: subagents never design protobufs; the new-family dispatch killed; the SIMPLE-ADD wave owes orchestrator review

**The user's ruling ("i dont want subagents to be designing protobufs").**
Subagents collect, classify, enumerate and research; every SHAPE is the
orchestrator's sketch, the user's agreement, the orchestrator's landing —
which is the skill's own "Who writes, and who never writes", now applied
to remediation waves too. The in-flight new-family agent was KILLED before
it edited anything; its six settled families (hooks, diagnostics, injected
context, mode transitions, retraction, new tool arms) return through the
ordinary sketch loop. The landed SIMPLE-ADD wave (`c7814df86`) stands
pending a full ORCHESTRATOR REVIEW of its text and judgment calls, brought
to the user as review items and amended by hand.

### The documentation re-vet lands: 11 of 13 no-producer claims REFUTED; batch-2 rulings so far

**The re-vet (Opus, SDK doc surface + official docs), vindicating the
corrected evidence standard.** KEEP, refuted as no-producer: Grep (real
default-allowed built-in; corpus absence was tool-deferral), Glob's
structured output ("absent on results persisted by CLI versions predating
this field" — the corpus predates the field; mirror the SDK's optional
pair, not a required extent), bash isImage and interrupted, subagent
models_used (multi-entry is the field's documented purpose),
requested_name and note (SendMessage addressability; task progress
summary), all five workflow fields (resumeFromRunId, sessionUrl, warning,
script-rejection error, TaskStop kill), the question note/free-text
distinction (declared verbatim — the owed verification is DISCHARGED),
clear-keeps-session-id (SDKConversationResetMessage), MCP error text (on
Query.mcpServerStatus(), not init's impoverished copy — wire the shim
there), DetachedCauseByUser (TaskStop), permission vocabulary (the live
gate). RE-COMMENT as SHIM-authored, not delete: SessionIdentityRotated
.reason; SessionColdLapsed cache TTL (derive the tier from observed
ephemeral_5m/1h; prefer a tier enum over ms). SURVIVING QUESTIONS, two:
NOPROD-6 (async completions carry one scalar vs sync's full usage — shape
ruling pending) and NOPROD-12c (detached timeout has no vendor surface —
keep only as a shim-imposed deadline, pending). Methodological note
recorded for the register: three claims failed by inspecting the
impoverished copy of a structure (init.mcp_servers vs mcpServerStatus();
transcript vs live gate; Messages-API schema vs toolUseResult).

**Batch-2 rulings so far.** HOOKS: full hook activity family (start/
settled with outcomes incl. blocking; event vocabulary; stop roll-up) —
the footer's hook activity gains its producer. ATTACH-2: an LSP
diagnostics activity kind (the diagnostics card). ATTACH-3: a SessionUpdate
arm (per-workspace by construction — the user's scoping question answered:
one session = one workspace), drawn as a feed notice. ATTACH-5: COVERED by
the landed UserPromptProvenance queued arm — no new shape.

### EVIDENCE STANDARD CORRECTED: corpus absence is NOT deletion evidence; the SIMPLE-ADD wave lands; batch judging opens

**The user's correction, a standing rule.** "Not seeing it in the
transcripts could just be because I simply have not used the feature yet"
— absence from the personal corpus proves NON-USE, never NON-SUPPORT.
Deletion/no-producer verdicts require DOCUMENTATION-GRADE evidence: the
SDK's own doc comments, official docs, release notes, web research —
corpus absence demoted to supporting color. A research subagent is
re-vetting every NOPROD claim on that standard; the batch-1 picks for
NOPROD-1 (delete Grep) and NOPROD-2 (retype Glob) are SUSPENDED pending
it, since they rested on the rejected basis. Batch-1 rulings that stand:
NOPROD-4 KEEP (the user plans to use bash image output), NOPROD-5 KEEP
(the live shim is the interrupt's producer), NOPROD-7 KEEP repeated,
NOPROD-8 KEEP; NOPROD-3 kicked for discussion; NOPROD-6 under discussion
(the async completion record carries one scalar; the daemon can derive the
breakdown by summing the frames it already holds).

**The SIMPLE-ADD wave landed (`c7814df86`, Opus subagent, one commit,
whole tree compiles).** 21 remediation groups: the vendor handshake block
on SessionStarted; sixteen turn-stop error arms; refusal detail; read
extent arms (image/pdf/notebook/split/unchanged); the shared
AgentToolFailure payload filling all twelve empty *Failure{} arms; run
accounting (cost in micro_usd, per-model usage, latency) on both
terminals; sixteen new SessionUpdate arms; skill/plugin attribution;
settle instants; prompt provenance on UserSaid; compaction detail + new
cut arms; bash termination/sandbox/spill; detached-work output path;
subagent isolation/retry/meta fields; workflow phases and totals; task
tracker deleted/blocked/rejected; interrupt provenance arms; permission
decider-absent arm; model capabilities; API class arms + synthesized-
notice authorship; cache-miss diagnostics; the user-content file block.
Its judgment calls and vendor-identity skips are in the agent report
(notably: backgrounding routed to AgentSuccess.backgrounded; money as
int64 micro_usd; uuid-typed parts of STOP-9/IDENT-3/IDENT-4/RETRACT-5/
COMPACT-4/RETRACT-8 skipped for the hard bucket; IDENT-17 parent_agent_id
skipped as contradicting the ancestry ruling). ORCHESTRATOR REVIEW of the
landed text against the conventions is owed before the design-complete
gate.

### The RECONCILIATION PRESCRIPTION settled — per-subsystem agents; deleted-or-respelled tests DIE; the main agent architects the replacement coverage

**The user's prescription (dispatched to the skill by one-shot).**

- Reconciliation runs PER SUBSYSTEM: bindings regenerated once, then one
  adaptation agent per system (elisp, daemon, shim, store, sidecar,
  webapp) in isolated worktrees, merged as each passes.
- THE TEST RULE: any test referencing a DELETED symbol, or a RESPELLED one
  (points at a genuinely different structure — not a mere rename), is
  DELETED, never adapted. Pure renames adapt mechanically. An adaptation
  that would require deciding what behavior should now be is a surfaced
  gap, not an adaptation.
- REPLACEMENT COVERAGE IS ARCHITECTURE WORK, THE MAIN AGENT'S ONLY ("main
  agent is smarter"): for every deleted test the orchestrator determines
  the REPLACEMENT TESTS TO BE ADDED and records ONLY those forward specs —
  the deletion itself never appears in any document. Routing: unit/
  integration replacements → that subsystem's SUBSYSTEM-SPECIFIC
  IMPLEMENTATION PLANNING DOCUMENT (the fanout subagent's reference);
  e2e replacements → the MAIN IMPLEMENTATION DOCUMENT (the orchestrator's
  reference). These two document kinds are NEW — introduced by this
  prescription.

### AUDIT JUDGING OPENS — finding 1 lands: the STOP TAXONOMY gets its conversation.v1 arms

**The judging protocol.** Findings one at a time, five-answer test, contract
changes as ordinary increments — walked in load-bearing order. IN PARALLEL,
an Opus (xhigh) subagent is re-auditing EVERY vendor surface (SDK types,
real JSONL, journals/meta/spools) against the CURRENT conversation.v1,
verifying the stale A–E reports and consolidating every no-home-no-reason
gap into audit/GAPS-consolidated.md — collection only, no remediation; the
judging continues from that document.

**Finding 1 ("yes, we need to add those fields").** The vendor's stop facts
(stop_reason, stop_details) had no producer arms while FeedTurnEnded now
draws max-tokens/refusal notices. LANDED: AgentResponseFailureReason gains
{ max_tokens | refused { optional vendor explanation } |
context_window_exceeded | aborted }. Deliberately WITHOUT arms, stated at
the message: pause_turn (the vendor resumes it itself), compaction (the
context cut is that fact's home), stop_sequence (unused). NO AgentFailure
change: turn-level max-tokens/refusal renderings resolve from the LAST
response's failure reason — one home for the fact.

### Session diagnostics join the topbar dropdown — the FRONTEND REMEDIATION PASS IS COMPLETE

**What changed ("looks good").** TopbarWarning gains two detail arms:
session_fault { component; detail — verbatim from the shim } and
degraded_window { component; reason; began_at_ms; oneof extent { open |
closed { ended_at_ms; dropped_count } } } — one warning per fault/window;
the daemon pulls GetSessionDiagnostics at its own cadence and a healthy
pull retracts on the next push. This closes the pass inventory's
session-diagnostics item; the kill/stop-refusal item closed as
wave-derivation guidance (a live-work refusal arm's derivation NAMES the
work where a payload is wanted; the footer's standing display is the
default naming).

**THE FRONTEND REMEDIATION PASS (the reopened stage 2) IS COMPLETE.** What
follows per the settled sequence: the AUDIT JUDGING (reports A–G, one
finding at a time — the user's "at the very end" deferral now due), then
the design-complete gate with the consequences recap, the vetting
register, reconciliation, and the /cross-system-fanout handoff.

### Held prompts BLOCK a close — the quiet set is complete

**The user's ruling ("held prompts should absolutely block a close").** A
held prompt is undelivered user intent; a close may never silently discard
it. The closable condition is now: no turn in flight, no live async work,
no held prompts. The user clears a hold via the tray's existing verbs
(release or drop). Stated at CloseWorkspace's endpoint; the footer's
close-blocked reasons line names held prompts like any other blocker. A
standing cold gate, the task tracker's contents, and a parked (shim-less)
session remain NON-blocking.

### HIBERNATION'S MACHINERY COLLAPSES to an idle-shutdown sweep; the last contract trace neutralizes; the removal plan is named wave work

**The user's rulings, completing the hibernation arc.** (1) ReviveWorkspace
already died; OpenWorkspace just opens, reviving under the hood when
needed. (2) Hibernation's PURPOSE examined and affirmed: each open
workspace is a shim + vendor binary (real memory) and keepalive spend, so
an idle-cutoff shutdown is useful — but it is "entirely an implementation
detail of the daemon to save memory": NO frontend (webapp or emacs)
knowledge, NO daemon API surface. (3) The machinery COLLAPSES accordingly:
what survives is the idle-cutoff sweep (shim stopped past cutoff; at most
one registry bit) plus the ORDINARY resume path — sessions outlive
processes by design (StartSession.resume), the host stream already says
shim_attached=false, and the cost story is the cold gate. The leases,
revival modes/holds/gates, hibernation states and bootsweep hibernation
verdicts existed to manage an explicit-revival distinction the contract no
longer has: they move from KEEP to DELETE in the plan.

**The last contract trace, neutralized.** HeldPromptRevivalHold →
HeldPromptSessionStartingHold (tag 11, arm renamed `session_starting`):
"the session is still coming up" — a cold resume, a chosen compaction
landing — with the same no-classifier/no-force/loud-drop semantics. No
frontend word says hibernation anywhere now.

**THE REMOVAL PLAN (named implementation-wave work; full inventory in the
enumeration agent's report, summarized here).** ELISP: teal tab treatment,
💤 glyph, roster label+decoders, RENDER_STATE/CONNECTIVITY hibernated
decoders, the hibernate command + SPC o z, the RPC client path and error
copy, open-progress arm, ~8 test blocks. DAEMON: the two wire states, the
topbar teal arm, the revival gate and its pushes, the Hibernate/Revive
handlers and user-forced entrypoints, the SessionHibernated failure-kind
encode (refusal retired outright — prompting a parked workspace just
works), six e2e suites — PLUS, per ruling (3), the leases/holds/gates
machinery, keeping only the idle sweep + resume. WEBAPP: hibernation.ts
whole (741 lines), the gate DOM/CSS, adapter/store/command arms, teal
variables. Ambiguity dispositions: SPC o c bring-up behavior kept with
generalized wording; the frontend-state live-branch guard dies with the
decoder; teal dies (palette contracts to five, cross-system);
log/diagnostic prose kept and reworded lazily.

### HIBERNATION LEAVES THE CONTRACT ENTIRELY — an agentrepl implementation detail; revival is implicit

**The user's ruling.** "I'm not sure that emacs should know about
hibernation at all… all the useful information that's practically cared
about by the user is implicit in the compaction warning" — a hibernated
workspace only matters because keepalives are gone, which only matters
because of the context cache, which the COLD GATE already fully surfaces.

**What died (all four sites).** ReviveWorkspace (endpoint + rpc — revival
becomes IMPLICIT: a prompt to a parked workspace revives it under the
hood, cost story = the cold gate); the host stream's `hibernated` standing
arm and the whole HostHibernation* family (parked/reviving; the
idle-cutoff/forced/cache-expired causes); FooterStatusAsleep; and
RosterRowStatusHibernated.

**The committed consequence, stated plainly.** The frontend can no longer
distinguish a parked workspace from an idle one anywhere: a parked session
presents on the host stream as `live` with `shim_attached = false` ("the
session exists and serves on demand" — the composer stays open, typing
revives), the footer shows `idle`, the roster shows the ordinary dot. The
distinction surfaces only as the cold gate, when it has a cost. The WSM's
hibernation MACHINERY (idle cutoff, parking, revival) is untouched — it
just stopped being a wire fact.

**Detour queued next (the user's ask): the ELISP hibernation integrations
sweep** — enumerate and remove every Emacs-side hibernation surface (the
teal tab-bar treatment for hibernated workspaces, any hibernate/revive
commands and API plumbing) so the implementation matches the contract.

### CLOSE / KILL / NUKE land as the workspace verb triad; HibernateWorkspace DIES; the footer's close-blocked treatment lands

**The settled semantics (the user's, across the iteration).** THREE verbs,
all with Emacs commands that are THIN WRAPPERS — send the request, await
the daemon's ack, then tear the tab down; every piece of real machinery is
the daemon's:

- CloseWorkspace (SPC j x) — the USER's close, a VIEW act: fast ack, tab
  gone, the daemon↔shim session UNTOUCHED (keepalives continue; the
  workspace is merely unviewed). REQUIRES QUIET: a busy workspace refuses
  and the refusal manifests in the FOOTER — status `closing`, sub-status
  `close blocked`, and the daemon's composed plain-English reasons as the
  activity line ("a turn is in flight; 2 subagents and a shell are
  running"). The response carries only the `blocked` cause arm; the footer
  owns the reasons. The user waits or interrupts the work normally.
- KillWorkspace — the BIG RED BUTTON: forced session death (connections
  AND the shim itself; the shim's forced kill underneath). Never blocks,
  never warns, checks nothing. Worktree and branch survive. This is the
  better-named replacement for the StopAgentShim idea, which never landed.
- NukeWorkspace — DATA DESTRUCTION: kill first if live, then delete the
  worktree and branch. "Nuke" is reserved for exactly the verb that
  destroys data — consistent with the elisp rename (the old emacs "nuke"
  never destroyed data).

**HibernateWorkspace is DELETED** (endpoint + rpc): hibernation is an
INACTIVITY policy, never an explicit act — "i don't think it should ever
be done explicitly." ReviveWorkspace STANDS (waking a parked workspace is
explicit). The WSM's hibernation machinery is untouched; only the verb
dies.

**Also settled on the way (recorded at the earlier entry, refined here).**
Attach/detach of daemon↔shim streams is never a semantic act; no verb
closes just a connection. The earlier stage-3 reading of CloseWorkspace as
"the only teardown" is SUPERSEDED by this triad.

**Footer changes.** FooterStatus + `closing` (10); FooterSubStatusClosing
{ blocked } family; FooterStatusActivityCloseBlocked { composed text } —
the footer's own composed-text convention (the cold gate's data-not-prose
ruling is component-bounded and does not apply here).

**Open still**: whether HELD PROMPTS block a close (the tray question) —
unruled; and the sub-status family anticipates a graceful-drain step if
one is ever wanted.

### CLOSE vs KILL settled — semantics, vocabulary, and where the refusal surfaces

**The user's rulings, stated plainly.** (1) Closing a workspace with active
work is a USER-FACING ERROR, and that error MANIFESTS IN THE FOOTER (shape
to be designed). (2) Killing a turn/session in flight KILLS IT — never an
error; kill is the stated intent, so no refusal exists on that path. (3)
The vocabulary discrepancy is settled by renaming DOOM-SPEAK to match
agent-repl-speak: doom's "kill" (remove the workspace from the editor —
tab-bar, buffers, editor state; worktree survives) becomes "close"; doom's
"nuke" (the harder destroy) becomes "kill". An Opus subagent is executing
the elisp-only symbol rename in modules/app/agent-repl (Go/TS/protos
untouched — their vocabulary was already right).

**The distinction that motivated it (from the compare/contrast).** Kill
targets WORK the user is looking at — naming is free, refusal is senseless.
Close targets the CONTAINER, and the collision with live work is
incidental — the refusal's job is to redirect attention to work the user
was not thinking about, which is why it deserves a designed surface rather
than a bare error.

**Open, being designed next**: when exactly a workspace is closable; how
the close refusal surfaces in the footer; the Emacs close UX.

### The COLD-CONTEXT GATE lands as a FEED ROW; the compaction scope becomes ONE ENUM, four→three

**The drawing agreed first**: the gate across the feed's tail, composer
owned while standing — headline, cost, parenthetical, and the three buttons
with the compact submenu (model + type).

**The iteration, each ruling the user's.** (1) No `none` arm — absence is
the none (the earlier two-arm none oneof died; then ColdGateStanding
flattened away with it). (2) DATA, NOT PROSE: the daemon serves raw facts
(token count, last-request instant, the model) and the CLIENT owns wording,
formatting and ticking — a recorded departure from the daemon-formats
precedent, bounded to this component because its facts are counts and
instants, not resolved presentation. (3) NOT its own component/stream — a
ROW beside the meta kinds (FeedRow.cold_gate = 13), which also buys cold
paint from pages and a RESOLVED trace in history: FeedColdGate { standing
{ context_tokens; last_request; model; compact menu } | resolved { at_ms;
pay | clear | compact{model} } }. (4) The compaction TYPE is selectable in
the menu, and it is an ENUM — the orchestrator's presence-per-option
modeling was rejected ("you can only select one scope"); a compaction scope
is a genuine closed scalar set (the convention's own non-state carve-out),
so ONE canonical `conversation.v1.SessionCompactScope { ALL | PROMPTS |
RESPONSES }` is declared at the conversation level and imported by the menu
(`repeated scopes` = the offered radios) and the verb — never respelled.

**The conversation.v1 REOPEN, confirmed.** SessionColdCompact's four-arm
scope oneof (full | prompts_only | responses_only | prompts_and_responses)
reshapes to the enum and PROMPTS_AND_RESPONSES DIES, per the user's
three-type ruling ("the types being all/prompts/responses").

**The verb.** NEW endpoint_answer_cold_gate.proto: { WorkspaceRef; FeedId
gate (echoed as served); pay | clear | compact { AgentModel echoed from the
menu; SessionCompactScope } } → success empty (the resolution is the
feed's push) | error (derived: no gate standing, unserved model or scope).
rpc AnswerColdGate joins the FEED section — 30 rpcs. No WatchColdGate
exists: the gate streams and pages as a row.

### conversation.v1: `AgentToolCallProgress` lands — the heartbeat relay's arm on nine tool kinds

**What changed ("looks good"; the reopen queued at the FeedSimpleToolCall
landing).** NEW shared `AgentToolCallProgress { last_progress_at_ms }` — the
vendor's per-call beat relayed as observed, never a shim-invented ping; the
shim keeps the wedge RULING (timeout still settles the unit's failure). A
`progress` arm joins the result oneof of read, write, edit, grep, glob,
bash, skill_use, send_message and unmodeled (next free tag each). The
start/update rule SURVIVES INTACT: progress is a THIRD arm kind — a beat,
not growth — so `update` stays earned by growth alone.

**Boundaries drawn.** The SUBAGENT keeps its existing richer progress (the
vendor's subagent channel carries tokens/duration/retry; a beat would be a
lesser second spelling). TASK ACTS get no arm (an act is instantaneous).
STORE: no change needed — a progress frame is an ordinary upsert of its
unit's page line; the unit's row simply carries a fresher frame.

### The topbar WARNING DROPDOWN lands remediated — list + per-kind overlay; the two unmodeled re-homings arrive

**What changed ("looks good", after the user's list+overlay restructure).**
TopbarWarningStrip = the last warnings newest-first, daemon-capped; each
TopbarWarning = { line (the dropdown row's sentence) ; oneof detail — the
click's OVERLAY content }: accounting { composed evidence lines — the
topbar's OWN resolved copy now that an overlay draws it; the old
carries-nothing stance was right for a hover badge, wrong for an overlay },
unmodeled_tool { tool_name; abbreviated legible argument_lines — never a
dump, never a failure; one warning per distinct tool name }, and
detached_unmodeled { tool_name; started_at_ms — one warning per live item;
the fact the footer chips deliberately dropped, surfaced so it can never
vanish }. This discharges the stage-2 reopen the unmodeled-tool landing
recorded ("its home is the topbar's alert surface, in the dropdown") and
the footer landing's re-homing of detached-unmodeled. The
small-model-summarization idea remains an idea, unmodeled.

### `AnswerQuestion` lands — the sibling verb; echo-the-value end to end

**What changed ("looks good").** NEW endpoint_answer_question.proto:
request { WorkspaceRef; FeedId question; repeated AnswerQuestionAnswer
{ question_text (echoed — never position); chosen labels (echoed); optional
other_text (legal beside choices and alone) } }; success empty (delivered);
error arms derived (unknown card, answered/expired, an echo the batch never
served, multi-pick on single-select). rpc AnswerQuestion joins the FEED
section — 29 rpcs. The split from AnswerPermission mirrors the
conversation-level permission/question split and the held-offer precedent:
different item kind, different answer shape, own verb.

### `AnswerPermission` repoints — FeedId echo, the landed card's arms; the WHOLE TREE COMPILES again

**What changed ("looks good").** endpoint_answer_permission.proto rewritten:
request { WorkspaceRef; frontend.v1.FeedId permission (the card's row,
echoed as served; the standing echo token stays daemon-side); oneof answer
{ allow_once | allow_standing (legal only when the card carried
standing_offered) | deny { optional relayed reason } } }; success empty
(delivered — the card's new state is the feed's push); error arms derived
at the wave. The old ToolCallId shape and its embedded question-answers arm
die — question answers become the sibling AnswerQuestion verb, next.

### `FeedQuestion` lands — the choice card; answers ride the row for cold repaint

**What changed ("question stuff looks good", after the coupling question).**
FeedQuestion { repeated FeedQuestionItem (1–4: header chip; text — an ECHO
VALUE; per-question single_select | multi_select over options whose labels
are echo values, descriptions optional); oneof state { open | answered
{ at_ms; per-question GivenAnswer { header; chosen labels; optional
other_text } } | expired { at_ms } } }. The free-text escape is always
drawn. Expiry is the producer's idle timeout, drawn as expired never
pending.

**The user's coupling question, answered and recorded at the message.** Why
do answers live beside questions? The card is one drawn box in two lives —
form, then verdict lines — and a COLD REPAINT of a settled card has only
the row to draw from, so the choices must ride it. Same upsert-through-
states pattern as the permission card.

### The explicit-`optional` sweep lands across every package (Opus subagent, `aace177a1`)

**What changed.** 37 nilable message fields gained the explicit `optional`
keyword across feed (20), footer (11), store (2 — `StoreAgentUpdate.top_level`,
`EntryBatch.cursor_advance`), sidebar (`WorkspaceRoster.current`), topbar
(`TopbarModelSelector.selected`), and the two feed-verb `feed` addresses.
Post-edit protoc output verified byte-identical to the pre-edit baseline
(only the known `endpoint_answer_permission.proto` break remains).

**Borderline judgments recorded so they are not re-litigated**: the footer's
expanded panels stay bare ("always resolved on every push"); RosterRow's
current/closed stay bare (their bool carries the state, presence is not the
signal); the host-surface facts stated as unconditional stay bare;
`AgentPrompt.agent` stays bare ("never unset"); always-set elements
(TopbarView.token_breakdown, FeedPage.breadcrumbs, AgentWorkflowStart.script,
AgentBash output) stay bare.

### `FeedPermission` lands — the consent card from the typed source; the standing token stays daemon-side; NEW CONVENTION: nilable message fields carry explicit `optional`

**What changed ("looks good").** FeedPermission { headline (vendor-rendered
sentence); optional subtitle; optional trigger note (composed; ask-rule
wording forbids auto-approve); arguments (composed preview lines);
optional standing_offered (empty marker — presence draws the "always
allow" button); oneof state { open | answered { at_ms; allowed_once |
allowed_standing | denied_by_user | denied_by_policy{composed reason} } |
abandoned { at_ms } } }. A denial is an ANSWER; a policy denial is worded
never to read as the user's act. THE STANDING ECHO TOKEN NEVER REACHES THE
CLIENT: the daemon holds it and supplies it to the shim when the answer
verb picks standing — the card carries presence only.

**NEW CONVENTION (the user's, dispatched to the skill by one-shot):**
nilable MESSAGE fields always carry the explicit `optional` keyword — never
bare message-presence — so maybe-absent reads off the schema at a glance. A
repo-wide sweep of all packages is dispatched to an Opus subagent; this
landing already conforms.

### `FeedTurnEnded` bodies land — concluded is BARE; errors are the API taxonomy respelled, per-arm info only

**What changed ("looks good", after the user struck the concluded arms).**
FeedTurnEnded { ended_at_ms; oneof { concluded { FeedId answer — the
final-answer border target, closing the FeedResponse flag } | errored |
interrupted } }. The orchestrator's concluded-notice oneof (quiet |
max_tokens | refusal) was REJECTED: "max tokens and refusal are errors,
essentially" — both moved into errored's cause arms. Errored was first
sketched as generic headline+detail strings; the user rejected that too —
"error information should be specific to the error types" — so the arms are
conversation.v1 api.proto's nine-kind vendor taxonomy RESPELLED figma→idl
(FeedTurnError*: rate_limited/overloaded carry optional retry_after_ms so
the client ticks a countdown; vendor_unmodeled keeps the type name; six
empty arms), plus max_tokens, refusal, and query_died. The envelope carries
the vendor's own sentence when recorded (UNSET for wordless causes).
Interrupted stays the acknowledged-stop accusation.

### `FeedAgentPrompt` lands — one orange prompt bubble, both ends of the relay

**What changed ("okay").** FeedAgentPrompt { address (composed — "→ Explore"
sender-side, "from Plan" recipient-side); body { repeated blocks of the
feed's shared drawn vocabulary } }. One component for the outgoing send and
the delivered prompt.

**A recorded departure from the old SendMessage card.** That card NEVER drew
the body (relays run long; summary-only, with a stated gotcha). Under the
user's "same as a normal user prompt" ruling the body IS drawn, with
fold/cap as client presentation. The old gotcha is superseded by this
choice.

### `FeedShell` lands — the spool is a whole-replaced TAIL; a non-zero exit is COMPLETED

**What changed ("looks good").** FeedShell { command; runtime; optional
spool { tail text; composed omitted line } ; oneof state { live
{ last_progress — spool growth IS the beat } | settled { ended_at_ms;
optional exit { code }; oneof outcome { completed | cancelled | lost } } } }.
Snapshot semantics: the daemon caps and replaces the tail whole; the client
appends nothing (the old offset-append machinery stays dead).

**Two judgments recorded.** NO `failed` outcome arm — a non-zero exit still
COMPLETED; the exit chip carries the verdict and "failure" is the reader's
judgment of the code. And the old "page older spool inside the bubble"
affordance is DROPPED — tail + composed omitted line only; display implies
no retrieval. Exit is optional because the spool terminator may be absent.

### `FeedSubagent` lands — the collapsed head rides the PARENT feed; identifier-only rows weighed and declined

**What changed.** FeedSubagent { label; optional description; optional
tokens (formatted running sum); runtime { started_at_ms }; oneof state
{ live { last_progress } | settled { ended_at_ms; oneof outcome
{ succeeded | failed | cancelled | lost } } } }. One component for sync and
detached; the body is the sub-feed (opened by the row's own FeedId), so the
message is exactly the collapsed head. `lost` keeps its own word — we
stopped seeing it, not known failed. Live carries the same last-progress
heartbeat feedback as the tool card (the subagent progress channel).

**The user's spitball, weighed and declined with reasons recorded.**
"Should subagents be handled exclusively as dedicated WatchFeed calls —
row carries only the identifier?" NO, on two grounds (transport was NOT
one — streams multiplex over the one socket): (1) HISTORY — most bubbles
in a feed are settled; identifier-only rows would demand a live connection
per settled bubble just to draw "✓ Explore · 2:10", inverting pulled
history vs pushed live; (2) STREAM UNIFORMITY — the head is the parent's
fact about its child, and routing it over the child's connection would
grow the sub-feed stream a second frame kind (row | head). What survives
of the instinct IS the design: the identifier is how expand reaches the
content (OpenFeed(id)); only the collapsed head rides the parent.

### `FeedTask` lands — the task bubble is the task AS IT STANDS

**What changed ("look good").** FeedTask { status (six-arm projection,
running{active_form}); subject; optional description; optional owner
(composed line) }. One row per tracker task, upserted by every act; the
footer checklist rows jump here. DELIBERATE OMISSION, recorded: no act
history inside the bubble — current state only; act-by-act narration, if
ever wanted, is presentation-nested rows later, never fields here. The
status family is its own (one fact, two views with the footer's checklist).

### `FeedSkill` lands — the teal document card

**What changed ("looks great").** FeedSkill { invocation (the composed
"/skill args" line); oneof outcome { running (document pending — the unit
settles on the DOCUMENT, which arrives after the tool's own
acknowledgement) | loaded { document (SKILL.md markdown, folded) |
allowances (composed consent line, UNSET = none declared) } | failed
{ composed reason } | denied } }.

**Why its own card, not a SimpleToolCall arm.** Its body is a markdown
document with a fold — a different drawn component than the four output
forms — and the teal wash marks a conversation-of-its-own. Post-skill
nesting stays a `parent` presentation choice (nothing delimits a skill's
scope at the source, per the skill landing's transcript verification).

### `FeedSimpleToolCall` lands — one shell, output by DRAWN FORM; heartbeats become first-class shim→daemon feedback (a recorded reopen)

**The drawing agreed first** (read off render.ts/CSS, in the message
comment): head (purple tool name + status badge) / one composed input line /
dashed divider / capped output. The six tools' variance is entirely in the
input phrasing and the output form.

**What changed ("yeah looks good").** FeedSimpleToolCall { name; input
(ONE composed line — the daemon owns per-tool phrasing, including a write's
created-vs-updated wording); oneof outcome { running { last_progress } |
returned | denied } }. FeedToolCallReturned { oneof verdict { succeeded |
failed }; oneof form { text | code { spans; omitted } | diff { arm-typed
lines } | lines { lines; omitted } } }. GENERICIZED per the user's ruling:
the arms are presentation forms, not tools — "unless there needs to be
specially supported information, it should be genericized" — so the client
holds no per-tool knowledge and a tool-specific affordance is a schema
change by design. What did not survive genericization, and where it lands:
created/updated → the input line's wording; read's head-of-N and glob's
at-least floor → the composed `omitted` sentence (the completeness arms
collapse to daemon prose at this boundary — the client draws, never
compares); bash's exit → the verdict badge. FeedCodeSpan keeps a STRING
paint_class with the closed-arm-set flag carried in its comment (owed as
before); FeedDiffLine is arm-typed for color (header|added|removed|context,
text prefix-free).

**HEARTBEATS AS FIRST-CLASS FEEDBACK — the user's reopen of the shell
landing's discard.** That landing had the shim DISCARD the vendor's
per-call heartbeat/elapsed; the user: the shim should surface beats to the
daemon as responses — the timeout still yields the shim's failure exactly
as ruled, but the daemon also gets feedback to reflect into the feed. This
is consistent with the 4b dissolution (what died was the shim-invented
connection ping and the id-list heartbeat; the vendor's per-item
tool_progress was already mapped to "an update frame on that work's own
stream", and its retry block already feeds the footer's retrying arm).
Division of authority: shim owns the wedge RULING (failure on timeout,
unchanged); daemon relays the latest beat into FeedToolCallRunning
.last_progress by re-pushing the row; the client ticks "quiet for N s"
locally from the instant — no cadence timing, no client threshold. The
in-between state ("beats stopped, shim has not ruled") is deliberately
unrepresented: one ruling beats a two-stage alarm.

**QUEUED, its own increment: the conversation.v1 update-arm reopen** —
per-tool progress arms (the tool kinds EARN an update arm now that the
relay produces one), reopening the start/update rule's "only bash, thinking
and response earn one" enumeration and the shell landing's discard note.

### `FeedResponse` lands — the prose bubble; usage rides the ENVELOPE, live in every state

**What changed ("land").** FeedResponse { FeedResponseUsageStamp usage
(optional-by-presence); oneof result { update { prose } | success { prose }
| error { prose } } }, with FeedResponseProse { markdown } and the stamp a
daemon-formatted string. The user hoisted usage from the success arm to the
envelope: the vendor states usage when a response OPENS and restates it as
it grows (verified at the thinking/response landing), so the stamp is live
while arriving, final once settled, last-observed on a broken bubble;
absence draws no stamp, never a zero.

**The error arm carries NO reason, on purpose.** WHY a response died is the
turn terminal row's fact (FeedTurnEnded.errored) — one failure, one home;
the row's arm only marks which bubble was cut short and keeps its partial
prose drawn.

**Open, flagged.** The final-answer treatment (the old green border on the
turn's answering response, which AgentSuccess.completed.answer names) is
deliberately unmodeled here pending the user's call — likely a resolved
marker on success if kept, possibly redundant beside the terminal row.

### The FEED VERBS repoint lands: OpenFeed mints the tail token; WatchFeed goes token-addressed; GetFeedPage walks ONE feed; Interrupt echoes FeedId

**What changed (shapes agreed in conversation at the container settlement:
"that looks correct" / "great").** NEW endpoint_open_feed.proto —
OpenFeed { WorkspaceRef; optional frontend.v1.FeedId feed (UNSET = the root
feed; SET = a subagent bubble's sub-feed) } → success { the newest FeedPage;
FeedWatchToken } | failure (arms derived). NEW agentrepl/v1/feed_token.proto
(shared file — two endpoints need it): FeedWatchToken, opaque, minted by the
open, pinning the tail exactly after the answered page — the store's
open/watch bifurcation paralleled at the frontend boundary, discharging the
reopen recorded at the store's OpenAgentSession landing ("the agentrepl.v1
feed surface at the frontend remediation pass"). endpoint_watch_feed.proto
REWRITTEN: request is the echoed token alone (workspace-scoped by mint);
response unchanged (one whole FeedRow per frame); the stream is STANDING
across turns — a turn's end is the FeedTurnEnded ROW, never the stream
concluding, and any non-client-cancelled end is a transport failure.
endpoint_get_feed_page.proto: the container oneof (top_level |
parent(MessageId)) collapses to the same optional FeedId address; first/next
and the daemon-held walk unchanged; GetFeedPageTopLevel deleted.
endpoint_interrupt.proto: the detached target retypes MessageId →
frontend.v1.FeedId, echoed as served (the daemon decodes and issues the
stop). service.proto gains rpc OpenFeed in the FEED section.

**Consequences.** One connection per OPEN feed: the client opens the root on
view-open and each bubble on expand, abandons tokens on collapse. The
remaining compile break is endpoint_answer_permission.proto alone (its
ToolCallId — the permission walk's turn).

### The FEED's container lands remediated: one opaque FeedId; feed-within-feed; the row taxonomy (sync activity / terminal / detached wrappers / blocking / meta); AgentActivity respelled figma→idl; terminal-as-row

**IDENTITY ("maybe there should be a simple canonical feedid").** One opaque
daemon-minted string. The daemon ENCODES the identity of what the row DRAWS
(a unit, a task id, an agent id, an ask id, or a daemon fact for a
synthesized row) and DECODES it on echo — mint and resolve are encode/
decode, no id table, stable across pushes/restarts. The typed identity
spaces are FULLY HIDDEN from the frontend ("the frontend only cares what
bubble the thing needs to go into"); the one typed survivor is the TurnId
stamp for own-prompt matching. An earlier typed-oneof FeedRowId (and a
FeedRow/FeedRowId structural split) was iterated away by the user: the
parallel id-arm/kind-arm oneofs could co-vary illegally. UPSERTS ARE
UNIVERSAL — every row replaces whole by id; what varies is only which
identity the id is minted from, under the rule "one row per drawn subject"
(a response row per unit; ONE task bubble fed by many acts; a subagent
bubble keyed by the agent, not its spawn call).

**FEED-WITHIN-FEED (the user: "agent bubbles are literally their own
feeds").** A subagent bubble — sync OR detached, one FeedSubagent component
— is a SUB-FEED: same row vocabulary, its own connection and pages. The
bubble row's own FeedId IS the sub-feed's address (a FeedContainer element
was proposed and dropped as a second copy of the row's id). TWO KINDS OF
NESTING: real sub-feeds (agent bubbles; connection = placement, rows carry
no parent) vs PRESENTATION nesting (`parent`: merge-phase rows, work under
a skill heading). The agentrepl feed verbs follow at their repoint:
OpenFeed { workspace; optional FeedId } → first page + FeedWatchToken;
WatchFeed { token } → pure tail of one feed (response unchanged: one
FeedRow per frame); GetFeedPage keeps the older-pages walk — shapes agreed
in conversation, LANDED AT THE VERBS' OWN INCREMENT.

**THE ROW TAXONOMY, the user's organizing principle.** user_prompt /
agent_prompt / activity / turn_ended / detached_subagent / detached_shell /
permission / question / context_cut:

- PROMPTS ARE TWO SIBLING KINDS: FeedUserPrompt (renamed from FeedUser,
  family renamed with it) and FeedAgentPrompt — "conceptually extremely
  similar (agent sends prompt to another agent, vs user sends prompt to an
  agent)"; the agent one wears an ORANGE BORDER, appears on the sender's
  feed as the send and the recipient's as the delivery. This relocated
  SendMessage out of activity (it had first landed there as a card; the
  user: it is a prompt).
- FeedTurnActivity = SYNCHRONOUS TURN PROGRESS, whoever drives it:
  { response | simple_tool_call | skill | task | merge | subagent(sync) }.
  MERGE IS ACTIVITY by the user's ruling: "from the user's perspective the
  turn has not concluded while a Merge is in flight" — the container was
  renamed from FeedAgentActivity to FeedTurnActivity because merge is the
  one arm the agent does not author.
- FeedSimpleToolCall GENERALIZES read/write/edit/grep/glob/foreground-bash:
  one shared grey-bubble shell (VERIFIED in the webapp: .tool-card — grey
  --card background, bordered, agent-column cap width, already a standalone
  .feed-item sibling of the bubbles, teal for skill/agent, amber for merge)
  with a per-tool oneof for tool-specific content. Shell sectioning to be
  read off the drawn cards at its increment.
- THINKING IS DROPPED from the feed (the user: not rendered there; footer
  only). UNMODELED IS DROPPED from the feed → the topbar warning dropdown
  (and the detached-unmodeled footer chip/panel had already moved there).
- TASKS get the subagent-analogous treatment: a bubble in the feed AND
  footer rows that jump to it (FooterTaskRow gains a FeedId target —
  landed). FeedTask is keyed by the TRACKER task, not any act's unit.
- DETACHED WRAPPERS: FeedDetachedSubagent/FeedDetachedShell wrap THE SAME
  drawn component their sync forms use (FeedSubagent; FeedShell) — sync-vs-
  detached is placement, never a second drawing; the old FeedDetached
  (generic head + body oneof) died as door-keyed coalescing of different
  components.

**TERMINAL-AS-ROW ("we'll also need to model when a response is
terminal").** The user proposed connection-lifecycle terminality (WatchFeed
ending with typed arms); the orchestrator surfaced the two constraints —
history must replay how a settled turn ended (the terminal is the stop
notice's only source), and the bounded-stream convention forbids a bare
close meaning anything — and the agreed synthesis is FeedTurnEnded, A ROW:
{ concluded | errored | interrupted }, streamed and paged like any row.
Liveness is structural ON THE DATA: no terminal row for the current turn =
live; WatchFeed stays standing across turns; a dead connection stays a
transport failure. It sits at ROW level, not in activity ("it's not agent
activity" — the user's oneof, realized as the row arm).

**Facts re-verified for this settlement.** Multiple responses per turn is
the normal case (the ~13k-message survey; AgentSuccess.answer exists
because finality is positional nowhere). Skill scope attribution: NO — no
SDK/transcript delimiter; only the document links via sourceToolUseID;
nesting under a skill stays a presentation choice. Sync agents at a feed's
top level are unambiguous: another agent only ever appears AS its bubble;
activity rows always belong to the feed's own agent (request-scoped
placement, the store rule's frontend face).

**What died in feed.proto.** The FeedAgent family (head+blocks composition
— one-unit-per-row makes the tool card the row, matching both the store's
"each block is its own feed item" and the DOM), the FeedTool family, the
FeedDetached family, the old FeedPermission body (questions inside it —
FeedPermission and FeedQuestion are now separate skeleton kinds re-derived
from AgentPermission/AgentQuestion at their increments), FeedAgentStopNotice
(superseded by FeedTurnEnded). FeedUserPrompt (renamed), FeedContextCut and
FeedMerge families carried verbatim; kind bodies for
response/simple_tool_call/skill/task/subagent/shell/agent_prompt/
turn_ended/permission/question are declared skeletons filled at their own
increments; git history is reference material, not a template.

**Consequences.** The daemon's feed resolver keys rows by drawn subject and
switches per DetachableWork arm to the kind components (subagent arm and
sync spawn converge on FeedSubagent; bash → FeedShell; workflow → nothing,
deferred; unmodeled → topbar). The webapp routes strictly by FeedId, opens
one connection per expanded bubble, and its parent-routing applies only to
presentation nesting. agentrepl.v1's endpoint_watch_feed / endpoint_get_feed_page /
endpoint_interrupt do not compile until their repoint increment (interrupt's
detached target becomes a FeedId echo).

### The FOOTER lands remediated: selection-keyed expanded panels; the tokens CELL shrinks to one figure; live-work CHIPS; the task tracker gets its drawn home; WORKFLOWS ARE DEFERRED WHOLESALE

**The strip ("looks good" across the iteration).** Status | SubStatus |
StatusActivity | Clock | the TOKENS CELL | the LIVE-WORK CHIPS
(right-aligned). The cell is ONE figure — the turn's uncached input — plus
two glyphs (alarm ⚠, accounting badge); thinking leaves the strip. The
chips are ⚙ agents (count), ☑ tasks (done/total fraction), $ shells
(count); an unset chip is not drawn (presence-gated on liveness /
tracker-non-empty). Cell and chips are click targets; the selection is
WEBVIEW-LOCAL, so the daemon ships EVERY expanded panel fully resolved on
every push (the folded-menu convention) and the client draws whichever the
selection picks.

**The expanded panels.** Tokens: NOT a list — heterogeneous facts, one
dedicated line-element each (uncached input, cache read, cache write,
output, thinking indented under output, first-token latency, the alarm
sentence when tripped, the verdict line with evidence text on the
incomplete/invalid arms); lines are always set with optional values so the
panel shape is stable mid-turn. Agents and shells: TRUE LISTS (repeated of
one row type) — label/description/tokens/runtime rows for subagents,
command/runtime for shells, each row a jump target. Tasks: a list too — the
checklist; the "checkbox" is NOT a bool but the drawn projection of the
tracker's six-arm status (a bool would collapse running/failed/killed/
paused), running carrying the optional active-form phrasing.

**The user's rulings on the way.**

- WORKFLOWS ARE KICKED DOWN THE ROAD: no footer support, no frontend.v1
  support, no daemon handling — a later feature added at a later date. The
  drawn workflow panel from the drawing iteration is withdrawn; the run
  bubble/richer view research (jobs-list default, graph secondary, per
  GitHub Actions/Temporal/Airflow) is recorded with it for that later turn.
- The unmodeled chip is REMOVED: detached-unmodeled surfacing belongs to
  the topbar's warning dropdown (re-aligning with the unmodeled-tool
  entry's "its home is the topbar's alert surface"). The unmodeled panel
  dies with the chip (a panel with no chip is unreachable).
- The tasks chip's two counts are a progress fraction, both always drawn —
  explicitly NOT the completeness-oneof case, stated at the message.

**The TASKS research that reopened the gap (the user asked "is that a huge
gap?").** conversation.v1 is NOT the gap: AgentTaskAct { AgentTaskId;
created|changed; AgentTaskState { subject; description; optional owner;
status pending|running{active_form}|completed|failed|killed|paused } }
rides AgentActivity arm 16, state on every act. The GAP was frontend:
stage 2 deleted TaskCatalog/TaskEntry as "superseded by the detached
bubble and the footer's expanded rows", but the landed rows were
detached-work only — the tracker had NO drawn home. This landing closes
it. FLAGGED, not solved: cold paint of the CURRENT task list — acts are
history entries, so a cold consumer has no snapshot; the daemon must hold
or recover the list (its source at the wave).

**What died in footer.proto.** The old FooterExpandedRow family (its
MessageId target was already dead — this also discharges the footer's
share of Owed F), the five-sibling FooterTokens cell
(Input/Thinking/FirstToken/ExpensiveTurn/Accounting), the structured
FooterTokensExpensiveTurn (TurnId/counts/threshold/origin arms — the alarm
is now the cell glyph + the daemon-composed panel sentence, whose phrasing
states the prompt-vs-cold-keep-alive origin), and FooterTokensAccounting
(summary+verdict → cell badge + panel verdict line; the hover-tooltip
treatment is replaced by lines). Imports: message.proto/turn.proto →
agent_activity.proto (AgentId, AgentActivityId jump targets).

**Embedded decisions, accepted.** Daemon-formatted figure strings
throughout (no client rounding, the context-cut precedent); typed-identity
jump targets (AgentId for a subagent row, AgentActivityId for a shell row)
anticipating the feed remediation keying bubbles by those; clocks carry
only the start instant and the client ticks.

### THE FRONTEND REMEDIATION PASS OPENS — stage 2 reopened by name; the walk starts at the FOOTER (expanded footer figma→idl in depth)

**The stage open, in the user's terms.** "The protos we landed were good when
we landed them, but we found and added a lot of missing functionality" —
frontend.v1 must now reflect everything stages 4–6 made available. Named
first: WORKFLOWS, now comprehensively supported, need their frontend.v1
shape, their UI/UX, and the daemon architecture that follows ("the daemon is
going to need to be able to resolve a given workflow to all its toplevel
agents, differentiating them from the subagents those toplevel agents
themselves spawn").

**Direction stated by the user (to iterate, not yet settled shapes).**

- A workflow's subagents do NOT enter the feed; they are shown in the FOOTER
  only. Normal (agent-spawned) subagents keep both: an Agent bubble in the
  feed AND a listing in the expanded footer.
- The expanded footer's content DEPENDS on the main-strip selection: clicking
  a live-work summary chip (e.g. "3 Agents") on the strip's right side opens
  the expanded section listing exactly those items, one per line, with
  per-item metadata (token usage, duration, …) whose organization is to be
  figured out in the drawing.
- The walk starts with the FOOTER's figma→idl in depth (the expanded footer
  especially), per the settled ASCII-drawing-first discipline.

### STAGE 6 COMPLETE: `state.v1` is DELETED — the WSM's durable state is DDL, not proto

**What changed ("this looks good, i approve").** `state/v1/durable.proto` is
deleted and the `state.v1` package ceases to exist. Its only stored uses —
`TokenUtilization` and `TurnAccounting` blob rows — lose their producer:
usage rides the store's frames and aggregates are derived on read. The
daemon's durable state needs no wire shape at all: it has one producer and
one consumer (the daemon itself), so it is internal DDL under the same
columns-vs-blob rule as the store.

**The approved WSM schema (architecture guidance, recorded not proto).**
Seven tables: `workspace` (lifecycle arm + since, current merge phase,
last activity), `merge_queue` (repo-ordered positions), `merge_lease` (one
open window per repo), `workspace_merged` (set-once), `held_prompt`
(TurnId PK; UserSaid as the one content BLOB; classification and hold
reason as columns), `session_binding` (workspace → mutable
vendor_session_id + last known model/mode for pre-attach display),
`shutdown_schedule`. Merge phase HISTORY stays out — the merge bubble's
rows are conversation content the daemon synthesizes into the feed.

**What dies with today's state.db, each with its replacement** (from the
enumeration the user walked): turn ledger → open WatchAgent streams + store
entries; prompt receipts → StartTurn; keep-alive windows → shim-internal
(Owed G); reader positions and compaction gate → client-held pointers and
the shim's store reads; connectivity/fault ledgers → stream lifetimes +
pulled diagnostics; token evidence → store frames; failure cards → frames
and resolved views.

**Architecture rulings recorded with it.** statedb stays an IN-PROCESS
library (one SQLite file, one writer): a service split was weighed and
declined — the store earned its process boundary by having two producer
processes; daemon state has one, and the single file's value is cross-table
atomicity. The SSM→WSM rename and the shed of the six non-workspace
families (audit F) are implementation-wave work carried with this stage.

### `ReadHistory` regains its FIRST arm — a subagent's cold paint reads it

**What changed (the user's catch).** `StartTurn`'s first page is the MAIN
agent's only, so a subagent bubble painted from scratch (a bounced frontend)
had no first-page verb. `ReadHistoryRequest` regains `oneof position
{ first | after (HistoryPointer) }`. The daemon's cold-paint sequence for a
subagent: ReadHistory(first) → WatchAgent with the page's newest pointer as
known_through, pinning the tail to exactly after the page. An
OpenAgentSession+token split was re-weighed and declined again: the single
WatchAgent stream plus the pointer handoff pins the same seam without a
token.

### CORRECTION to the agent consolidation: `StartTurn` is the MAIN AGENT's verb and returns the FIRST PAGE; `AgentInput.prompt` returns; `UpdateAgent` prompts existing agents

**What changed (the user's rejection of the target field).** `StartTurn`
loses `optional target` and gains the store-open semantics: request
{ turn; said; origin; page_size; optional known_through }, success
{ AgentPrompt prompt; HistoryPage page } — one call paints and submits, and
`prompt.agent` is the WatchAgent address. `AgentInput.prompt` (tag 3) is
RESTORED: a prompt to an EXISTING agent (a subagent, a workflow's agent)
rides `UpdateAgent`, with delivery per kind the shim's as already ruled.
`WatchAgent` and `ReadHistory` unchanged.

**The distinction, in the user's terms.** "StartTurn principally differs in
that it returns in the response the first page (like it is in the store)"
— it is the session's own verb; other agents are reached by ids the API
already serves. A subagent's FIRST prompt needs no verb at all: REVIEWED and
confirmed covered — `AgentSubagentStart.prompt` (`AgentSubagentPrompt.text`
plus description/type/name/model/isolation) rides every frame of the spawn.

### The AGENT CONSOLIDATION lands: StartTurn targets ANY agent; ONE `WatchAgent` opens-with-a-page; `UpdateAgent` replaces the per-kind updates; "main agent" leaves the API

**What changed (settled across the exchange; "sounds good" + the WatchAgent
correction).** shim.v1's TURN section becomes the AGENT section:
`StartTurnRequest` gains `optional AgentId target` (UNSET = the session's
prompt thread, resolved by the shim; SET = any agent — subagent, workflow
agent — with per-kind delivery the caller never learns). NEW
`endpoint_watch_agent.proto`: `WatchAgent { optional target; page_size;
optional HistoryPointer known_through }` → a standing stream whose FIRST
frame is the opening page (full repaint, or only entries newer than the
caller's own mark) and whose tail is one pointered entry per write. NEW
`endpoint_update_agent.proto`: `UpdateAgent { optional target; AgentInput }`
— stop and answer only. DELETED: `WatchTurn`, `UpdateTurn`, `WatchSubagent`,
`UpdateSubagent` and their files. `ReadHistory` goes NEXT-ONLY
(`{ optional target; page_size; HistoryPointer after }`) — the first page is
WatchAgent's opening frame. `conversation.v1`: `AgentInput.prompt` RETIRED
(a prompt to ANY agent is StartTurn — one way to say each thing);
`history.proto` gains `HistoryPointer` (replacing `HistoryContinuation`) and
`HistoryEntryAt { at; entry }`, with `HistoryPage.entries` pointered and
`HistoryMore` carrying the last entry's pointer; `SessionStarted` LOSES
`main_agent_id`.

**Why, in the user's terms.** "Watching the main agent and watching a
subagent is the same API"; nothing StartTurn does is main-specific once the
target defaults; the id sources ARE the API (StartTurn's `prompt.agent`, the
spawn announcements, workflow levels), so an OpenAgentSession verb and its
token were rejected — "that's ALREADY covered" — and the store's
known-through trick carries the repaint/catch-up split instead. "Main agent"
survives only inside the shim and as the store's scope (Owed H unchanged);
no consumer ever sees it.

**Discharged / superseded.** The recorded WatchTurn open/watch reopen is
discharged. `SessionStarted.main_agent_id` (the history entry that added it)
is superseded; the store still scopes by the logical session internally.
The one-turn-in-flight rule becomes per-agent, stated at StartTurn.

### STAGE 6 OPENS: the SSM's purpose is WORKSPACE STATE — rename to WSM; the durable evidence layer's premises re-examined

**Settled with the user, above any shape.** The daemon's SSM exists to manage
WORKSPACE state — "what workspaces are open currently, what workspaces are
merging and what the merge queue status is" — and is to be RENAMED WSM
(workspace state manager). It tracks at a much higher level than the store,
which owns agent-response information. A comb of the current implementation
against this categorization is running; whatever does not fall inside it is
enumerated and re-homed by decision, not ported.

**A correction of the orchestrator, kept visible.** The orchestrator called
`durable.proto`'s frozen-replay premise VOID under nuke-never-migrate; the
user: it is NOT void — it is an OPERATIONAL prescription, merely not relevant
during development. The constraints return once the schema ships.

**Also noted at stage open.** `durable.proto` is dark (its two deleted
shim.v1 imports); usage evidence now reaches the daemon on the same streams
as everything else (envelope usage, account_usage session updates), so the
stage's central question is what the daemon must durably hold that the store
does not.

### `GetLiveWork` lands — the OPEN-OBLIGATIONS verb ("okay seems reasonable")

**What changed.** `endpoint_get_live_work.proto`: empty request; success
{ live_agents; live_workflows; live_detached } — ids only, from the three
tables' non-terminal rows; failure derived. Eight rpcs on the service.

**The semantics the user probed to settlement.** "Live" is a claim about the
RECORD, not the world — "a start was written and no terminal ever was" —
so it cannot go stale; a five-second bounce and a two-week-old workspace run
the identical procedure. The SHIM (never the sidecar, which is a copier
whose only recovery is cursors) calls it once at session start and resolves
every item: re-adopt what the revived vendor process actually has (reported
as SessionStarted.live_work), WRITE the closing terminal for what did not
survive (dual-write closes the record and puts the stop notice in the feed).
The invariant: every started thing eventually gets a terminal row, by
observation or by reconciliation. Deleting the verb was weighed: it would
make restarted background work invisible-but-running and dead work
spin forever.

**Rename considered, declined.** `WriteBatch` → `WriteSidecarBatch` was
proposed and withdrawn: the shim also writes batches (stream-plane facts,
spill replays); only cursor_advance is sidecar-specific and already states
its absence.

### store.v1 `GetWorkflow` lands ("looks great")

**What changed.** `endpoint_get_workflow.proto`: request { DetachedWorkId };
success { AgentWorkflowStart (from the workflow row's columns); oneof
standing { live { the DERIVED level } | ended { the terminal, embedded
stream vocabulary } } }; failure derived. The level is computed at serve
time from the agent table, stored nowhere. The shim's own GetWorkflow serves
from this, adding only the watch token. `ended` deliberately carries no
level; a caller wanting a dead run's spawn list is a later additive arm if
ever needed.

### `detached_work` moves UP into `AgentFrame`; the frame's oneof IS the datalayer route; the routing and schema settlements

**What changed ("looks good … let's land").** `AgentFrame.result` becomes
`{ update | success | failure | detached_work }` (tag 5); `AgentUpdate`
loses its `detached_work` arm (tag 2 retired) and is now purely
conversation content `{ activity | question | permission }`. Consumers that
switched on AgentUpdate for the open-a-stream obligation switch on
AgentFrame — same information, one level up.

**The routing rule the arms now state (the user's design, kicked around to
settlement).** `update` → the entry table, as a page line of the agent's
book; `success`/`failure` → BOTH entry (the stop notice has no other
source) and the agent row's terminal columns, one transaction;
`detached_work` → the lifecycle table for its kind (agent, workflow,
detached_work), NEVER a page line — the spawning call is already one. The
`AgentWorkflow` arms route without any shape change: start → workflow row,
update level → AGENT rows upserted (`spawned_by_workflow`, spawn columns,
liveness), terminal → workflow terminal columns; the level is stored
nowhere and is the join.

**Schema settlements from the same conversation.** Prompts and frames share
ONE entry table (one position space is what makes "everything after the last
real prompt" — the rollback — a range query; `turn_id` a nullable column or
1:1 side table; a prompts "table" is a partial index, never a second
position space). The dedicated tables are PRIMARY for their facts (the
earlier projections-rebuildable-from-entry idea is superseded); verbs read
tables, entry is canonical for content; every write lands in exactly ONE
table, decided by its wire arm, plus the success/failure dual-write. The
`agent` table carries `spawned_by_agent` XOR `spawned_by_workflow` (main:
neither); "agents of run W" is one indexed query and IS GetWorkflow's level.

### STAGE 5 SCHEMA ARCHITECTURE: four tables, canonical homes, and the columns-vs-blob line ("it's the queryable and joinable stuff")

**Settled with the user, as architecture guidance for the store
implementation (the store.v1 wire contract is unchanged by it).** Four
tables, each the ONE canonical home of one kind of fact:

- `agent` — one row per `AgentId`, main agent included; THE source for agent
  metadata: `spawned_by` (an agent, a workflow, or nothing for main), the
  unpacked `AgentSubagentStart` fields, `started_at`, `ended_at` (NULL =
  live).
- `workflow` — one row per run, keyed by the announced handle; `spawned_by`
  + origin unit, the unpacked `AgentWorkflowStart` fields, the terminal once
  ended. THE SUBAGENT LEVEL IS NEVER STORED: it is the join (agents whose
  `spawned_by` is the run, with their liveness), so agent liveness has
  exactly one home.
- `entry` — the page lines: a QUERYABLE SPINE (`upsert_key` PK,
  `book_agent_id` indexed and NULL for unserveable, `write_id` unique,
  plane, first-insert position) around a SERIALIZED frame the store never
  opens.
- `detached_work` — one row per detached non-agent run (bash today):
  the handle, kind, origin unit, owner agent, unpacked latest state,
  `ended_at`.

**The columns-vs-blob line, the user's rule.** "Not ALL shapes need to map
… it's the queryable and joinable stuff": agent, workflow and detached_work
are UNPACKED to columns (the store filters and joins on them); `entry`'s
frame stays serialized (the activity vocabulary is content, and unpacking it
would put every conversation.v1 churn into DDL and two mapping directions
for nothing the store ever queries). Mapping tests per the persistence-model
principle guard the unpacked three.

**What the verbs become.** `GetWorkflow` = one workflow row + the agent
join; `GetLiveWork` = the two `ended_at IS NULL` scans; the page verbs =
the entry spine. Every foreign key is stamped at insert with one lookup,
per principle #3.

### The WORKFLOW pipe collapses: the run stream is a STATELESS LEVEL of its subagents; Get/Watch/Stop replace Watch/Update; the daemon owns the fan-out

**What changed (the user's restructure across three messages).**
`conversation.v1`: `AgentWorkflowUpdate` is retyped to `{ repeated
AgentWorkflowSubagent all_subagents }` — REPLACE semantics, every frame the
whole list — with `AgentWorkflowSubagent { AgentSubagentStart agent_start;
oneof liveness { live | ended } }` (the user's `bool is_alive` landed as the
standing two-arm oneof). The `agent_frame` arm is RETIRED. `shim.v1`:
`endpoint_get_workflow.proto` (request { DetachedWorkId }; success { start;
oneof standing { live { WorkflowWatchToken; current level } | ended
{ embedded AgentWorkflowSuccess|Failure } } }); `endpoint_watch_workflow.proto`
rewritten (request { the token }; bounded stream { update | success |
failure }, no start arm — the get answered it);
`endpoint_update_workflow.proto` DELETED, replaced by
`endpoint_stop_workflow.proto` (the run's only addressable act; prompts and
answers to a run's agents go through `UpdateSubagent` by `AgentId`).

**Why, in the user's terms.** The old `agent_frame` arm multiplexed every
agent's whole stream through the run's connection — "paralleling the agent
streaming architecture across systems". The shim/sidecar/store "should be
very stupid": they say an agent EXISTS; "the daemon needs to use that
information to create the necessary connection as it sees fit" — a workflow
agent is exactly a subagent from the daemon's perspective, the parity
principle applied instead of duplicated. The level is stateless because
workflows are small and deliberately have no history/pagination support.

**The open/watch bifurcation, applied here too.** The token lives INSIDE the
`live` arm (meaningless once ended — adjacent exclusivity), so a concluded
run's get is an ANSWER, not a dead token. Each verb owns its failure
vocabulary (unknown handle vs unknown token); where the run's terminal fact
appears it EMBEDS the stream's terminal messages rather than respelling.

**Consequences.** The `UpdateWorkflowSuccess` prompt-mirror from the
AgentPrompt landing dies with its endpoint. The store's workflow row shrinks
to start + latest level + terminal (one upserted row); a store `GetWorkflow`
read verb serves it — next increment. `AgentInput`'s comment claiming the
workflow update carries it is stale and is corrected at the next
conversation.v1 touch.

### `WriteBatch` lands — the write gains the ack the old socket never had; `StoreEntryWrite` dies

**What changed ("land it").** `endpoint_write_batch.proto`:
request { producer; EntryBatch batch }; response { success {} | failure
{ detail } }. `StoreEntryWrite` is DELETED — under Connect the rpc is the
envelope, and a separate carrier would be a second spelling. Success means
DURABLE (records + cursor advance, one transaction), with replay absorption
via write_id documented as the same arm; failure means NOTHING committed, so
the producer's spill holds and replays. The old UDS protocol acked nothing —
a producer learned failure only by connection death; the success arm is what
lets the shim's spill retire batches on acknowledgment.

### `store.v1` adopts the standard service file model ("we really should have service.proto, endpoint_*.proto, and store.proto for datatypes")

**What changed.** `read.proto` is split into
`endpoint_open_agent_session.proto`, `endpoint_watch_agent_session.proto`,
`endpoint_read_agent_page.proto`; `GetSidecarCursors`'s shapes move to
`endpoint_get_sidecar_cursors.proto`; the cross-endpoint page vocabulary
(`StoreItemPointer`, `StoreLineAt`, `AgentSessionToken`, `AgentSessionPage`,
the More/Floor arms) moves into `store.proto` beside the record, the write
batch and the cursor. This supersedes the one-file-reads fold from earlier
today; no shape changed.

### `GetSidecarCursors` lands ("you can land it immediately")

**What changed.** `CursorQuery`/`CursorList` become
`GetSidecarCursorsRequest` and a canonical-outcome
`GetSidecarCursorsResponse { success { cursors } | failure { detail } }` on
the service under a RECOVERY section; empty success is documented as the
fresh-store answer.

### The three read endpoint files fold into `read.proto` — store.v1 is `store.proto` + `read.proto` + `service.proto`

**What changed ("i think we just need a read.proto, not dedicated files").**
`endpoint_open_agent_session.proto`, `endpoint_watch_agent_session.proto`,
`endpoint_read_agent_page.proto` merge verbatim into `read.proto` under
per-rpc banners; the per-endpoint file model is set aside for this small
single-caller package. No shape change.

### Rename: `ReadPage` → `ReadAgentPage` ("we only paginate on agents, so names carry AgentPage, not Page")

**What changed.** The rpc, its endpoint file and its whole message family
(`ReadAgentPageRequest/Response/Success/Failure/More/Floor`) rename; the
open's page arms repoint. Pure rename, no shape change.

### `OpenAgentSession` + `WatchAgentSession` land; `ReadPage` loses its first arm — OPEN answers "where am I", WATCH is a pure tail

**What changed (the user's factoring and names).**
`endpoint_open_agent_session.proto`: request { AgentId agent; page_size;
optional StoreItemPointer known_through }; success { AgentSessionPage page
(lines each with their pointer; boundary more|floor); AgentSessionToken
watch }. `endpoint_watch_agent_session.proto`: request { the token, echoed };
STANDING stream of `StoreLineAt` (one frame per written line, upserts
included). `ReadPageRequest.position` collapses to a required `after`
pointer — there is no first-page request semantics anywhere; the first page
is always the OPEN's answer.

**The design, in the user's terms.** Pages and watching are DECOUPLED: the
open is a bounded unary answer, the watch a pure tail addressed by an
opaque store-minted token — "a hash of the actual session identifier, so the
client MUST call open to subsequently watch" — which also pins the tail to
begin exactly after the page's newest item. `known_through` is the caller's
own high-water mark: UNSET = repaint (a reopened historical workspace paints
the first page whole); SET = catch-up after a shim bounce (the page carries
only newer items; a gap wider than page_size is walked older via ReadPage
until the caller meets its own mark). The store deliberately tracks NOTHING
about what it previously served. Every streamed line carries its pointer so
the caller always holds a current mark.

**REOPENED BY NAME, for their stages.** (1) Stage 4's `WatchTurn` "opens by
replaying from the turn's beginning" — under this pattern the shim boundary
gets the same open/watch split and the watch carries only new lines. (2) The
`agentrepl.v1` feed surface at the frontend remediation pass: open/resume
answers with the first page; `WatchFeed` becomes tail-only. The user: the
same pattern "should be paralleled at the shim boundary … and when the
frontend creates or resumes a workspace".

### The store becomes a Connect service; `ReadPage` lands — the first read verb

**What changed ("let's use Connect RPCs here as well"; "we can land the page
RPC").** NEW `store/v1/service.proto` (`service ShimStore`, callers the shim
and sidecar only) and `endpoint_read_page.proto`: request { AgentId book;
uint32 page_size; oneof position { first | next { StoreItemPointer after } } };
success { lines (newest first); oneof boundary { more { StoreItemPointer
last_item } | floor } }; failure arms derived. `StoreItemPointer` is opaque
and store-minted, stable across upserts because ORDER IS BY THE UNIT'S FIRST
INSERT, never its last write — a unit settling mid-walk cannot teleport
across a continuation.

**The user's pagination spec.** Page size rides the REQUEST so it can vary
across calls of one walk; `next` carries ONLY the pointer to the previous
page's last item, which the previous response's `more` arm served — no
memory of the previous page's size anywhere.

**The read inventory this opens** (names settled in conversation, shapes at
their turns): GetItem (by upsert_key: re-announcement, late-join),
GetRun (a run's frames by typed identity), GetLiveWork (non-terminal runs —
the ONLY producer of resume-time `live_work`, since the vendor's level is
per-process and empty at startup; audit E), GetLastPrompt (keep-alive
rollback point, may fold into others), GetCursors (already shaped),
GetSessionFacts (main_agent_id for resume). The old UDS Subscribe surface is
superseded.

### RETRACTION: `DetachedWorkId` STAYS — it is the uniform CONNECTION token, and the mapping is the producer's

**The verdict it retracts.** An earlier verdict in this stage (unlanded on
the wire; discussed while shaping the store) removed `DetachedWorkId` in
favor of addressing detached work by its underlying identity —
`AgentActivityId` for bash, `AgentId` for subagent and workflow.

**The user's ruling.** "The detached work id is returned because it makes
creating the subsequent connection from daemon to shim the same regardless
of the detached work (just check the id). If the producer maps to it
differently depending on the underlying message, that's fine, but it
shouldn't be the CONSUMER doing that." It is the typed-echo-token pattern:
the announcement serves the handle, the daemon echoes it to Watch/Stop, one
code path for every kind.

**What survives from the discussion.** `UpdateSubagent` keeps `AgentId` (a
prompt targets the AGENT, which may have no live run — addressing, not a
connection); the store's run wrappers keep their typed identities
(`StoreAgentBash.run` = `AgentActivityId`, `StoreAgentWorkflow.run` =
`AgentId`) because the store joins on identity; the shim owns the
handle↔identity mapping (one lookup, `task_started` carries both). The
"placement { foreground | detached }" settled-frame idea and the
`HistoryEntry` announcement-comment question remain open, unaffected.

### `store.v1` becomes ONE FILE (`store.proto`); the unserved oneof lands — keepalive beside the three residue arms

**What changed (the user's sketch).** `write.proto`, `entry.proto`,
`unsupported.proto` merge into `store.proto` (18 messages).
`StoreAgentUpdate.unserveable_frame` is replaced by `StoreUnservedItem
unserved_item { keepalive (StoreAgentItem) | StoreVendorSpecific |
StoreUnknown | StoreUnparsed }` — THE ARM IS WHY it cannot be served: no
book, or unconvertible. The residue bodies carry over verbatim under
`Store*` names; `UnsupportedEntry`-era wrappers stay dead.

**Consequences.** `top_level` is documented UNSET when unresolvable (an
unparsed record may name no agent). The residue thereby rides the same
envelope as everything else — one `upsert_key` space, one write path — and
the old separate `unconverted` table's reason to exist goes with it. STILL
HOMELESS, flagged not landed: the old `source_record` kept-whole field (a
faithful conversion that was nonetheless LESS than the source); the user has
not yet said where or whether it returns.

### `cursor.proto` folds into `write.proto`; `OpenTaskState` and the authoritative bool DIE

**What changed ("should this just be in write.proto?" — yes).** `CursorState`,
`CursorQuery`, `CursorList` move into `write.proto` under a banner —
one concern: how a producer writes and resumes; `EntryBatch` already embeds
the cursor and nothing else imported the file. `cursor.proto` deleted.
DELETED with it: `OpenTaskState` (a timestamp that could not name WHICH task
— live-work recovery now reads the run rows) and
`CursorList.open_tasks`/`open_tasks_authoritative` (an old-store
compatibility crutch, dead under nuke-never-migrate).
`CursorQuery.file_id` goes `optional` (empty-means-all was a sentinel).

**The high-level, settled first at the user's prompting.** The cursor is
invisible machinery whose UX is "after any crash or deploy, history has no
gaps and no repeated messages". It rides the BATCH, not the entry: one read
position yields many entries, it is a file bookmark rather than a
conversation fact, and stream-plane writes have no file to be positioned in.

### STAGE 5 OPENS: `StoreEntry` lands — the storage envelope around conversation.v1; pageability is the PRODUCER's arm; one opaque `upsert_key`

**What changed ("let's land this").** `store/v1/entry.proto` REWRITTEN:
`StoreEntry { Plane; write_id; upsert_key; oneof entry { StoreAgentUpdate |
conversation.v1.SessionUpdate } }`; `StoreAgentUpdate { AgentId top_level;
oneof agent_info { StorePageLine serveable_frame | StoreAgentItem
unserveable_frame | StoreAgentBash bash | StoreAgentWorkflow workflow } }`;
`StorePageLine { AgentId page_agent_id; StoreAgentItem }`; `StoreAgentItem
{ AgentPrompt | AgentFrame }`; the two run wrappers carry the run's identity
(bash: `AgentActivityId`; workflow: `AgentId`). `Entry`, `InternalEntry` and
the `shim.v1.ExternalEntry` import are DELETED; `Plane` keeps its two arms;
`write.proto`'s batch carries `StoreEntry`.

**The shape is the USER's sketch**, arrived at through: the store writes
conversation.v1 (not a flat vendor log); a page is the contiguous items of
ONE book; non-item frames (a bash update) must be structurally unable to
appear in a page, so PAGEABILITY IS DECIDED BY THE PRODUCER — a page line
NAMES its book (`page_agent_id`), unserveable material has no book, and run
frames are not page lines at all. The earlier `parent_item`/anchor
construction DISSOLVED when the user ruled each block its own feed item.

**`upsert_key`, the user's rule.** One opaque producer-minted key; the store
holds one row per key and a write supersedes it whole. "The store should
have a single place it looks for a given property, never multiple places for
a given column" — the mapping (TurnId, unit id, run id) is the shim's and
sidecar's, never the store's.

**`top_level`, the user's taxonomy.** The nearest NON-SYNC ancestor (main
agent or detached-work agent, never a sync subagent), copied from the
parent's row at insert.

**Dangling, named.** `unsupported.proto`'s three residue bodies and the old
`source_record` kept-whole field have no carrier on `StoreEntry` yet —
judged at `unsupported.proto`'s walk, not silently dropped. `OpenTaskState`
and the read verbs are `cursor.proto`'s walk. The store implementation's
schema (session_id/seq/top_level_message_id) is superseded wholesale under
nuke-never-migrate.

### `AgentPrompt` is the ONE form of a delivered prompt: returned by the delivering rpc, persisted by the store, replayed by history; `HistoryPrompt` deleted

**What changed ("looks great").** `conversation/v1/turn.proto` gains
`AgentPrompt { TurnId id; AgentId agent; UserSaid said }`. `HistoryEntry.user_prompt`
is retyped to it and `HistoryPrompt` is deleted. `StartTurnSuccess` wraps it;
`UpdateSubagentSuccess` and `UpdateWorkflowSuccess` become `oneof outcome
{ AgentPrompt prompt | *Delivered }`, mirroring the input arm.

**Why, in the user's terms.** The store entry sketch had an envelope `parent`
beside `AgentFrame.agent_id` — the user asked whether that was a different
agent; it was the same one, so the envelope duplicated the frame's own field.
The only frames with no agent were prompts and session updates. The user:
the rpc that sends a `UserSaid` should RETURN a message carrying TurnId,
AgentId and UserSaid, "then we can reuse that instead of HistoryPrompt, which
is a respelling of that info". So the delivered prompt is a conversation
fact with one form, and the store indexes `agent` straight out of the frame.

**Consequences.** `StoreEntry` carries no parent column at all: the agent is
read from `AgentFrame.agent_id` or `AgentPrompt.agent`, and a `SessionUpdate`
row is the main agent's. The shim must resolve the recipient agent before
answering `StartTurn`.

### `update` frames carry FRAGMENTS: `AgentResponseUpdate` and `AgentThinkingUpdate` retyped to deltas; the settled text rides the terminal arms only

**What changed ("looks good").** `AgentResponseUpdate { new_markdown }`;
`AgentThinkingUpdate.text` is NEW `AgentThinkingTextDelta { new_text }`;
`AgentResponseProse` and `AgentThinkingText` keep their shapes and are now
documented as the WHOLE text carried on the terminal arms only (success, and
failure's partial prose). The two `start` arms' comments, which justified the
empty arm by the old cumulative rule, are reworded.

**Why, in the user's terms.** Principle #3, ruling 3: the shim forwards what
CHANGED and accumulates nothing; gluing, coalescing or forwarding fragments is
the daemon's choice. This REOPENS the thinking/response entry's
"prose-so-far, not a delta" and supersedes it.

**No offset, the user's ruling.** A gap-detecting `from_offset` on the
`AgentBashUpdate` pattern was offered and refused: a lost fragment "is
evident in the response", and the terminal frame carries the whole text, so
the settled bubble self-corrects. Bash keeps its offset (its spool has no
settled whole to recover from).

**Consequences.** The daemon's feed resolver now owns the prose buffer per
in-flight unit (`FeedAgent.update` still carries prose-so-far to the webapp —
that is a daemon-resolved VALUE, unchanged); the shim's response accumulator
is deleted; `slash_command.proto`'s `ContextCompacted.summary` keeps
`AgentResponseProse` as a whole text, unaffected.

### STAGE 4 CLOSE-OUT SWEEP: the five old `shim.v1` files are DELETED; `ModelMarker` moves to `api.proto` — STAGE 4 (shim.v1) COMPLETE

**What changed ("looks good").** `core.proto`, `bookkeeping.proto`,
`external.proto`, `entry-delivery.proto`, `message-page.proto` deleted.
`ModelMarker` and its `model_marker_literal` extension move VERBATIM to
`conversation/v1/api.proto` beside `AgentModel` (a vendor value that is not a
model is an API fact; ten live consumers in daemon, shim and webapp). The
package is now exactly the file model: `service.proto`, eighteen
`endpoint_*.proto`, and `prompt_origin.proto` as the one shared file.

**The sweep, by disposition** (every declaration was enumerated to the user
and ruled on). `core.proto`'s 48 remaining declarations: the handshake
family → `StartSession`/`SessionStarted`; the turn family → `StartTurn`,
`UpdateTurn`, `KillTurn`; the detached family → the per-kind `Update*`/`Stop*`
and `SessionStarted.live_work`; the session family → `SetSessionModel`,
`WatchSession.query_died`, `GetSessionDiagnostics`; the transport family
(`Ack`, `Nack`, `ConnectionHeartbeat`, `Subscribe`, `Replay*`,
`PermissionRequest`/`Response`) DEAD under Connect, the Watch streams,
`AgentPermission` + `AgentInput`; `SessionRewound`/`KeepAliveDiscard`
shim-internal (Owed G). `bookkeeping.proto`'s ten arms dispersed as recorded
at WatchSession. The three flat-log files superseded by the datalayer/protocol
split, `AgentFrame`, and `ReadHistory`.

**Dark until their stages**: `store/v1/entry.proto` imported `external.proto`
and `state/v1/durable.proto` imported `core.proto` and `bookkeeping.proto`;
both packages no longer compile, by the land-whether-or-not-it-breaks rule,
and are walked at stages 5 and 6.

**Stage 4 is complete.** Remaining per the settled sequence: 5 (`store.v1`),
6 (`state.v1`), then the design-complete gate, the vetting register, and the
reconciliation handoff. The stage-2 and stage-3 repoints owed at item F
(`MessageId` → the new identities; `FeedPermission` and `AnswerPermission` →
`AgentPermission`/`AgentAnswer`) remain owed.

### 4c: `ReadHistory` lands — the HISTORY section; a page is ONE AGENT's children; the main agent has an identity and nothing is nil

**What changed ("great, let's ship").** NEW `conversation/v1/history.proto`:
`HistoryPage { repeated HistoryEntry entries (newest first); oneof boundary
{ more { HistoryContinuation } | floor } }`, `HistoryEntry { oneof entry
{ HistoryPrompt user_prompt | AgentFrame agent_frame } }`, `HistoryPrompt
{ TurnId id; UserSaid said }`, `HistoryContinuation { value }` (opaque,
echoed). `endpoint_read_history.proto`: request { AgentId agent; oneof
position { first | next(continuation) } }; success wraps the page; failure
arms derived (unknown agent, stale continuation, store unavailable).
`SessionStarted` gains `main_agent_id`. Eighteen rpcs; stage 4's four
sections are all landed.

**THE PAGE'S SCOPE IS THE AGENT, and that is the whole of placement.** The
orchestrator kept stamping each entry with a `TurnId`; the user: a history
page "can be requested for something that was NOT a turn" — a subagent, a
workflow's agent — and "the HistoryEntry already IMPLICITLY corresponds to an
agent, because we've specified the parent id." So an entry carries no
placement; the request's agent is it. Entries are `UserSaid | AgentFrame`,
the user's own shape.

**THE MAIN AGENT IS AN AGENT, the user's ruling, and nil dies.** Rather than
"parent nil means top level", the main agent has an `AgentId` like any other:
the store's parent column is never nil, a prompt's parent is always the agent
it was sent to, every frame's `agent_id` is real, and the request's container
collapsed from `top_level | parent(AgentId)` to `AgentId`.

**ROTATION, the user's worry, and the decoupling that answers it.** If the
main agent's id were the vendor session id, a rotation mid-page would split
the agent's history. So the two identities are DECOUPLED: `main_agent_id` is
OURS — shim-minted on the first fresh start, store-persisted, reported
unchanged on every later start — and `SessionIdentityRotated` changes only
the vendor handle the shim resumes by. A subagent's `AgentId` already behaves
this way (the vendor's `agentId` survives its runs). Shim-minted rather than
daemon-minted so a fresh daemon resuming an old store has one authority.

**THE PROMPT'S ID IS THE DAEMON'S, and nothing is reconciled.** The daemon
mints it at submission and returns it so the client draws the row at once;
on delivery the shim ADOPTS it (`StartTurnRequest.turn`) and writes the
record under it; history returns it as `HistoryPrompt.id`. The vendor's own
`uuid`/`promptId` stay in the store's internal half. Whether the type keeps
the name `TurnId` or becomes `PromptId` is an open naming question, separate
from history: under the parity principle the daemon mints one per delivered
prompt to ANY agent.

**Terminal arms ARE entries.** `AgentSuccess`/`AgentFailure` frames appear in
history; they are the only record of how a turn ended and the feed's stop
notice has no other source. No `start` is replayed: the settled frame carries
the start's facts by the upsert rule.

**OWED TO STAGE 5 (vetting register, Owed H).** The store scopes by the
LOGICAL session / main agent id, with the vendor session id as a mutable
attribute; the parent column is never nil; "N most recent children of agent
X" is an indexed query on `agent_id`, not the old ingest-time feed-row walk.

**Flagged, not modelled.** `PromptOrigin` on a history prompt (a merge-driven
prompt may draw differently; it is shim.v1 today and would have to move
down); whether an UNSETTLED unit (the turn died mid-unit) appears as its
last frame or is dropped.

### 4c: the SESSION and TURN sections RESTRUCTURED around spawn / attach / end — `StartSession`, `WatchTurn`, `KillSession`, `KillTurn`; `StartTurn` goes unary; resume recovers model and mode from the transcript

**What changed ("yes").** Renames: `OpenSession` → `StartSession`
(`SessionOpened` → `SessionStarted`), `CloseSession` → `KillSession`
(`SessionClosed` → `SessionKilled`), `SetModel` → `SetSessionModel`,
`SetPermissionMode` → `SetSessionPermissionMode`. `StartTurn` becomes UNARY
(success `{}`: the prompt was accepted); NEW `WatchTurn(TurnId)` is the turn's
stream, replaying from the beginning and refusing a concluded turn (that is
HISTORY's). NEW `KillTurn { TurnId; bool force }` → `TurnKilled { agent_only
| forced { stopped_work } }` / `TurnLive { live_work }`, in `turn.proto`.
`StartSessionRequest`'s model and mode move INTO the `fresh` arm; `resume`
carries only the id and the optional remediation; `SessionStarted` gains
`permission_mode`. Seventeen rpcs on the service.

**Stop ≠ Kill, the user's distinction.** `UpdateTurn.stop` interrupts the
main agent alone and leaves what it spawned running. `KillTurn` ends the main
agent AND everything the turn spawned, transitively — refusing while any of
that is live unless forced, naming it. `KillSession` ends everything live in
the process, whichever turn spawned it. VERIFIED at the type surface: the
vendor's `background_tasks_changed` IS the live set (so KillSession needs no
tracking), but nothing names a task's TURN — a task carries only its spawning
`tool_use_id`, and `parent_agent_id` beyond depth 1 — so KillTurn's refusal
list needs the shim to keep `task → spawning call → turn`. That is the ONE
bounded exception to "no tree anywhere": spawn provenance, kept for kill
purposes only, never on the wire except inside a refusal.

**RESUME RECOVERS MODEL AND MODE — a correction, kept visible.** The
orchestrator claimed permission mode was "not recoverable" on resume. WRONG:
every `user` record in the JSONL carries `permissionMode` at top level (4,722
occurrences in real sessions) and every `assistant` record carries `model`.
What IS true, observed in the probe: the SDK does NOT RESTORE either — a
resume with no options ran on the default model, not the session's
`claude-haiku-4-5`. So both are recorded, neither restored, and the shim reads
the last of each back and passes them; the answer reports what was recovered.
ROOT CAUSE: reading "not restored" as "not recorded". The user's instinct —
"the shim should know the model and permission mode" — was right.

**The model-switch rule, the user's aside.** `SessionCold.model_switch` fires
ONLY when the requested model differs from the transcript's last; a same-model
resume is judged by the lifetime alone, and within it nothing fires. Stated at
the arm.

**Recorded as a core principle** (section above): spawn / attach / end are
decoupled because sessions and turns outlive the daemon.

### 4c: `CloseSession` lands — refuses while anything is live unless forced; both outcomes NAME the work

**What changed ("yes land").** `session.proto` gains `SessionClosed { idle |
forced { optional interrupted_turn; repeated stopped_work } }` and
`SessionLive { optional turn_in_flight; repeated live_work }`.
`endpoint_close_session.proto`: request { bool force }; success wraps
`SessionClosed`; failure { oneof cause { SessionLive live }; detail }.

**The user's ruling.** Refuse while live, unless the request says force. The
daemon is the one decider of what dies; a close that cascaded stops would
hide three decisions inside one verb. `force` is a plain bool by the
`RestartWorkspace.force` precedent (no adjacent data changes meaning under
it).

**What the response carries that the bool does not.** A forced close NAMES
what it killed so the consumer concludes the elements it was drawing; a
refusal NAMES what is live so the daemon can stop selectively rather than
force blindly.

**Unverified, on the register as item 9**: whether the vendor's background
tasks survive the query closing. If a backgrounded shell is a child of the
agent binary, a non-forced close with live work is the only safe answer and
"refuse" is exactly right; if they survive, the next OpenSession would report
them and a close-with-live-work could be legal.

**The SESSION section is complete**: OpenSession, WatchSession, SetModel,
SetPermissionMode, GetSessionDiagnostics, CloseSession. Remaining in stage 4:
`WatchTurn` (TURN reattach) and the HISTORY section.

### 4c: `GetSessionDiagnostics` lands — the shim's health is PULLED ("you can land it yourself")

**What changed.** `session.proto` gains `SessionDiagnostics { oneof health
{ healthy | unhealthy { repeated SessionFault } }; repeated
SessionDegradedWindow }`, with `SessionDegradedWindow { component; reason;
began_at_ms; oneof extent { open | closed { ended_at_ms; dropped_count } } }`.
`endpoint_get_session_diagnostics.proto`: empty request; success wraps the
diagnostics; unhealthy is an ANSWER inside success, per the domain-outcome
rule. `SessionFault` follows `DaemonFault`'s discipline: `{ component; detail }`
now, a `kind` oneof added with its first derived arms at the wave.

**Why a window with an extent oneof.** The old `DegradedState` carried
`recovered` (bool) beside `dropped_count`, which is meaningless until
recovery — the mode-selecting-bool defect. The closed arm owns the count.
Windows are kept since the shim started, so a daemon asking after the fact
still learns what was dropped.

### 4c: `SetModel` and `SetPermissionMode` land; a model switch is a COLD CACHE and shares OpenSession's remediation vocabulary

**What changed (the user's spec for SetModel; SetPermissionMode "you can
handle yourself").** `endpoint_set_model.proto`: request { AgentModel;
uint64 cold_threshold_tokens; optional SessionColdRemediation }; response
{ success {} | failure { oneof cause { SessionCold cold }; detail } }.
`endpoint_set_permission_mode.proto`: request { AgentPermissionMode };
response { success {} | failure { detail } }. `SessionUpdate` gains
`permission_mode_changed` (a standing grant's `set_mode` can change it too, so
one authoritative statement is needed). `SessionCold`'s comment now names the
second way a context goes cold: the model changing, since a cache is per
model.

**SetModel's two rules, the user's.** (1) It RESOLVES AFTER THE CURRENT TURN
ENDS — a turn is answered by one model throughout; with no turn open it
resolves at once. This departs from the SDK's mid-turn `setModel` on purpose:
the vendor permits it, the product does not want it. (2) It is REFUSED
IMMEDIATELY — before any waiting — when the context exceeds a threshold THE
REQUEST NAMES, because the switch re-reads everything at full price; the
failure carries `SessionCold` and the retry names a `SessionColdRemediation`.
Same messages as OpenSession on both sides, same shim handling.

**Why the threshold is in the request.** It is daemon policy (cost
tolerance, user preference) stated per call, not a shim constant; the shim
only measures.

**A PROCESS ERROR, recorded.** The orchestrator landed SetModel on reading the
user's spec as agreement; the user had approved only SetPermissionMode. The
landed text was shown back point by point and approved after two amendments:
`SetModelSuccess` wraps `SessionModelChanged` (the conversation.v1 fact, per
the extraction rule applied to every session rpc), and `SessionCold` gains
`oneof reason { lapsed { cache_ttl_ms } | model_switch }` — the TTL was
meaningless for a model switch, an adjacent-exclusivity defect caught on
reading the landed text back. ROOT CAUSE: a spec stated in prose was taken as
the agreement the sketch still owed.

### 4c: `WatchSession` lands — `SessionUpdate` in `session.proto`; `BookkeepingEntry` is DEAD, arm by arm; shim diagnostics become a PULL

**What changed (the user's two corrections, then land).** `session.proto`
gains `SessionUpdate { identity_rotated | query_died | model_changed |
fast_mode | mcp_server | account_usage }` and its arm messages.
`endpoint_watch_session.proto`: empty request; standing stream wrapping
`SessionUpdate`. No diagnostic arm.

**"IS WATCHSESSION JUST A RESPELLING OF BOOKKEEPING?" No — and the user's
sharper claim holds: there is no third category.** Every fact is either about
a TURN (its stream says it) or about the SESSION (this stream says it). The
old `BookkeepingEntry`'s reason to exist — "retrieved by seq range, never
counted in a page" — was a workaround for a flat log where a page counted
records; a page now counts UNITS and the store persists whatever streams
carry, so the special retrieval path dies with seq. The ten arms, each with
its equivalent: `session_began` → OpenSession success; `session_ended` →
CloseSession success or `query_died`; `turn_began` → StartTurn's stream
opening; `turn_ended` → StartTurn's terminal frame (no `unexplained`
successor); `heartbeat` → none (the open-stream set); `response_timing` →
none (derived from the response unit's frame instants); `producer_diagnostic`
→ a pull rpc; `session_identity_changed` → `identity_rotated`;
`account_usage_observation` → `account_usage`, no longer turn-pinned;
`response_usage_corrected` → none (the next frame IS the correction).

**TWO CORRECTIONS.** (1) `query_died` is DUPLICATED on purpose: each open
stream (a turn's, a subagent's, a run's) also concludes with its own failure,
but a consumer with no stream open still needs the session-level fact — the
same reasoning as `model_changed` being stated even when the consumer asked
for it. (2) DIAGNOSTICS ARE PULLED. The user: this stream "is for notable
information as it manifests, and this is a synthetic manifestation" — the shim
intermixing upstream happenings with periodic self-reports at its own cadence.
So the arm is dropped and inventory item 6 `CheckHealth` becomes
`GetSessionDiagnostics`, one pull returning health and degraded windows.

**Dropped from the old usage observation, each by name**: `query_instance_id`
and `turn_id` and the turn-boundary oneof (no longer sampled at boundaries),
`sample_latency_ms` (nothing draws it). Only the five-hour window is carried,
because only it was ever sampled; the footer's WEEKLY allowance has no
producer here — flagged.

**Kept as verbatim vendor strings, vocabulary not in evidence**: the
rotation `reason`, `subscription_type`, fast-mode `reason`.

### 4c: `OpenSession` lands — the SESSION section opens; `conversation.v1/session.proto` holds the opened-session and cold-context vocabulary

**What changed ("everything looks good", with the extraction).**
`endpoint_open_session.proto`: request { oneof source { fresh | resume
{ vendor_session_id; optional SessionColdRemediation } }; AgentModel model;
AgentPermissionMode permission_mode }; response { success { SessionOpened }
| failure { oneof cause { SessionCold cold }; detail } }. NEW
`conversation/v1/session.proto`: `SessionOpened` (vendor id, runtime,
effective model, catalog, turn in flight, live detached work),
`SessionRuntime`, `SessionCold` (context tokens, last request instant, the
TTL that lapsed, requested model), `SessionColdRemediation { pay | clear |
compact { model; scope { full | prompts_only | responses_only |
prompts_and_responses } } }`.

**THE HIGH-LEVEL CONTRACT, worked out before any shape at the user's
instruction.** OpenSession RESOLVES WHEN A PROMPT CAN BE ACCEPTED. It returns
only facts fixed at handshake; anything that changes later is WatchSession's.
And a COLD CONTEXT IS REFUSED, NEVER SILENTLY PAID: the shim must not load a
lapsed context into the model unasked, so a bare resume of a cold session
fails with the cost, and the daemon reopens naming a remediation.

**VERIFIED EMPIRICALLY, because the protocol rests on it.** (1) `query({resume})`
makes NO API call and emits NOTHING — a resumed probe session sent no prompt
for 15s and produced zero messages, not even `system/init`; `init` arrived
only WITH the first prompt, and the first assistant usage was `cache_read 0,
cache_creation 23,683` (the cold read, landing on the first prompt). So the
shim can refuse before any cost, and "can accept a prompt" is the SHIM's
readiness, not a vendor signal. (2) Compaction is therefore a cold read PLUS
output, never the cheap path; its payoff is later turns, and the comment says
so. (3) `SESSION_SOURCE_COMPACT_CONTINUE` was ours (one daemon reference,
`metaprompt.go:16`), not a vendor mode — the SDK has only `resume` and
`continue` (most-recent-in-cwd, which the daemon never needs); the source
oneof is `fresh | resume`.

**COMPACTION IS OURS, the user's design.** Daemon-directed, shim-implemented:
the request names the MODEL that writes the summary and the SCOPE (four arms);
the shim runs a throwaway session that summarizes, then initializes the real
session from the compacted transcript. Owed (vetting): whether the SDK can
initialize a session from a transcript we wrote (`sessionStore` is `@alpha`;
`forkSession` and writing the binary's JSONL are the other candidates), and
whether our bookkeeping survives the vendor's `compact_boundary` format.

**KEEP-ALIVES BEGIN BEFORE SUCCESS IS RETURNED — a producer obligation
stated at the message.** A warm context stays warm while the user types; a
`pay` resume takes its cold read at open, in the background, rather than on
the first prompt. The CADENCE has started; the read need not have finished.

**WHY THE FIELDS LIVE IN conversation.v1.** The user: these "are going to
become externally-facing conversation messages" — a cold context is a warning
the USER sees and a remediation the USER chooses, carried back through the
daemon into the next open. So the shapes are conversation vocabulary and the
endpoint wraps them whole.

**Dropped from the old handshake, each by name**: `from_seq` /
`query_created_seq` (no seq on the wire), `protocol_version` /
`daemon_version` (the package version is the protocol), `query_instance_id`
(the query is shim-internal), fast mode and the cache-evidence fingerprints
(they change — WatchSession), `turn_in_flight` bool + `active_turn_ids` (one
turn, typed).

**Inventory consequence.** `WatchTurn(TurnId)` is REQUIRED to reattach to a
turn in flight after a daemon restart; added to the TURN section's inventory.

### 4c: the DETACHED WORK section lands — seven bespoke rpcs; `AgentFrame` carries `agent_id`; a workflow's update carries its agents' FRAMES

**What changed ("that looks mostly correct", with three corrections).**
`service.proto` gains the section: `WatchSubagent`/`UpdateSubagent`,
`WatchBash`/`StopBash`, `WatchWorkflow`/`UpdateWorkflow`, `DetachForeground`,
one endpoint file each. Every Watch wraps its kind's frame whole
(`AgentFrame`, `AgentBash`, `AgentWorkflow`); `UpdateSubagent` and
`UpdateWorkflow` carry `AgentInput`; `StopBash` carries nothing but the
handle; `DetachForeground` names the turn unit by `AgentActivityId`.
`AgentFrame` gains `agent_id`, which LEAVES `AgentActivity` (tag 17
retired), `AgentQuestion` (tag 4) and `AgentPermission` (tag 1).
`AgentWorkflowUpdate`'s second arm is `AgentFrame agent_frame` (tags 2 and 3
retired).

**WHY BESPOKE RPCS PER KIND, the user's ruling.** "The update api changes
depending on that (you can send a prompt to a detached agent, but you can't
send one to bash)." A generic update would have had arms that half-apply.

**THE THREE CORRECTIONS, each following from the frame being the shared
currency.** (1) `UpdateWorkflow` carries `AgentInput`, not `AgentAnswer` —
same idea as `UpdateSubagent`; how a prompt lands on a run is the shim's.
(2) A workflow's update carries `AgentFrame`, not `AgentUpdate` — so a
workflow agent FINISHING (its `AgentSuccess`) finally has an arm, which bare
activity never gave it. (3) `agent_id` rides `AgentFrame`, because the frame
is now what the turn, the subagent stream and the workflow's update all carry;
putting it on the activity (the earlier ruling) predated the frame existing.
Attribution moved up one level, once, and the flat-frames guarantee is
unchanged.

**THE ADDRESS SPLIT.** WATCH by `DetachedWorkId`, because a stream is one RUN
and the handle names the run. UPDATE a subagent by `AgentId`, because a prompt
may target an agent whose run has finished and then no run handle exists; a
stop or answer to an agent with no live run is a refusal. Bash and workflow
never outlive a run, so both their verbs take the run handle.

**Stated at the Watch responses, because a late joiner depends on it.** Every
stream opens with the unit's `start` repeating the ORIGINAL instant; a prompt
to a finished subagent starts a new run, announced on the prompting stream and
followed by a new `WatchSubagent`.

**Stage 4's TURN and DETACHED WORK sections are complete.** Remaining: SESSION
and HISTORY.

### The workflow's FRAME moves into `agent.proto`; its update carries `AgentUpdate`; success gains `interrupted`; the announcement carries the START description only

**What changed ("UpdateWorkflow needs to carry AgentUpdate, of course";
"AgentWorkflowUpdate and the high-level messages should be in agent.proto").**
`AgentWorkflow`, `AgentWorkflowUpdate`, `AgentWorkflowSuccess` (now `oneof
outcome { completed { optional summary } | interrupted }`), `AgentWorkflowFailure`
and its two cause arms move VERBATIM (bar the two shape changes) from
`workflow.proto` to `agent.proto` under their own banner. `AgentWorkflowUpdate`'s
activity arm (tag 2) is RETIRED; tag 3 `agent_update` carries `AgentUpdate`.
`DetachableWork.workflow` is retyped `AgentWorkflow` → `AgentWorkflowStart`.
`workflow.proto` keeps the run's DESCRIPTION (start, script, placement, notice,
summary) and imports only `agent_activity.proto`.

**THE CYCLE, which forced the file move.** `AgentUpdate → AgentDetachedWork →
DetachableWork.workflow → AgentWorkflow → AgentWorkflowUpdate → AgentUpdate` is
a genuine type recursion (a workflow's agents are agents), and protobuf permits
recursive messages but not cyclic imports. The user placed the frame-level
workflow messages in `agent.proto`, beside every other stream vocabulary.

**WHY THE ANNOUNCEMENT CARRIES ONLY THE START.** A workflow is never carried
by a turn; a spawning stream only ever ANNOUNCES one, and its frames arrive
solely on `WatchWorkflow`. So the `DetachableWork` arm names the description,
not the frame — which is also what breaks the import cycle at the right seam.

**Why `AgentUpdate` and not `AgentActivity`.** A workflow's agent hits the
permission gate and may pose questions; with bare activity those had no arm
and `UpdateWorkflow.answer` would have had nothing to answer. And
`interrupted` exists so a stop has a terminal arm to conclude on, as bash and
the agent frame already do.

### `AgentInput { stop | answer | prompt }` lands — the ONE write vocabulary for any live agent; `UpdateTurn` carries it

**What changed ("let's use the same api for the subagents").** `agent.proto`
gains `AgentInput` and `AgentStop`; `UpdateTurnRequest` is `{ TurnId; AgentInput
input }`. The core principle made literal: `UpdateSubagent` will carry the
identical type, differing only in its address.

**WHAT A SUBAGENT IS, verified, because the principle needed it.** At the type
surface (`AgentInput` in sdk-tools, `task_notification`) and in 231 observed
`SendMessage` results: a subagent is spawned by an agent's `Agent` call with a
commission; it runs that commission to COMPLETION and the task ends; it
persists only as transcript + identity; the next message RESUMES it as a new
run ("had no active task; resumed from transcript", 78×). So it is analogous
to a turn — one run per input. The ONLY input route to it is another agent's
`SendMessage` tool; no SDK route lets a human address a subagent directly, so
"click a subagent and prompt it" has no vendor path today and the shim must
relay or drive the resume itself — recorded with item 7.

**THE ONE ASYMMETRY, and how it is absorbed.** A LIVE subagent accepts a
message at its next tool round without being stopped (observed); whether the
main agent does is UNVERIFIED (vetting item 7). The user's ruling: keep one
API and resolve the inconsistency INSIDE THE SHIM — `prompt` to a turn and to
a subagent may land differently, and the consumer never learns which.

### `AgentFrame` and `AgentAnswer` extracted; `StartTurn`/`UpdateTurn` renamed; the permission gets its OWN identity; question answer types renamed

**What changed ("yep agreed"; "update the names as you see fit").**
`agent.proto` gains `AgentFrame { update | success | failure }` and
`AgentAnswer { question_answer | permission_decision }`. `StartTurnResponse`
(was `SubmitPromptResponse`) wraps `AgentFrame` whole; `UpdateTurnRequest`
(was `UpdatePromptRequest`) is `{ TurnId; oneof { interrupt | AgentAnswer
answer } }`. `permission.proto`: `AgentPermission.id` is NEW
`AgentPermissionId`, and the gated tool unit is an explicit `gated_call`
(`AgentActivityId`); `AgentPermissionDecision` lands as the write-back.
`question.proto`: `AgentQuestionAnswered` → `AgentQuestionAnswers`,
`AgentQuestionAnswer` (per question) → `AgentQuestionSelection`, and NEW
`AgentQuestionAnswer { ask; answers }` is the write-back.

**WHY THE FRAME ONEOF MOVED INTO conversation.v1, reversing the previous
entry's split.** The user, on the detached-work inventory: every watch was
described as "same as X's own ABC", and "this to me screams extraction". The
one thing repeated was the result oneof, so it is declared ONCE per kind —
`AgentFrame` for agents; `AgentBash` and `AgentWorkflow` already are theirs —
and every stream response wraps its kind's frame whole. One handler per kind.

**THE RENAME, the user's.** `SubmitPrompt`/`UpdatePrompt` named the prompt;
the rpcs act on the TURN. `StartTurn` opens it (the stream is the turn),
`UpdateTurn` writes to it. `agentrepl.v1.SubmitPrompt` keeps its name: there
a user really submits a prompt and the daemon decides what it becomes.

**THE PERMISSION IDENTITY, the user's second catch.** `AgentActivityId` was
doing two jobs on the permission — its identity and the join to the gated
tool unit. "It isn't an activity arm anymore." Split: the identity is the
permission's own, the join is an explicit field. Names are premises.

**Question naming collision, resolved.** The landed `AgentQuestionAnswered
{ repeated AgentQuestionAnswer }` used "answered"/"answer" for batch/element,
leaving no name for the write-back. Renamed top-down: `Answers` (batch),
`Selection` (one question's choices), `Answer` (the write-back).

**Superseded within this landing.** `UpdateTurnRequest`'s inline `{ interrupt
| answer }` oneof is replaced by `AgentInput` at the next increment, per the
core principle above.

### 4c: `UpdatePrompt` lands — THREE actions, not two; `AgentQuestionId` is the question's own identity

**What changed ("let's land after those changes").**
`endpoint_update_prompt.proto`: request { TurnId; oneof action { interrupt |
answer_question { AgentQuestionId ask; AgentQuestionAnswered answered } |
answer_permission { AgentActivityId ask; oneof decision { AgentPermissionAllowed
| AgentPermissionDeniedByUser } } } }; response { success {} | failure
{ detail } }. `rpc UpdatePrompt` on the service. In `question.proto`,
`AgentQuestion.id` is retyped from `AgentActivityId` to NEW `AgentQuestionId`.

**Three arms, reopening 4b's two.** 4b settled `{ interrupt |
answer_permission }` when a question was still a tool riding the permission
gate. Today's split of question from permission makes their answers distinct
shapes, so the oneof gains an arm rather than overloading one.

**THE IDENTITY CORRECTION, the user's catch.** "AgentActivityId in the ask is
weird to me. question asking isn't in agent activity anymore, no?" Correct —
the name says "names WORK", and a question is no longer work. The two blocking
kinds differ: a PERMISSION's id IS a unit-of-work id on purpose (consent joins
to the tool unit it gates, which carries the same identity if the gate opens),
so it keeps `AgentActivityId`; a QUESTION joins to no unit, so it gets its own
space. Names are premises: a wrong one here would have had a consumer looking
for a tool unit that never comes.

**Reuse over respelling, twice.** The question answer carries
`AgentQuestionAnswered` WHOLE — the same type the success frame restates, so
the shim validates by comparing the echo against the batch it is already
holding. The permission decision carries `AgentPermissionAllowed` (once |
standing) and `AgentPermissionDeniedByUser` directly, minus the two arms no
user can say (`abandoned`, `policy`).

**Success is EMPTY on purpose.** The ask's new state arrives as its next frame
on the turn's stream; restating it in the unary response would be a second
authority. Failure arms are derived at the wave (no turn open, wrong turn, no
such ask, already decided, shape mismatch).

### 4c: `SubmitPrompt` lands — the first shim.v1 rpc; `prompt_origin.proto` split out; the keep-alive leaves the shim API entirely

**What changed ("looks great").** `shim/v1/service.proto` with its TURN section
and `rpc SubmitPrompt(SubmitPromptRequest) returns (stream SubmitPromptResponse)`.
`endpoint_submit_prompt.proto`: request { TurnId; UserSaid; PromptOrigin };
stream frame `oneof result { conversation.v1.AgentUpdate | AgentSuccess |
AgentFailure }`. `PromptOrigin` moves VERBATIM from `core.proto` to
`prompt_origin.proto` (an enum of send sites, no exclusive siblings, needed by
SESSION's bookkeeping too), MINUS `PROMPT_ORIGIN_CACHE_KEEP_ALIVE` (tag 27
retired).

**ONE TURN, STRUCTURALLY.** The user: "There is only one turn in flight,
conceptually, and thus there should only be one turn connection between
daemon<->shim structurally." The rpc comment states it: a second call while a
stream is open is a daemon fault, refused, never queued.

**THE FRAME IS conversation.v1's OWN VOCABULARY.** Nothing shim-specific rides
the turn: the three arms are `agent.proto`'s, shared with a detached
subagent's stream, so one consumer handler serves both.

**THE KEEP-ALIVE IS NOT A PROMPT THE DAEMON SUBMITS — reaffirming 4b, after a
reopen the user considered and declined.** He first proposed a `keep_alive`
request arm (and then a separate rpc) so the daemon never treats it as a
prompt with dynamic text. The orchestrator pointed out that 4b had already
settled keep-alives as ENTIRELY INSIDE THE SHIM — the daemon never submits
one, so nothing keep-alive-shaped is on the wire at all, which satisfies the
goal more strongly. The user: "entirely in the shim is fine too. it's really
its responsibility to know what the vendor requires." So the enum value goes,
and no rpc is added.

**REQUIREMENTS STATED WITH IT, recorded as owed (vetting register, Owed G).**
(1) Keep-alive turns must be FIRST-CLASS IN THE STORE as never-served: indexed
so no page returns them and no activity is routed to the daemon. (2) A real
prompt must ROLL BACK context to just after the last real prompt, so the next
turn does not build on keep-alive context; `SessionRewound` +
`KeepAliveDiscard` already claim this, and its reliability is a vetting item.
(3) The shim determines the keep-alive prompt text under the hood.

**Superseded by this landing.** `core.proto`'s `SubmitPrompt` message
(`request_id`, `text`, `origin` string, `permission_mode`) — permission mode is
SESSION state, request ids are Connect's, and text became `UserSaid`.
`bookkeeping.proto`'s `TurnEnded.unexplained` has no successor: a stream
ending without a terminal frame IS the transport failure. Both die when their
files are walked.

### `session_command.proto` → `slash_command.proto`; `context_cut.proto` folds into it; `ContextCompacted.summary` retyped

**What changed (the user's proposal, "adapt it so it fits").** File renamed.
`ContextCut`, `ContextTokenDelta`, `ContextCleared`, `ContextCompacted` move
in verbatim under a banner; `context_cut.proto` deleted. The one adaptation:
`ContextCompacted.summary` was `AgentContent` (a deleted record type) and is
now `AgentResponseProse` — the summary is the agent's markdown prose, the
same type every response unit carries. `daemon_hold.proto`'s import repointed.

**Why, in the user's terms.** A context cut "is really for wrapping the /clear
command and providing a daemon-synthesized handling for it" — so every
slash-command wrapper lives with the command set. One refinement stated in the
header: the binary also compacts AUTOMATICALLY, so `ContextCut` is the outcome
of the context-reshaping command family by ANY trigger, which is still that
family's concern.

**Consequences.** `slash_command.proto` now imports `agent_activity.proto`
(no cycle: nothing in the activity file imports it). The ContextCut producer
question from stage 1 stands unchanged (what the binary writes on `/clear`
decides whether `tokens_after` can be filled honestly).

### `question.proto` — the `AgentQuestion` family leaves `agent_activity.proto`

**What changed ("shouldn't it be extracted to question.proto?").** The seventeen
`AgentQuestion*` messages move VERBATIM to `question.proto`, imported by
`agent.proto`. No shape change. Same rule as `permission.proto`: a unit that
rides `AgentUpdate` directly rather than the activity envelope is its own
concern and its own file. `agent_activity.proto` now holds only what rides
`AgentActivity.item`, plus the identities.

### `AgentUpdate` becomes the CONSUMER-OBLIGATION oneof: question and permission leave activity; the PERMISSION GATE lands (`permission.proto`)

**What changed ("looks good").** `AgentUpdate` = { activity | detached_work |
question | permission }. `AgentQuestion` leaves `AgentActivity.item` (tag 4
retired) and gains `agent_id` + `id`. NEW `permission.proto`: `AgentPermission
{ agent_id; id; start | success | failure }` — start carries the vendor's
rendered prompt (title / display name / description), an optional trigger
(blocked path | ask rule | note), the offered STANDING as a typed echo token,
and the start instant; success is { allowed { once | standing } | denied
{ by_user | by_policy } | abandoned }. The standing's change vocabulary
(add/replace/remove rules, set mode, add/remove directories, destination,
behavior) is typed whole from the vendor's `PermissionUpdate` union.

**THE USER'S PRINCIPLE, which reorganizes the top level.** The routing oneof's
arm is the consumer's OBLIGATION: activity is READ-ONLY ("this is what
happened"); detached work means OPEN a stream; a question and a permission mean
the agent is BLOCKED and the user must WRITE BACK on the update channel — a
choice, or consent. "So they are all semantically unique wrt what the
subsequent push interaction should/can look like from the daemon to the shim."
Hiding the two blocking kinds inside activity made the read-only arm lie.

**ARE PERMISSION AND QUESTION THE SAME THING? At the SDK, yes; in meaning, no.**
VERIFIED at the type surface (`sdk.d.ts:206-266`, `PermissionResult` at 2114,
`SDKPermissionDeniedMessage` at 4166) and in the shim's `canUseTool`
(`session.ts`): ONE gate exists, every tool passes through it, and
`AskUserQuestion` is a tool whose answer is an `allow` carrying `updatedInput`.
So the mechanism is shared; the MEANING is not — for a question the gate is the
answer's transport and "allow" means nothing as consent; for every other tool
the gate IS consent. Two kinds, one producer path. The user ruled them
different messages.

**Identity is carried by each blocking kind, NOT hoisted.** Outside the
activity envelope the two units need `agent_id` (a subagent asks too, and the
answer routes to it) and an ask identity. The user: option (a) "is needed not
just preferable", because `AgentActivity` keeps its own `agent_id` (the
workflow's update carries activity directly) and `detached_work` is keyed by a
different identity, so nothing can sit above the oneof once.

**The gate is NOT the tool's outcome.** An allowed tool then runs as an
ordinary activity unit under the SAME identity (the `tool_use_id`-sourced one);
a denied tool never starts and has no activity frames. So the eleven tool
families are untouched, and consent joins to work by identity.

**Facts from the SDK carried into the shape.** The vendor RENDERS the prompt
sentence (`title`, `displayName`, `description`), so no consumer composes one
from tool+arguments. `suggestions` is a classic echo token — the vendor mints
what "always allow" means and a standing grant returns it verbatim. A policy
denial (`system/permission_denied`: classifier, `dontAsk`, deny rule) is a
denial with NO open ask, hence `denied.policy` distinct from `denied.user`.
`matchedAskRule` marks a rule-forced prompt hosts must not auto-approve.

**Out of scope, named.** Allow-with-edits (`updatedInput` on an ordinary tool):
write/edit already expose `user_modified`, and the answer verb is stage 4's.
The permission MODE as SESSION state (setPermissionMode) is the SESSION
section's; it appears here only as a value inside an echoed standing.

**Consequences.** `frontend.v1`'s `FeedPermission` and
`agentrepl.v1.AnswerPermission` repoint to this unit at their turns (the
answer's arms — once / standing / deny / question answers — now have a typed
source). The shim must emit `AgentPermission.start` from `canUseTool` and
`success` from its own resolve, and `denied.policy` from the system message.

### `conversation.v1` REORGANIZED by concern: `agent.proto` (the stream's three arms, with `AgentSuccess`/`AgentFailure` NEW), `detached_work.proto`, `workflow.proto`; `ApiRequestFailed` finds its home

**What changed ("let's land").** Four files now carry the protocol model.
`agent.proto` holds `AgentUpdate` (moved verbatim) plus NEW `AgentSuccess
{ completed { optional answer } | interrupted }` and `AgentFailure
{ api_request_failed }`. `detached_work.proto` holds the eight `Detached*`
messages verbatim; `workflow.proto` holds the twelve `AgentWorkflow*`
messages verbatim, imported by `detached_work.proto` for `DetachableWork`'s
third arm. `agent_activity.proto` keeps `AgentActivity`, the identities, and
every other unit family; its stale header (which still described the
recursive arm and said the terminal frames live in shim.v1) is corrected.
`api.proto` is unchanged — the user judged it fine.

**WHY THE TERMINAL ARMS MOVE INTO conversation.v1, which REOPENS amendment
(5) of the turn-update-model entry.** That entry put success and failure on
the rpc's response in shim.v1. The user's observation: `agent_activity.proto`'s
top-level message was `AgentUpdate`, which makes a reader EXPECT `AgentSuccess`
and `AgentFailure` beside it — and `ApiRequestFailed` had nowhere else to go.
The resolution keeps both decisions true: the three arm MESSAGES live here, as
the frame vocabulary of ANY agent's bounded stream (the main agent's turn and a
detached subagent's stream alike — the agnosticism settled at the rename), and
the `oneof result` that selects among them is each rpc's stream frame in
shim.v1. So shim.v1's `SubmitPrompt` and the detached-work stream will both
wrap these, and a consumer handles a turn and a subagent stream with one switch.

**`ApiRequestFailed` IS AN AGENT-LEVEL FAILURE, not a response unit's.** The
orchestrator had proposed folding it into `AgentResponseFailureReason`; the user
placed it on `AgentFailure` — the vendor refusing a request ends the AGENT's
stream, it is not a property of one prose block. `feed.proto`'s earlier fold of
the API failure into `FeedAgent.error` is re-judged at its repoint.

**Two shapes from the withdrawn SubmitPrompt sketch that survive here.**
`AgentCompleted.answer` names the answering response (finality stated by the
producer that knows it, never derived from position; optional because a
refusal or ceiling may leave no prose). `AgentInterrupted` is the acknowledged
user stop, an accusation needing evidence, as `TurnInterrupted` already was.
`TurnEnded.unexplained` has no successor: under the bounded-stream convention a
stream ending without a terminal frame IS the transport failure, recorded by
the daemon on its own side.

**WITHDRAWN, and why.** The sketch's `permission_ask` stream frame. The user
could not see what it did or why it sat beside `AgentActivity`; the answer is
that awaiting permission is a STATE OF THE TOOL UNIT, so it belongs in the
unit lifecycle here, not as a shim.v1 sibling. That gap (the vendor's
`canUseTool` gate is not a transcript block, so no arm announces it) is still
OPEN and is the next conversation.v1 increment before stage 4 resumes.

**Corrected, on observed evidence.** The token-usage entry claimed "there is NO
thinking-token field inside `usage` at all" — read off the type surface. Real
transcripts carry `usage.output_tokens_details.thinking_tokens` (324 non-zero
of ~73k usage objects), so `TokenUsage.output_thinking_tokens` HAS a billed
producer and stands. Owed item D shrinks to the live ESTIMATE channel
(`system/thinking_tokens`) if anything ever draws it. Also observed and NOT
modelled, by the user's ruling that `api.proto` is fine: `cache_creation`'s
5m/1h split, `cache_missed_input_tokens` + `cache_miss_reason`,
`iterations[]`, `server_tool_use`, `service_tier`, `inference_geo`, `speed`.
Recorded so the survey is not re-purchased.

**Import graph now**: `agent` → `agent_activity`, `api`, `detached_work`;
`detached_work` → `agent_activity`, `workflow`; `workflow` → `agent_activity`;
`agent_activity` → `api`, `content_blocks`. No cycles. Walk order top-down:
`agent` → `detached_work` → `workflow` → `agent_activity` → leaves.

### The OLD record model is DELETED from `conversation.v1`: `agent.proto`, `tool_call.proto`, `detached_work.proto`, `message.proto` are gone

**What changed ("let's delete").** Four files deleted. `ToolResultContent` and
`ToolResultContentBlock` — the only two messages `agent_activity.proto`
still used from `tool_call.proto` (the unmodeled tool's typed result) — move
VERBATIM into `agent_activity.proto` under their own banner; that file now
imports `content_blocks.proto` directly. Everything else in the three files
was the record model the protocol model replaced: `AgentSaid`/`AgentContent`/
`ContentArriving`/`ThinkingBlock`, the thirteen `ToolCall*` arms,
`ToolReturned`, the `Permission*` family, and the whole old `DetachedWork*`
lifecycle. `message.proto` the user deleted by hand as junk.

**Why now, in the user's terms.** Stepping back from stage 4: the orchestrator
sketched `SubmitPrompt` and the user could not see what the sketch's
permission-ask frame was doing or why it sat beside `AgentActivity`, and
questioned whether detached work belonged inside the activity envelope at all.
Both questions are about the UNIT LIFECYCLE in `conversation.v1` being
incomplete, and dead files were obscuring what the new model actually holds.
Stage 4 is not entered until `conversation.v1` is clean and whole.

**What is NOT reopened.** `context_cut.proto` stays, dangling on the deleted
`AgentContent`: it models DAEMON post-processing of shim data, which is a later
increment, not a stage already owed. `api.proto` stays and is walked next — it
is a shim-dependency file and should already be fully accounted for.

**Homeless after this deletion, to be judged one at a time (not ported).**
`StopReason` (the vendor states it; no unit arm carries it today), the
permission GATE as a state of a tool unit (the vendor's `canUseTool` is not a
transcript block, so no arm announces "awaiting permission"), and
`ApiRequestFailed`'s placement in the activity vocabulary.

**Consequences.** `frontend/v1/feed.proto` and `footer.proto`,
`agentrepl/v1/endpoint_get_feed_page/interrupt/answer_permission.proto` and
`shim/v1/external.proto` import deleted files — the stage-2/3 repoints already
owed as vetting item F, now widened to these types. `AnswerPermission`'s
`ToolCallId` becomes `AgentActivityId` at its repoint.

### A workflow's agents are ANNOUNCED explicitly: the update arm becomes a oneof, and a subagent's description turns optional

**What changed ("sounds good").** `AgentWorkflowUpdate` becomes `oneof update
{ AgentSubagentStart agent_start | AgentActivity agent_activity }`.
`AgentSubagentPrompt.description` becomes `optional`.

**WHY THE ANNOUNCEMENT CANNOT BE AN `AgentActivity`.** That envelope states WHICH
AGENT DID a unit of work. A workflow's agents are created BY THE SCRIPT, so
there is no actor to name in `agent_id` and no unit of anyone's work to identify
in `activity_id` — wrapping the announcement would require inventing both. The
user's oneof is therefore not a convenience; it is the only honest shape.

**THIS CLOSES THE ONE IMPLICIT LAYER.** Previously a workflow's top-level agents
existed only by first appearance, while every layer beneath them had a real
announcement (their own spawns are ordinary subagent calls). Now every agent in
the tree is announced, and nothing is inferred from arrival order.

**THE EXHAUSTIVE JOURNAL SURVEY that prompted it.** 132 journals, 1,013 records,
exactly two shapes: `{type: started, key, agentId}` and `{type: result, key,
agentId, result}`. NOTHING in a journal is non-agent-scoped — no phases, no
run-level events, no completion, no errors — so the update arm covers the whole
of it and no category of update is missing.

**WHAT THE PER-AGENT META FILE ADDS, and it is why the announcement can be
populated at all.** Every workflow agent has an `agent-<id>.meta.json` beside its
transcript: `agentType` and `spawnDepth` always, plus `model`,
`spawnedWithWorktree` and `worktreePath` on 158 of 524. Verified separately: the
agent's PROMPT is the first user message of its own transcript. So the
announcement can carry the instruction, the subagent type, the model and the
isolation.

**`description` HAS NO PRODUCER on this path, so it turns optional.** A script's
`agent()` call may label a step, but that label reaches no artifact a reader can
recover. Rather than synthesize one, the field states its absence and the comment
tells a consumer to fall back to the subagent type or the instruction's opening.

**TWO FACTS FROM THE SURVEY that are not yet addressed anywhere.**

- 524 `started` records against 489 `result` records: 35 agents started and never
  produced one. Nothing in a journal distinguishes "still running" from "died",
  so a container for such an agent would stay open indefinitely.
- NOTHING IN A JOURNAL EVER SAYS THE RUN FINISHED. The `success` and `failure`
  arms have no producer there at all; their only possible source is the run
  leaving the vendor's live-background set, which is the same membership signal
  already settled for detached work. That dependency is real and was not
  previously written down.

**Sidecar consequence.** Ingesting the per-agent transcripts is not sufficient on
its own — the meta file must be read alongside each one, since it is the only
source for type, model and worktree.

### The workflow update carries ONE activity, not a batch; timeliness is a stated producer obligation

**What changed ("let's do singular").** `AgentWorkflowUpdate.agent_activities`
(repeated) becomes `agent_activity` (singular).

**THE REASON THE USER GAVE, CORRECTED.** He proposed singular to force the shim
to stream immediately rather than buffer. Singular does NOT do that: a producer
can still accumulate and then emit several singular frames in a burst, so the
shape removes the ABILITY TO EXPRESS a batch without removing the ability to
delay one. Timeliness is a producer behavior and the schema cannot enforce it.

**What singular actually buys, and it is enough.** One frame carries one unit as
everywhere else in this contract, so this stops being the only place a frame
carries many. And frame size stays bounded — a COLD READ ingests an entire run's
history at once, and a batched frame would have no cap on what it carried.
Nothing is lost: a consumer renders five activities identically whether they
arrived together or apart.

**So the obligation is STATED rather than implied.** The comment now requires a
producer to emit as observed and not buffer, with the reason: work already
observed and withheld makes a run look stalled. The user's intent is preserved by
saying it, which is the only place it could have been enforced.

### `run_id` is a RESUME handle, not a stream handle — it moves into the placement arms and `AgentWorkflowRun` dissolves

**What changed ("sounds good").** `AgentWorkflowRun` is DELETED. The workflow's
name becomes a plain field on the start arm. `run_id` moves into
`AgentWorkflowPlacementLocal`; the remote arm's `session_url` is documented as
the resume handle for that placement. `resumed_from` becomes a bare optional
string. The success and failure arms stop repeating a run identity.

**THE TWO VENDOR VALUES ARE DIFFERENT FACTS, which is what the orchestrator had
conflated.** `taskId` is the background task's identity — always present, what
the live-background set tracks and what a stop aims at. `runId` is documented as
the "local workflow run identifier for resumeFromRunId" — the RESUME handle,
which is also the run's directory name, and which is ABSENT for a remote run
because the session URL serves that role there. So the envelope's
`DetachedWorkId` is the task id and nothing else, and `run_id` never belonged
beside it.

**WHY THE PLACEMENT ARMS, and not an optional field.** The user asked whether a
workflow-only value should be confined to the workflow messages; it should, and
one level further. A resume handle ALWAYS exists — it is simply a DIFFERENT
handle per placement — so putting `run_id` on the local arm and naming
`session_url` as the remote one removes the optional entirely and makes the
adjacent-exclusivity explicit: `run_id` means nothing when a run is remote.

**Two consequences that fall out.** `AgentWorkflowRun` reduced to a bare name and
stopped deserving to be a message. And the success and failure arms no longer
repeat a run identity — a frame's own identity already says which run it belongs
to, so `AgentWorkflowFailure`'s awkward optional run (optional only because a
pre-launch rejection had no run id) disappears with it.

**Different lifetimes, stated at the field.** The detached-work identity
addresses LIVE work and dies with it; the run id OUTLIVES the run, being both how
a later invocation reuses its results and where the producer keeps its files.

**STILL OPEN.** Whether a workflow agent's creation should NAME its run rather
than relying on which stream it arrived on — the same positional-provenance
concern this contract has refused elsewhere.

### The workflow lands as DETACHED WORK ONLY, and is the one unit that CONTAINS other units — STAGE 1's arm bodies are COMPLETE

**What changed ("ship this motherfucker").** `AgentActivity.item` LOSES its
`workflow` arm (tag 13 retired, not reused). `DetachableWork` gains
`AgentWorkflow workflow = 3`. The `AgentWorkflow` family lands: { start | update
| success | failure }, where the update arm carries `repeated AgentActivity`.

**WHY DETACHED-ONLY, verified at the source.** A workflow CANNOT be
synchronous, confirmed two independent ways: its input has no blocking option at
all (compare bash and the subagent, which each have an explicit
`run_in_background` — a tool that can go either way says so), and its output's
status values are only `async_launched` and `remote_launched`, with no
`completed`, unlike the subagent's output which has one precisely because a spawn
can be awaited. So an activity arm for it could only ever have reported
"launched", with everything real happening elsewhere.

**The orchestrator's counter-argument, raised and dissolved.** It objected that
dropping the arm loses the turn's record of the agent having started a
workflow — the same "effects without causes" problem that justified send_message
existing. Wrong: a detached-work announcement rides the SPAWNING stream, so the
turn's stream still says a workflow was launched here. The trace survives without
an arm.

**THE STRUCTURAL PROBLEM THE USER FOUND, and how it was closed.** He asked
repeatedly where a workflow's subagents were modelled, and the orchestrator kept
answering with behavior rather than shape — "they arrive as flat activity on the
run's stream" — until he named the constraint: detached work is ONE CONNECTION PER
ITEM, so there was literally nowhere for that activity to be returned. The
orchestrator conceded the container did not exist and that it had been letting
stage-4 reasoning drive stage-1 shapes. THE USER'S ANSWER was the update arm:
`AgentWorkflowUpdate { repeated AgentActivity }`. The run's stream stays one
type, and the run's progress IS its agents' work, so the arm is honest rather
than a workaround.

**Two orchestrator objections to that arm, both withdrawn.** (1) That `repeated`
made it a delta with no gap detection. Wrong — the stream is ordered and the
bounded-stream convention already reads a stream ending without a terminal frame
as a transport failure, so frames cannot vanish silently; the bash offset exists
for a different reason (resuming a byte position, which identified units do not
need). (2) That two identities in one frame made upsert ambiguous. Wrong — the
update arm carries NO run state, so nothing about the run is upserted and each
activity upserts by its own identity. The user pushed on both and both collapsed.

**THIS IS THE ONLY UNIT THAT CONTAINS OTHER UNITS, and the justification is
stated at the message.** A workflow is the only kind of work that is a PROGRAM
rather than an action. It does NOT reintroduce the recursion that was retracted:
the shim needs only to know which run an activity came from, which it knows from
the directory it read it out of, and a workflow agent that spawns its own agent
arrives flat in the same update — so no lineage is maintained anywhere.

**AN AGENT'S FIRST APPEARANCE IS ITS ANNOUNCEMENT.** A workflow's agents are
created by the script, so no activity unit announces them the way
`AgentSubagentStart.created_agent_id` announces an agent-spawned one. The run's
journal records a step STARTING, and that record is the announcement — the
sidecar converts a real record rather than synthesizing anything.

**Shape decisions changed from the pre-detour draft, each on evidence.**

- THE `source` ONEOF IS GONE. The producer persists a script for EVERY
  invocation and returns its path, so inline-versus-named-versus-path was a
  distinction with no consequence and the empty `SourceInline` arm was dishonest
  about the script being unavailable. Replaced by `AgentWorkflowScript { path }`,
  always set.
- RESUME BECAME AN OPTIONAL FIELD rather than a source arm, being orthogonal to
  where the script came from. When set it tells a reader the run may have cost far
  less than its agent count suggests, because unchanged steps returned cached
  results.
- THE REMOTE ARM NO LONGER CLAIMS UNOBSERVABILITY. The orchestrator had asserted
  a remote run's work cannot be followed, which was inference from the session
  living elsewhere and was never checked. It now states only what is known.
- `warning` BECAME `notice`, because the producer calls it a non-blocking
  heads-up and the word "warning" invites drawing it as a problem.

**A PROCESS ERROR, recorded.** An earlier attempt landed a workflow body that had
been drafted BEFORE the flat-model detour and never re-presented afterwards. The
orchestrator had asked a COMPOUND question — land the reversion and the workflow
body together? — and read assent as covering both. The user caught it: "we did a
detour to update the agent, which we should have, but did not return to
workflow." The body was reverted and re-proposed from scratch. ROOT CAUSE: a
compound agreement request cannot be answered separately, so it cannot be refused
separately either.

**STILL OPEN, named rather than buried.** Whether `AgentWorkflowRun.run_id` and
the envelope's `DetachedWorkId` are the same value spelled twice; and whether a
workflow agent's creation should NAME its run rather than relying on which stream
it arrived on.

**STAGE 1's ARM BODIES ARE NOW COMPLETE.** Every arm of `AgentActivity.item` has
a body, and `agent_activity.proto` compiles — 492 documented declarations across
15 activity kinds plus the detached-work and workflow families. What stage 1 still
owes is the `MessageId` repoint in four stage-2 and stage-3 files, which is those
stages' work.

### RECURSION IS RETRACTED: subagent activity is FLAT, attributed by `agent_id`

**What changed ("okay go for it").** `AgentActivity.subagent_activity` is
DELETED. `AgentSubagentStart` gains `created_agent_id`. `message.proto` is deleted (see
its own entry). THE WORKFLOW BODY DOES NOT LAND HERE — see the note at the end
of this entry.

**WHY THE RECURSION WENT, in the user's terms.** It "requires the shim to be too
'smart': no longer a simple vendor-adapter and message recorder, it's now
managing relatively complex relationships (subagents of subagents of subagents,
etc, means maintaining a state trace of relationships)". That state belongs
nowhere in the middle: the frontend already draws bubbles within bubbles, so the
DOM IS the tree, materialized once in the only place that needs it.

**HOW PLACEMENT WORKS WITHOUT ANCESTRY ON THE WIRE.** Three mechanisms, and
between them nothing holds a tree:

- READING HISTORY: a page names the parent whose items it wants
  (`top_level` or a parent agent), so a consumer never receives an item it cannot
  place — placement comes from the REQUEST, not from the frame.
- LIVE FRAMES: a consumer draws a container when it sees an agent's CREATION and
  keys it by that agent's identity; every later frame carrying that identity
  routes into it by key lookup.
- THE DAEMON: needs no parentage at all, for either path. It takes an identifier
  and passes it to the shim.

**A FIELD PROPOSED AND RETRACTED.** The orchestrator argued cold open would break
— a consumer paging backward would receive activity for an agent whose creation
was further back, with nowhere to nest it — and proposed a denormalized
`parent_agent_id` on every frame. The user's request-scoped paging DISSOLVES that:
a top-level page returns only top-level items, including the creation records, and
a subagent's items arrive only when explicitly asked for. The field was withdrawn
unlanded.

**THE ONE FIELD THE MODEL CANNOT DO WITHOUT.** `AgentSubagentStart.created_agent_id`
— the agent the spawn produced, as distinct from the spawn itself. Without it
there is no key to draw a container under, and the flat model has no attribution
at all.

**PAGINATION IS FOR AGENTS AND NOTHING ELSE — settled, with the test that decides
it.** The user: "Agent is the only thing with ITEMS. Bash only has, at best, lines
of output." The test is IDENTITY: an item has one, can be upserted, and can
contain other things. A bash line has none — line 400 cannot be addressed or
upserted, and the only handle is an offset. The same is true of read lines, grep
matches and patch hunks: positional, not identified. So pagination in this
protocol is exactly one thing, a page of activity items under a parent agent, and
the orchestrator's attempt to unify it with payload overflow was a category error
built on the shared English phrase "there's more".

**Consequences of that, recorded so they are not re-litigated.** The omitted
counts on the grep, glob, read and bash partial arms serve DISPLAY ONLY and imply
no retrieval. `AgentBashUpdate.from_offset` is a GAP DETECTOR on a live delta
stream, not a retrieval handle. And the earlier exploration of a fetchable versus
not-recoverable distinction is dropped: the user judged the line too dependent on
current implementation capability to be worth encoding, and practically shell is
covered while grep is not, which is the right side of the trade.

**PREFETCH EAGERNESS IS DAEMON POLICY, NOT CONTRACT.** Lazy, one level deep, or
fully recursive are all expressible against the same request shape, so the
contract stays silent and the policy can be tuned without a wire change. The user
observed that one level down is "99% of the information cared about" at almost
none of the cost of going deeper.

**THE WORKFLOW BODY IS NOT LANDED, and the process error is recorded.** The
orchestrator asked a COMPOUND question — whether to land the flat-model reversion
and the workflow body together — read the user's assent as covering both, and
landed a workflow shape that had been drafted BEFORE the flat model and never
re-presented once the flat model changed its premises. The user caught it: "we did
a detour to update the agent, which we should have, but did not return to
workflow." The body was reverted; the flat-model reversion, which WAS agreed,
stands. ROOT CAUSE: a compound agreement request, which cannot be answered
separately and so cannot be refused separately.

**What the workflow research established is NOT lost, and is recorded here so the
increment resumes from evidence rather than repeating it.** A run's journal holds
only `{type: started|result, key, agentId, result?}` — one pair per agent call,
which is what those subagents' own units already state. PHASE GROUPING HAS NO
PRODUCER: a script declares its phases in `meta` and each agent call may name one,
but neither reaches any readable artifact, since the journal has no phase and the
per-agent meta holds only `{agentType, spawnDepth, model}`. A LANDED CLAIM IS
WRONG: `detached_work.proto` asserts a journal "states a step's label, its detail
and its status separately", which is false against the real file, and the
sidecar's own converter concedes its rendering is lossy. And a workflow is ALWAYS
DETACHED — the producer's only status values are `async_launched` and
`remote_launched`, with no synchronous form, in contrast to the subagent where the
same suspicion was raised and a synchronous path turned out to exist.

### TOKEN USAGE MOVES TO THE `AgentActivity` ENVELOPE, with a one-unit-per-response rule; thinking's usage field DIES

**What changed ("makes sense").** `optional TokenUsage usage` lands on
`AgentActivity` beside the arm oneof. It is REMOVED from `AgentThinkingSuccess`
and from all three `AgentResponse` arms. `AgentSubagentTotals.usage` stays.

**THIS REVERSES A DECISION RECORDED EARLIER IN THIS WAVE.** The turn-update-model
entry removed usage from the envelope on adjacent-exclusivity grounds — "a read,
a search or a shell call has no token cost, so an envelope field is meaningless
for most arms; usage rides the arms that have it." The user reopened it on
aggregation grounds: the daemon must float usage up to the topbar and the footer,
and "it shouldn't have to look in 15 arms for that data."

**THE MEASUREMENT THAT SETTLED IT, and it defeated the orchestrator's own first
answer too.** The orchestrator agreed with the reopen; the user then pushed back,
reasoning that if usage genuinely belongs only to responses and thinking then
confining it to those arms is preferable. Measured across ~13,000 assistant
messages in three real transcripts:

| assistant message contains | count |
|---|---|
| `tool_use` only — no text, no thinking | 5,714 |
| `thinking`, no text | 4,673 |
| `text` only | 2,666 |
| `thinking` AND `text` | 1 |

FORTY-FOUR PERCENT OF ASSISTANT MESSAGES CONSIST PURELY OF TOOL CALLS, and every
one carries usage — because usage is a property of the API RESPONSE, not of what
the response contains. Confining usage to the thinking and response arms would
therefore DROP the accounting for 5,714 responses. That is lost data, not a
modelling preference, and it is what decided the question.

Incidentally the double-count the orchestrator had worried about — one message
holding both thinking and text, so two arms reporting one figure — occurs ONCE in
13,000. It was not the reason to move the field.

**THE EXHAUSTIVE ENUMERATION OF USAGE CARRIERS, recorded so nobody re-derives
it.** Three, and only three:

1. `assistant` messages -> `message.usage`. 13,057 occurrences. ONE PER API
   RESPONSE. This is what the envelope field carries.
2. `user` messages carrying a subagent's completion -> `toolUseResult.usage` and
   `totalTokens`. 5 occurrences. The subagent's OWN consumption, which is why
   `AgentSubagentTotals.usage` is a different fact and stays where it is.
3. `SDKThinkingTokensMessage` (`system/thinking_tokens`) -> `estimated_tokens`,
   `estimated_tokens_delta`. Present in the SDK's type surface, ZERO occurrences
   in the transcripts examined.

TOOL CALLS THEMSELVES CARRY NO USAGE. The user asked this directly and the answer
is no — a tool result is a USER message and has none.

**THE ONE-UNIT-PER-RESPONSE RULE, which is what makes the envelope safe.** The
user's instruction was to "ensure that it's set everywhere it can be". Taken
literally that breaks the aggregation it exists for: one response yields several
units, so stamping each of a three-tool-call message's units with the same usage
makes any consumer summing units over-count threefold. So EXACTLY ONE UNIT PER
RESPONSE carries it — the unit for the response's FIRST content block, chosen
because block order is deterministic and needs no judgment — and every other unit
of that response leaves it unset. Absence therefore means "not the unit carrying
its response's usage", never "this cost nothing", and the comment says so.

**THINKING'S USAGE FIELD WAS ALWAYS WRONG, on a fact discovered here.** There is
NO thinking-token field inside `usage` at all. The thinking figure a footer draws
comes from the separate `thinking_tokens` channel and is an ESTIMATE, not a billed
amount. So `AgentThinkingSuccess.token_usage` was never the cost of thinking; it
was the enclosing response's bill, attached to the wrong unit. Removing it fixes a
misattribution rather than merely relocating a field.

**OWED, recorded on the vetting register**: the thinking estimate has no
representation now. It needs its own type rather than borrowing `TokenUsage`,
precisely because it is estimated rather than billed and a consumer must not add
it to a bill. Also unmodelled: `usage.output_tokens_details`, which appears in
real transcripts and `TokenUsage` does not carry.

### The unmodeled-tool body lands; and its INTEGRATION REQUIREMENT is unlike every other arm's

**What changed ("seems good").** `AgentUnmodeled` = { start | success |
failure }. Start carries the tool name as a plain string, the arguments as a
`Struct`, and the start instant. Success and failure each carry the name and
typed `ToolResultContent`. No update arm.

**Typed on one side, untyped on the other, and the asymmetry is the point.** The
ARGUMENTS need the untyped escape — the producer holds no schema for a tool it
did not define, which is the one qualifying reason, already accepted twice in
this contract. The RESULT does not: a tool result is text and image blocks
whichever tool produced it, so it is `ToolResultContent` like any other tool's.

**NOT A FALLBACK, stated at the message.** Same stance as `UnsupportedBlock`: this
arm is for a tool whose schema genuinely cannot be known, never for one whose
modelling was inconvenient or deferred, and a recognizable built-in arriving here
is a PRODUCER DEFECT. Written down because this is precisely the arm that rots
quietly if it is not.

**THE INTEGRATION REQUIREMENT THE USER SET OUT, which no other arm has.** This
message must be surfaced to the user, but the requirements pull in two directions
and both must hold:

- AWARE, BUT NOT LOUDLY. Silence lets a growing blind spot go unnoticed. But an
  unmodeled tool is not an error — the tool very likely ran correctly, and the
  only thing wrong is that this contract cannot describe it. So it must not be
  drawn as a failure.
- COMPREHENSIBLE FOR REMEDIATION. The purpose of surfacing it is that someone can
  decide whether the tool deserves an arm of its own. A raw dump of the arguments
  serves that badly: an unstructured blob is illegible in a UI and tells a reader
  nothing actionable. So what surfaces must be an ABBREVIATED, LEGIBLE form.
- ITS HOME IS THE TOPBAR'S ALERT SURFACE, in the dropdown — not the feed, where
  it would read as part of the conversation, and not a failure card.

**AN IDEA THE USER RAISED AND EXPLICITLY DID NOT DECIDE.** Classify the
arguments' STRUCTURE programmatically, and when a structure is one the daemon's
state manager has never seen before, run a small fast model over it to produce a
plain-English summary fit for rendering. His words: "just an idea, not saying we
SHOULD do that, but we should note all this information when we land." Recorded
as an idea, NOT as a decision — so a later implementer neither treats it as
agreed nor re-invents it from scratch. What IS settled is the requirement it was
proposed to satisfy: abbreviated and legible, never a dump.

**CONSEQUENCE: a stage-2 reopen is owed.** `frontend.v1`'s topbar has a
`TopbarWarningStrip` of warnings, and this needs a warning kind of its own plus
the dropdown treatment described above. That is a frontend increment, recorded on
the vetting register's owed list rather than guessed at here.

**No update arm, for a reason worth stating.** This contract holds no knowledge
of any unmodeled tool's streaming behavior. An update arm would be an invitation
to invent a producer that does not exist.

### The send-message body lands: ADDRESSED at the call, RESOLVED at the outcome; the delivery arm is a cost fact

**What changed ("option a looks best").** `AgentSendMessage` = { start | success
| failure }. Start carries the addressed recipient as a plain STRING, an optional
summary, the body, and the start instant. Success carries the RESOLVED
`AgentId` plus `oneof delivery { queued_to_live | resumed_recipient }`.

**THE ADDRESSED/RESOLVED SPLIT, which is the whole of the user's choice.** Two
options were put to him: carry the recipient as a typed `AgentId` from the start,
or carry what the caller wrote at the call and the resolved identity at the
outcome. He took the latter. The reason it matters: the caller addresses a
recipient by identity OR by a human-readable NAME a spawn was given, and at the
instant of the call nothing has resolved which agent that is. Typing it as an
identity up front would claim a resolution nobody had performed.

**THE UX JUSTIFICATION, checked in the existing implementation rather than
assumed.** The webapp already draws this tool, and deliberately minimally: the
card is one line, `-> recipient: summary`, and the full body is NEVER drawn
(`render.ts:1706-1711`) because a relayed message is frequently long. A
SUCCESSFUL delivery renders nothing at all (`render.ts:1776-1782`) — "the
successful delivery echo adds nothing over the summary line" — while errors fall
through so failures stay loud. The purpose, in one sentence: without this card,
cross-agent communication is invisible and a subagent's bubble resumes activity
with nothing to explain why.

**EVIDENCE: 384 real results surveyed, not a type read.** The tool has NO typed
shape in the vendor's surface, so the shape came from transcripts. Input is
`{to, summary, message}`. Result is `{success, message, resumedAgentId?, pin?}`
— `success` and `message` in 384/384, `resumedAgentId` in 218/384, `pin
{id, name, ref}` in 381/384.

**THE OUTCOME IS ENCODED IN PROSE, and only two thirds of it is recoverable.**
Three distinct sentences appear: "Message queued for delivery to X at its next
tool round" (recipient live, no `resumedAgentId`); "Agent X had no active task;
resumed from transcript in the background"; and "Agent X was stopped
(completed); resumed it in the background". The presence of `resumedAgentId` is
the ONLY structured discriminator, so it separates live-delivery from
resumed-recipient and nothing more. The further distinction — WHY the recipient
needed resuming — exists only inside a sentence written for the model to read,
and recovering it would mean parsing prose. `AgentSendMessageResumedRecipient` is
therefore EMPTY, with its comment stating that the producer says more and that
the remainder is deliberately not recovered.

**WHY THE DELIVERY ARM EARNS ITS PLACE even though nothing draws it today.**
`resumed_recipient` means A DORMANT AGENT WAS WOKEN and is consuming tokens
again. That is a real user-visible consequence with no other producer, and it is
the causal link between one agent's send and another's renewed cost. Recorded as
a NEW drawn fact this contract makes available rather than one it owes an
existing surface.

**Dropped from the producer's result, each for a stated reason.** The prose
`message` (its two recoverable facts are structural once the delivery arm
exists, and carrying it would invite a surface to draw a sentence instead of
rendering an arm); `pin.name` (equal to `pin.id` in every sample observed —
these were unnamed agents); and `pin.ref` (a short handle nothing in this stack
uses).

**A gotcha stated at the field.** A surface must NOT fall back to the body when
no summary was supplied. Dumping a relay into a feed is precisely the outcome the
summary exists to prevent, so the absence of a summary means "draw the recipient
alone".

### The skill body lands, from REAL TRANSCRIPT EVIDENCE: the document is not the tool return, and nothing delimits a skill's scope

**What changed ("your model looks perfect").** `AgentSkillUse` = { start |
success | failure }. Start carries the skill name, optional args and the start
instant. Success carries the name, the DOCUMENT as markdown, and the tool
allowances the skill brings. No update arm, and NO NESTED WORK.

**THE RESEARCH METHOD, at the user's instruction.** He asked for the skill's
handling to be researched "in the SDK/JSONL at the source of truth, not our
existing code" — the orchestrator had been reasoning from our own sidecar and
webapp. Two findings followed that our own code obscured, and one of them
contradicts what our current model claims.

**FIRST: `Skill` HAS NO TYPED SHAPE AT ALL.** There is no `SkillInput` or
`SkillOutput` in the vendor's tool-types surface — the only skill-adjacent types
are `ProposeSkills*`, which are a different feature. So unlike every other tool
body in this file, this one rests entirely on observed transcripts.

**SECOND: THE FOUR-RECORD SEQUENCE, read out of a real transcript.**

1. `assistant` with a `tool_use` block, `name: "Skill"`, `input: {skill, args}`,
   id T.
2. `user` with a `tool_result` for T whose content is the bare string
   `"Launching skill: <name>"`, plus `toolUseResult {commandName, success}`.
   THE DOCUMENT IS NOT HERE.
3. `user` with `isMeta: true` and `sourceToolUseID: T`, whose text block IS the
   skill document verbatim.
4. `attachment` with `{type: "command_permissions", allowedTools: [...]}` — the
   tool allowances the skill brings.

**WHAT THIS CORRECTS.** The old model's `SkillBodyResolved` was right that a body
arrives separately, but our sidecar correlates it by keeping a skill-NAME map and
matching what arrives next (`convert/detached.go:65`). That is unnecessary:
`sourceToolUseID` links the document DIRECTLY to the invoking call, so the
correlation the contract needs is structural and free rather than positional and
fragile. Recorded because the fragile version is already in production and will
look deliberate to whoever reads it next.

**WHY SUCCESS CARRIES THE DOCUMENT RATHER THAN THE RETURN.** The tool's own
answer restates the skill's name and says nothing else, so a unit that settled on
it would settle with nothing to draw. The unit therefore settles when the
DOCUMENT lands, which is after the acknowledgement — the shim holds the unit open
across the two records.

**NOTHING DELIMITS A SKILL'S SCOPE, and the contract says so rather than
inventing one.** There is no skill-ended record, no boundary marker, and no
producer statement about where a skill's influence stops — verified by direct
transcript inspection, not inferred from our code's silence. So `AgentSkillUse`
has NO nested arm, unlike the subagent: work the agent does after loading a skill
is its own ordinary activity.

**THE ACCEPTED COST, stated because it changes today's behavior.** The webapp
currently draws a skill as a container whose body is the document and whose nested
rows are the agent's subsequent emissions (`async-render.ts:226`). Under this
model that container is a PRESENTATION choice belonging to whatever resolves the
surface, not a protocol fact. A surface may still draw subsequent work under a
skill heading — but it does so on its own authority, and the protocol never
claims an extent it cannot observe. The alternative — a recursive arm carrying the
same agent id — was weighed and refused: it would oblige a producer to invent an
end that nothing observes.

**`allowedTools` IS CARRIED, and it is a fact about CONSENT rather than
content.** A reader deciding whether a skill should have run wants to know it was
permitted to write files. The names are BARE STRINGS rather than the typed tool
vocabulary, for the same reason the subagent's activity label is: these are
allowances, not calls, so there is no typed call for a name to be a second
spelling of — and an allowance may name a tool this contract does not model.

**Presence, not sentinels, in two places.** `args` is optional because a skill is
commonly invoked bare, and an empty argument must stay distinguishable from no
argument. `allowed_tools` is optional because declaring NO allowances differs from
declaring an empty set.

### The question body lands: ECHOED VALUES rather than tokens or order, and failure scoped to the ASK

**What changed.** `AgentQuestion` = { start | success | failure }, success
carrying `oneof outcome { answered | unanswered }`. A batch of one to four
questions, each with a per-question single/multi select arm over options. An
answer names its question and its picked options by ECHOING the values it was
served.

**THE PRODUCER'S ANSWER SHAPE IS A SERIALIZATION, NOT A MODEL — and the contract
refuses to mirror it.** Its output carries answers as a MAP KEYED BY QUESTION
TEXT, with multiple selections COMMA-JOINED into one string, and per-question
annotations keyed by question text again. Joining on prose breaks when two
questions read alike; comma-joining is unrecoverable for any label containing a
comma. The shim undoes both at the boundary.

**THREE CANDIDATE KEYS, and why the third won.**

- POSITION (answers in batch order). The orchestrator proposed it. The user
  objected in principle to making order load-bearing, and was right: nothing
  about the producer's data is ordered, so the order would have been an invention
  the shim maintained.
- A MINTED TOKEN per question and per option, per the typed-echo-token
  convention. The orchestrator proposed this next. The user rejected it because
  IT REQUIRES THE SHIM TO TRACK STATE — a token means nothing without a stored
  mapping back to the text the producer keys on.
- ECHOING THE VALUE ITSELF, the user's proposal and what landed. The question's
  TEXT and the option's LABEL are the producer's own keys, so echoing them lets
  the shim reconstruct the producer call from the answer alone, with NOTHING
  remembered. The echoed values are typed on both sides (`AgentQuestionText`,
  `AgentQuestionOptionLabel`) rather than bare strings, per the echo-token
  convention's actual requirement — the convention is about the value being ONE
  TYPE across the round trip, not about the value being opaque.

**A refinement the user made to his own proposal.** He first suggested the answer
carry the whole question and option MESSAGES; then: "it's probably better to just
have the question/choice text embedded in the answer protos rather than the full
question protos themselves. The consumer can resolve that just fine." So an
answer carries the two echoed values and not the descriptions, previews or
headers, which nothing echoes.

**Why validation costs nothing, recorded because it is the objection an echo
usually loses to.** A client could in principle echo a question nobody asked.
The shim is ALREADY HOLDING the pending permission callback while the turn
blocks — it must, in order to answer it — so it has the original ask in hand by
construction and can check the echo without storing anything new. The echo is
stateless in the sense that matters: no NEW state, not merely relocated state.

**FAILURE IS SCOPED TO THE ASK, by the user's correction.** The orchestrator gave
the failure arm a cause oneof splitting "could not ask" from "could not apply the
answer". The user: the arm "should only support failures to ASK THE QUESTION,
because, well, it's about questions. Whatever the connection route is for
RESPONDING WITH AN ANSWER should support the ANSWER failure handling." Correct,
and it needs no new machinery — the answer travels on its own unary call, and the
response-outcome convention already gives that call its own failure arm. So the
arm here is empty like every other in this file. ROOT CAUSE of the error:
modelling failure causes for an exchange whose mechanics are stage 4's and not
yet settled, which is derived-not-invented applied backwards.

**THE EXCHANGE'S MECHANICS, stated because the user asked and it looked like it
might need bidirectional RPC.** An ask BLOCKS the turn it is in. The question
arrives as a frame on the server stream carrying the unit, and the answer returns
as a SEPARATE UNARY CALL — the `UpdatePrompt { interrupt | answer_permission }`
verb settled at 4b. Two directions, two calls; no bidirectional exchange is
required, which is why the transport decision never needed one.

**Domain outcomes.** An ask that NOBODY ANSWERED is a success carrying the
`unanswered` arm, not a failure — the producer has an idle timeout, so the agent
proceeds without a choice and a consumer draws the ask as expired rather than
pending forever. And the free-text escape is ALWAYS offered by the drawn card
whether or not the agent asked for one, so an answer may carry free text even
where the options looked exhaustive.

**Dropped from the producer's output, each for a stated reason**: the echoed
`multiSelect` flag (the question's own arm states it), and the idle-timeout
duration (nothing draws "it waited ninety seconds").

**UNCERTAIN AND RECORDED AS SUCH.** The answer carries BOTH a free-text field
and a note field, because the producer has two separate fields — one that
substitutes for a choice and one described as notes the user added to their
selection. The orchestrator is INFERRING that distinction from the field
descriptions and has NOT observed either in use, nor confirmed that the
substitute-for-a-choice field is the free-text escape rather than something else.
The user accepted carrying both; the verification is owed on the vetting
register.

### THE FAMILY IS RENAMED `Turn*` -> `Agent*`; `AgentActivity` carries `agent_id` and RECURSES — one shape for the main agent, an awaited subagent, and detached work

**What changed ("looks great, let's do it").** Twenty-six name changes across
~90 messages: `TurnProgress` -> `AgentActivity`, `TurnProgressItemId` ->
`AgentActivityId`, `TurnUpdate` -> `AgentUpdate`, `TurnAgent*` -> `Agent*`
throughout, `TurnToolCallStartedAt` -> `AgentActivityStartedAt`,
`TurnDetachableWork*` -> the `Detached*` family. `TurnId` is UNTOUCHED. New:
`AgentId`; `AgentActivity.agent_id`; the recursive `subagent_activity` arm; the
whole `AgentSubagent` family. `DetachableWork`'s arms revert to naming the unit
types, and `DetachedWorkBash` is deleted.

**WHY THE RENAME, in the user's terms.** He asked for the model to be AGNOSTIC
to whether an agent is the main agent or a subagent — "the subagent
representation should be the same as the turn representation itself, because
they can do the same things: have responses, thinking, tokens, bash, skill, etc
calls, or even more salient: subagents themselves. So we want a representation
that gives us this recursiveness, and agnosticism." The vocabulary was never a
TURN's; it is WHAT AN AGENT DOES, and a turn is merely the window in which the
main agent does it. The name was making a window own a vocabulary.

**THE RECURSION IS IN THE SCHEMA, NOT IN A POINTER — and the reason is
PROTOCOL-DRIVEN DEVELOPMENT.** Three models were weighed.

- Announcement-only, with attribution left to the data: REJECTED, it has no
  link at all. A subsequent activity frame carries only its own identity, so a
  consumer could attribute it only BY POSITION — everything after the
  announcement belongs to the subagent until something says otherwise. That is
  stateful, order-dependent decoding, and it breaks the moment a caller
  interleaves its own work with an awaited subagent's, or two subagents run.
- A flat `owner` pointer: viable, and what the orchestrator proposed.
- TRUE RECURSION, chosen. The user's argument: with oneof arms mapping to
  FUNCTIONS, recursion in the schema makes the recursion in the CODE fall out —
  the same subroutine that handles a turn's activity handles a subagent's,
  rather than the tree being reassembled from ids by logic the schema does not
  imply. The skill's own handoff rule agrees: the protobufs are a close
  representation of the implementation architecture, not merely its wire form.

**THE ORCHESTRATOR'S WRAPPER WAS UNNECESSARY, and the user removed it.** It had
proposed `AgentSubagentActivity { subagent_id; AgentActivity }` — a thin wrapper
carrying the owner beside the nested work. The user: the nested `AgentActivity`
already suffices. Correct once `agent_id` rides the envelope, so the arm is
simply `AgentActivity subagent_activity`.

**`agent_id` ON THE ENVELOPE, and the reversal that produced it.** The
orchestrator argued an agent id would be a SECOND SPELLING of what the envelope
already stated, since unit ids are sourced from the spawning call. WRONG, and
the user rejected the inference before it was checked — "the task that spawns the
agent is not the agent itself". Verified: `agent_id` and `tool_use_id` are
adjacent fields on one message (`sdk.d.ts:3630-3631`), and `SessionMessage`
carries `parent_tool_use_id` AND `parent_agent_id` separately, the latter
existing precisely because depth beyond one cannot be resolved from call ids.
See the identity-vocabulary entry for the four spaces and what each names. So
`agent_id` rides EVERY activity frame at every level, and one shape now serves
the turn's top level, an awaited subagent's nested activity, and a detached
item's own stream.

**THE UPSERT RULE, settled because the recursion forced the question.** On the
`subagent_activity` arm the outer `activity_id` is the SPAWN — the containment
path — and the unit the frame upserts is the INNERMOST one. A consumer keys its
store on the innermost identity and reads the outer ones as ancestry. Without
this rule stated, a consumer keying on the outermost id would have every nested
frame of one subagent overwrite the last.

**`DetachableWork` REVERTS, and the amendment that introduced description types
is withdrawn.** Two increments ago its arms were changed from the unit types to
dedicated description types, because a unit type's body was an outcome oneof and
could not describe work a consumer had never seen. The universal `start` arm
removed that objection — a start arm IS the description — so the arms name
`AgentSubagent` and `AgentBash` again and `DetachedWorkBash` is deleted. ROOT
CAUSE of the detour: the description types were a workaround for a missing
announcement frame, invented before the announcement was.

**The subagent's own lifecycle and its WORK are deliberately separate.**
`AgentSubagent` carries the spawn — prompt, progress, report, totals — and
NOTHING about what the subagent said. Its reasoning, prose and tool calls are
`AgentActivity` of their own. Two places describing the same output could
disagree; one cannot.

**The producer's own split is NOT reflected in the contract, per the user's
ruling.** The tool returns a full report when a spawn is awaited and a bare
launch acknowledgement when backgrounded, and an awaited spawn may report no
progress while a detached one does. Both are shim implementation details: the
shim maps them onto ONE lifecycle. A consumer that had to know which shape the
producer used would be learning a calling convention in order to draw a bubble.

**Kept from the vendor, each drawn**: the description and instruction, the
requested subagent type, the addressable name, the requested model override, the
isolation choice, mid-run progress (duration, tool count, running token sum), the
subagent's note and current tool label, the report, settled totals with full
usage, the tool-stat breakdown, `models_used` (more than one entry means a
mid-run model swap), and the worktree path and branch.

**Two shapes stated deliberately rather than by default.** The isolation choice
is SPLIT across arms — the prompt's oneof says what was ASKED FOR, and the
success arm's worktree says what was ACTUALLY USED, because the path does not
exist until the run reports. And `AgentSubagentActivityLabel.tool_name` is a
BARE STRING rather than the typed tool vocabulary: no arguments accompany it, so
it is an activity label rather than a call, and there is no typed call for it to
be a second spelling of.

**A REMOTE spawn is modelled and explicitly unobservable.** The isolation oneof
has a `remote` arm because the vendor has one, and its comment states that such
work runs detached in a cloud environment and produces nothing further on this
unit — a start and then silence. Better than omitting the arm and having remote
spawns look like broken local ones.

**NON-OBVIOUS IMPLEMENTATION CONSEQUENCES.**

- `AgentActivity` is now RECURSIVE, so every consumer's activity handler must be
  re-entrant. That is the point of the shape, but a handler written as a flat
  switch will silently ignore nested work rather than failing.
- The shim must attribute every frame to an agent. For the main thread that is a
  constant; for a subagent it is the vendor's `agent_id`, which appears on
  forwarded subagent messages and on the tool return.
- `DetachableWork` naming the unit types means the detached-work announcement
  now carries a full unit frame rather than a small description — larger on the
  wire, and the shim must have the unit's start state to hand when it announces.
- Every consumer of the ~90 renamed types recompiles. Nothing outside
  `conversation.v1` referenced them yet, because the arm bodies were landing
  incrementally and no importer had been repointed.

**OWED, and recorded on the vetting register rather than assumed**: that the
shim must set `forwardSubagentText`, without which a subagent's prose and
reasoning never reach us at all and the recursive arms have no producer.

### IDENTITY VOCABULARY: the four identifier spaces, what each names, and which are NOT interchangeable

**Why this is recorded.** Four identifiers in this design were being used
loosely, and two of them were conflated outright by the orchestrator. An
implementer who joins on the wrong one gets plausible, silently wrong
attribution — a subagent's work drawn as its caller's. So each is defined here
once, with the evidence.

**`agent_id` (the vendor's) — WHICH AGENT INSTANCE.** Its own identifier space.
It appears as `AgentOutput.agentId`, as `SubagentStopHookInput.agent_id`, and as
`parent_agent_id` on `SessionMessage` — the last documented as "agentId of the
subagent that spawned this subagent, or null when this message belongs to a
depth-1 subagent (spawned by the main loop) or to the main session itself". THAT
FIELD IS THE PROOF THE SPACE IS ITS OWN: depth greater than one cannot be
resolved from call ids at all.

**`tool_use_id` (the vendor's) — WHICH TOOL CALL.** For a Task call this
identifies THE SPAWN, not the agent it spawned. The two are adjacent fields on
the same message (`sdk.d.ts:3630-3631`), and `SessionMessage` carries
`parent_tool_use_id` AND `parent_agent_id` separately.

**A CONFLATION, KEPT VISIBLE.** The orchestrator claimed the spawning call's id
"IS the subagent's identity", and used that to argue an agent id on the wire
would be a second spelling of what the envelope already stated. WRONG on both
counts: the envelope states a CALL, and the vendor keeps the two spaces
separate. The user rejected the inference on exactly that ground — "the task
that spawns the agent is not the agent itself" — before it was checked, and the
check confirmed him. ROOT CAUSE: reasoning from where an id HAPPENS TO COME FROM
(unit ids are sourced from `tool_use_id` where one exists) to what the id MEANS.
Provenance is not semantics.

**`activity_id` (ours) — WHICH UNIT OF WORK.** The stable identity of one unit
from its first frame to its last: the thing every frame for that unit is an
upsert of, and the thing a nested unit is scoped under. Minted by the SHIM, one
per unit for the unit's whole life, sourced from `tool_use_id` where the vendor
has one and from message id plus block index for text and reasoning. IT NAMES
WORK, NEVER AN AGENT — so a nested unit's `activity_id` says what a subagent was
DOING and says nothing about WHICH subagent.

**`TurnId` (ours) — WHICH TURN.** The window during which the main thread cannot
accept a prompt. Daemon-minted, returned at submission, and unrelated to the
three above.

**NOT INTERCHANGEABLE, stated because each pairing was at some point treated as
one thing:** an agent is not its spawning call; a unit of work is not the agent
doing it; and a turn is neither.

**STILL OPEN, and owed to stage 5.** Whether `activity_id` and the store's
per-record identity are the same value. Under model 2 the unit's id IS the
record's identity, and the no-respell rule says a typed identity is imported
rather than restated, so they SHOULD coincide — but the store's
`top_level_message_id` is a THIRD and coarser thing (the feed-row owner column)
and must not be conflated with either. This is the granularity collision the
stage-1 reversion already reopened by name.

### The re-announced `start` instant is RECOVERED FROM THE STORE, not held in memory — a REQUIREMENT on stage 5

**The user's ruling, confirming and improving the orchestrator's note.** The
previous entry recorded that the shim must retain a unit's first-observation
instant "for the unit's whole life". The user: "the store should have this
information, and should be indexable on the [unit's] id. So as long as the id is
provided to the detached work, it's fine." Correct, and it removes an in-memory
obligation the orchestrator had stated as though it were unavoidable.

**Why it holds.** The join key already exists — `TurnDetachableWorkDetached`
carries the unit's `TurnProgressItemId` — and under model 2 the `start` frame is
itself a record, so its instant is DURABLE rather than merely remembered. The
second announcement reads the first.

**The path this actually serves is SHIM RESTART, not the ordinary detach.** For a
normal detachment the shim still holds the instant and no lookup happens. The
lookup exists because after a shim bounce, still-live background work must be
re-announced and the shim no longer has it — the same scenario
`ShimHello.live_task_set` already exists for. So this is one more fact the
reattach path must recover, not a new mechanism.

**THE REQUIREMENT ON STAGE 5, recorded so it is not designed away.** The store is
NOT indexable on a unit's id today. `entry` is keyed `PRIMARY KEY (session_id,
seq)`, with a dedup index on `write_id` and two indexes on
`top_level_message_id` — which is the FEED-ROW OWNER column derived by the
ingest-time parent walk, NOT the unit's own identity
(`shim-store/internal/db/db.go:156-172`). A lookup by unit id would be a scan.
Under model 2 the unit's id becomes the record's own identity, so `StoreEntry`
must carry it as an INDEXED column. Stage 5 owns that, and would otherwise have
no way to know anything depends on it.

### THE `start` / `update` RULE: `start` means "this stream now carries this unit"; a kind has `update` IFF something produces growth for it

**What changed ("your start audit suggested changes look good").** `read`,
`write`, `edit`, `grep` and `glob` rename their first arm `update` -> `start`
(types `*Running` -> `*Start`) and have NO update arm. `bash` splits into
`start` + a real `update` carrying an output delta and its offset. `thinking`
and `response` GAIN a `start` arm, each empty. One rule now holds across the
family.

**THE RULE the user's stream observation produced.** `start` announces that THIS
STREAM is now carrying this unit — it does NOT mean the work began. `update`
reports GROWTH. A kind has an update arm IFF something actually produces growth
for it. So `start` is universal and `update` is EARNED: today only bash (once
detached), thinking and response earn one.

**THE DEFECT THIS FIXED, which was the opposite assignment in both
directions.** The five file and search tools had been given an arm named for
growth they can never have, while the two kinds that genuinely grow had no
announcement at all. The orchestrator introduced the first error in the previous
increment; the second predated it. The user found both by asking whether the
`start` semantics generalized beyond bash.

**Why thinking and response needed it, which is more than symmetry.** Their
update arm carries text CUMULATIVELY. So "the block opened and nothing has
arrived yet" could only be spelled as an update carrying an EMPTY STRING — a
sentinel standing in for a state, which this contract forbids everywhere else.
The start arm states it properly, and it is the frame that makes a thinking
disclosure open with its live indicator, and an empty response bubble appear,
BEFORE the first token lands. Both start messages are EMPTY and carry no start
instant: nothing draws a clock against reasoning or prose, and a field no
consumer reads is a field that decays.

**THE USER'S DETACHMENT QUESTION, and why the answer is a rule rather than an
exemption.** If a command starts in a turn and then detaches, does its new
stream never send `start`? The user's instinct was that this is fine. It is —
but not because a missing announcement is harmless: because the detached stream
RE-SENDS `start`. Three reasons converge, and the second is decisive.

- Model 2 makes it the MECHANISM, not a duplication: identity is per thing,
  growth is a re-send, and a frame is an upsert of the whole unit. Re-announcing
  one unit under one id on its new stream is how the model already works.
- THE DAEMON MUST SURVIVE ITS OWN RESTART. A stream whose first frame presumes a
  frame delivered before the restart is unrecoverable, and daemon-restart
  catch-up is a settled requirement of this design.
- Every other landed shape is already self-describing for this reason — grep's
  query rides both its announcement and its success arm.

So: EVERY STREAM THAT CARRIES A UNIT OPENS WITH THAT UNIT'S `start`, and the
second announcement repeats the ORIGINAL start instant, so a drawn clock does
not reset when work moves between streams.

**FOREGROUND SHELL OUTPUT IS OBSERVABLE NOWHERE — verified against what the
sidecar can actually reach.** The user asked whether the sidecar could be
adjusted to read a running command's output. It cannot, and the reason is where
the bytes are. The sidecar tails exactly four kinds of file, all written by the
agent binary: session transcripts, subagent transcripts, workflow journals
(`internal/discover/discover.go:83-91`), and `tasks/*.output` spools — PER-TASK
files, so only background work has one. A foreground command's output exists in
NO file while it runs; it reaches the transcript only inside the completed tool
result. Nor can we fix it from our side: we do not run the process, the agent
binary does, so a foreground spool would be a vendor change. `bash`'s update arm
is therefore structurally DETACH-ONLY, and its comment says so, because an
integrator would otherwise wait for frames that never come.

**A CLAIM THE USER CHALLENGED — NOW OBSERVED RATHER THAN INFERRED, AND
CONFIRMED.** Measured empirically at the user's instruction: a foreground
command producing 603,300 bytes over ~75s, with the inline cap (observed at
~30KB) crossed within the first ~4 seconds, polled at ~1s intervals for the
command's whole runtime across two independent runs. NOTHING appeared in the
`tool-results` directory while the command ran; the file materialized at process
exit ALREADY AT ITS FINAL SIZE. So `persistedOutputPath` is a post-completion
artifact and the detach-only statement above STANDS. Two limits on the evidence,
stated so nobody over-reads it: the write path itself lives in a compiled binary
and was not inspected, so this rests on external behavior; and a first attempt
produced a FALSE NEGATIVE because `find` is shell-aliased to a shim that errored
on the poller's syntax — that run was correctly discarded rather than counted as
support. RECORDED SO IT IS NEVER RE-PURCHASED: there is no incremental producer
for foreground shell output through this mechanism, and the only live path is
backgrounding, which is the already-known `tasks/*.output` spool.

**The superseded wording, kept visible.** The orchestrator stated that `BashOutput.persistedOutputPath` is
written at completion. That was INFERRED from the field's doc comment ("set when
output is too large for inline"), never observed. The user: "are you sure?" — and
dispatched an agent to run a long-lived high-output command and watch whether the
persisted file appears and GROWS during execution. If it does, foreground
commands have an incremental producer after all and the detach-only statement
above is wrong; the shape does not change, only the producer note. Recorded here
because a design note resting on a doc comment is exactly the class of debt the
vetting register exists to hold, and this one was caught in flight rather than
after landing.

**NON-OBVIOUS IMPLEMENTATION CONSEQUENCES.**

- The shim emits a `start` frame per unit PER STREAM, not per unit. Detaching
  work therefore produces two starts, and the shim must carry the ORIGINAL
  instant into the second — a first-observation timestamp it must retain for the
  unit's whole life rather than stamping at emit time.
- `TurnAgentResponseStart` takes tag 4, out of numeric order, because the update
  and terminal arms keep their landed tags; the arm ORDER in the file reads
  start-first for legibility. Nothing depends on tag order, and renumbering the
  settled arms would churn every consumer for nothing.
- The webapp's response renderer must draw an empty bubble on `start`, which is
  a behavior it does not have today: it currently creates the bubble on the
  first text delta (`render.ts`, the `TextStream` path), so the bubble's
  appearance moves earlier by one frame.

### The shell body lands; and EVERY TOOL GAINS AN ANNOUNCEMENT ARM — the item must exist before it concludes

**What changed ("okay looks good then").** `TurnAgentBash` = { update |
success | failure }, success = { command; oneof outcome { completed |
interrupted } }, each outcome carrying `TurnAgentBashOutput` = `oneof form
{ text | image }`, text carrying stdout, stderr and a whole/partial extent arm.
`TurnDetachableWorkDetached` gains a `cause` oneof; `TurnDetachableWork`'s arms
repoint at description types; read, write, edit, grep and glob each gain an
`update` arm; grep and glob gain the query elements they were missing; a shared
`TurnToolCallStartedAt` lands.

**THE HOLE THE USER'S QUESTION OPENED, which is the important part of this
increment.** The orchestrator wrote that the SDK's per-call progress message
"carries elapsed time and a heartbeat flag and NO output". The user read that
back as an argument FOR an update arm carrying elapsed time. That reading
exposed something neither of us had stated: with two arms only, THE FIRST FRAME
A CONSUMER EVER RECEIVES FOR A TOOL ITEM IS ITS TERMINAL ONE. A command running
four minutes is undrawable for four minutes, because the item does not exist
until it is over. This was NOT bash-specific — read, write, edit, grep and glob
had all landed with two arms and the same hole.

**The fix, and why the elapsed figure is NOT what goes in it.** The update arm
ANNOUNCES the call, sent ONCE at issue, carrying the call's identifying element
and a START INSTANT. A consumer animates its own clock from that instant. An
elapsed count on the wire was considered and rejected twice over: it is a second
authority for a value derivable from one instant, and it arrives at the
producer's heartbeat cadence, so a drawn clock would jump to network timing
instead of ticking. This is the pattern the contract already uses
(`FeedDetachedHead.runtime`, `FooterExpandedRowRuntime` both carry a start and
nothing else).

**The per-kind update rule SURVIVES INTACT.** It says an update arm exists iff
the kind has an intermediate state a consumer draws differently. What changed is
the ANSWER for every tool: *running* is that state. The rule was never wrong;
it had been applied while assuming an announcement came from somewhere else,
and it does not.

**A SECOND DEFECT this surfaced, in already-landed shapes.** Grep and glob
carried NO pattern anywhere — their success bodies are pure outcome, so the
protocol model could not describe a grep call at all. Read and write happened to
carry a path and hid the gap. Fixed by `TurnAgentGrepQuery` / `TurnAgentGlobQuery`
on BOTH the announcement and the success arm. The duplication is required, not
tolerated: under model 2 a frame is an upsert of the whole unit, so a success
frame omitting the query would LOSE it for any consumer that missed the
announcement.

**A THIRD DEFECT, in `TurnDetachableWork`.** Its arms named the PROGRESS-ITEM
types (`TurnAgentBash`, `TurnAgentAgentCreate`), whose bodies are outcome
oneofs. Work being announced has no outcome, so those types could not say what
the work IS — the one thing a consumer with no earlier frame needs. The arms now
name description types (`TurnDetachableWorkBash { command }`;
`TurnDetachableWorkAgent` is owed at the agent increment).

**THE USER'S STRUCTURAL CORRECTION on backgrounding, and the SDK confirmation
he asked for.** The orchestrator had sketched a `backgrounded` outcome arm on
the shell item. The user: shouldn't that be the DetachedWork message, with a
detached-work stream established? Correct, and the arm is gone. VERIFIED at the
SDK type surface from four independent directions: `BashInput.run_in_background`;
`BashOutput.backgroundTaskId` / `backgroundedByUser` ("Ctrl+B") /
`timedOutAfterMs` ("auto-backgrounded"); `BackgroundTaskSummary.type` documented
as "'shell', 'subagent', 'monitor', 'workflow'" WITH a shell-only `command`
field, so shell membership in the live-background set is designed, not
incidental; and `stopTask(taskId)` / `backgroundTasks(toolUseId?)` addressing
tasks with no kind restriction.

**So the shell item's result is legitimately NEVER RESOLVED when a command
backgrounds** — it did not conclude, it MOVED, and the detached frame naming it
is what says so. The three backgrounding causes moved onto
`TurnDetachableWorkDetached` where detachment is actually stated, and ALL THREE
land on `detached` rather than `created`, including a `run_in_background` call:
such a call is streamed as a progress item first, so it always has an item it
detached FROM — it simply never has a foreground running phase.

**THE OUTPUT PRODUCER IS OURS, NOT THE VENDOR'S — verified, and load-bearing.**
The SDK exposes NO route that returns a background task's output; the whole
28-method control surface was enumerated and nothing fetches it, and
`task_progress` for a shell carries `{total_tokens, tool_uses, duration_ms}` and
no bytes. Every byte of detached shell output on this contract therefore comes
from the SIDECAR tailing the spool file the agent binary writes
(`shim-sidecar/internal/handler/shell.go`), terminated by its `EXIT=<code>`
line. The user's reading — "we CAN pipe the output along, it just comes from the
sidecar" — is exactly right, and nothing about the shape changes. What changes is
who is accountable: a vendor route breaking surfaces at the type surface on the
next upgrade, while a sidecar route breaking is a file format or path we do not
own changing underneath us, with no type to fail against.

**Dropped from the vendor's output, each for a stated reason.** No exit code
exists on `BashOutput` at all (the whole interface was read), so none is
modelled — a command's own output is the only account of how it went.
`rawOutputPath` (an MCP concern, not a shell one); `backgroundCwdHint`,
`returnCodeInterpretation`, `noOutputExpected`, `staleReadFileStateHint`,
`ghRateLimitHint` (all documented as MODEL-FACING notes — text written for the
agent to read, which nothing draws); `structuredContent` (untyped `unknown[]`,
and modelling it would be guessing at a shape).

**FLAGGED, not modelled: `BashOutput.gitOperation`.** Explicitly marked
client-facing — "lets clients render git activity without re-parsing stdout" —
carrying commit sha and kind, push branch, and PR number/action. Nothing drawn
consumes it: the merge bubble is DAEMON-orchestrated merges, not the agent's own
git commands. If agent-made commits should be drawn as something other than
shell output, that is a UI decision and a new element, not a field here.

**NON-OBVIOUS IMPLEMENTATION CONSEQUENCES, named concretely.**

- The shim must emit an announcement frame at `content_block_start` for EVERY
  tool call, not only at the tool's return. It has the call at that point (the
  verified `run_in_background` trace shows the call streamed before
  `task_started`), so no new observation is needed — but the emit site is new,
  and there are six kinds of it.
- The shim owns the start instant. It must stamp it at the announcement and NOT
  restate an elapsed figure, discarding the vendor's `elapsed_time_seconds` and
  `heartbeat` (`sdk.d.ts:4553-4572`) rather than forwarding them.
- The vendor's `heartbeat` flag is the ONLY evidence of a wedged tool call, and
  under this shape nothing on the wire carries it. Consistent with the settled
  layering — the party that can see the silence reports it — so the SHIM must
  surface a wedge as the item's `failure` arm. That obligation is now
  load-bearing rather than incidental, and it is recorded here because nothing
  in the schema implies it.
- `webapp/src/async-bubble.ts`'s offset-checked append handling and its spool
  decoder stay RELEVANT for detached shells (the spool is still a delta stream
  on that surface) but its 3-tier identity ladder still goes, per the feed
  entry.
- Grep and glob renumber their result arms and gain a field at tag 4; every
  consumer switching on those oneofs recompiles.

**Recorded to the VETTING REGISTER** (`figma-to-idl-redesign.vetting.md`, new
this increment): that the sidecar is a SECOND producer needing its own
verification pass, and that `fake-query.ts` — the shim's hand-written stand-in
for the SDK's `query()` — can only ever confirm our own reading, since we wrote
it and it agrees with us wherever we are wrong. Two landed decisions rest on it
alone, one load-bearing (that a spawn is announced DURING the turn, which is why
the turn must be a stream at all). The user called this "a highly solveable
problem": the fix is to build the fake's scripts FROM captured transcripts
rather than by hand, so it stops being able to agree with us by construction.

### Every landed field, oneof and message carries documentation; the standard is a skill convention

**What changed.** A full documentation pass over the four `.proto` files
reworked in this wave. Coverage before and after, counted as declarations
(field, oneof and message) carrying a preceding comment:

| File | Before | After |
|---|---|---|
| `conversation/v1/turn.proto` | 131/205 | 205/205 |
| `conversation/v1/api.proto` | 38/40 | 40/40 |
| `frontend/v1/daemon_hold.proto` | 37/51 | 51/51 |
| `workspace/v1/workspace.proto` | 6/6 | 6/6 |

**The systematic omission the audit exposed.** Nearly every gap was a `oneof`
ARM LINE. The arm's target message was documented; the line selecting it was
bare. That is precisely backwards for a reader: the arm line is where a
consumer decides whether to handle the case at all, so the arm line is where
"expect this when …" belongs, and the message body is where the payload's
meaning belongs.

**What a landed comment must carry.** Why the declaration exists, when
precisely a producer sets it and a consumer sees it, what the consumer does
with it, and any integration gotcha established while it was designed — for
example that a glob floor is drawn as "at least N" and never as a total, that
prose and reasoning text are cumulative rather than deltas so nothing
accumulates client-side, that a settled response block does not mean the turn
settled, that a withheld thinking block draws nothing at all rather than an
empty card, and that a task in the paused arm is not terminal.

**What it must never carry.** Any reference to the development process. A
comment states the standing fact; it never narrates how the shape was reached,
what it used to be, or who asked for it. That history is this document's job.

**Codified.** `/create-or-update-protobufs` now requires this standard of every
landing, so a landing with an undocumented declaration is incomplete rather
than merely untidy.

### Grep and glob bodies land; the COMPLETENESS FACTORING pattern, and the landed-comment standard

**What changed ("looks good then").** `TurnAgentGrep` = { success | failure },
success carrying `oneof matches { content | files | count }` — the vendor's
`GrepOutput` is one flat object whose fields apply PER MODE (content/numLines
for content mode, filenames/numFiles for files mode, numMatches for count mode),
so each arm carries only what applies. `TurnAgentGlob` = { success | failure }
over paths plus a completeness arm.

**THE FACTORING PATTERN the user named, now a skill convention.** The
orchestrator wrote `uint32 lines = 2; uint32 total_lines = 3;`. The user: "two
adjacent fields for which the value of one depends on the value of the other
screams oneof factor." Correct, and it is a special case of adjacent
exclusivity — `total_*` means nothing when the answer is complete. Factored to
`oneof extent { all | partial }`, and CRUCIALLY the partial arm carries what was
OMITTED rather than a total: that is the figure a reader is shown ("42 more not
shown"), it needs no arithmetic, and a total is recoverable by addition. Sent to
the skill by a one-shot subagent (worktree proto-complete-vs-partial).

**A second completeness distinction, nested.** Glob's own total can be a FLOOR —
the underlying search caps its own counting, which the vendor states explicitly
(`countIsComplete`). Rather than a bool qualifying a number, the omitted figure
carries `oneof omitted { exact | at_least }`, because "42 more" and "at least 42
more" are different claims and only one is safe to draw.

**Dropped from the vendor's outputs, each for a stated reason**: grep's
`appliedLimit`/`appliedOffset` (the partial arm already says the answer is short;
the cap that produced it is the producer's business), and glob's `durationMs`
(nothing draws it).

**Domain outcomes, restated at these shapes**: a search that matched NOTHING is
a success with an empty answer, never a failure — the caller asked a question and
got one. Neither tool has an update arm: there is no partial grep anything draws.

**THE LANDED-COMMENT STANDARD, set by the user as a standing rule.** Every
landed message and field carries documentation written FOR A FUTURE INTEGRATOR —
what it is in domain terms, why the shape exists and what it rules out, how and
when to use it, producer obligations, and integration gotchas (a floor that must
not be drawn as a total, a resolved value that must not be re-derived, an order
that carries no ranking). Design knowledge established in conversation is
CARRIED INTO the comment, because knowledge that lives only in this document is
unavailable at the call site. And a HARD PROHIBITION, permanent rather than
sketch-scoped: landed comments NEVER reference the development process — no
earlier drafts, no objections, no amendments, no "used to be". State the result
and its reasoning as a standing fact about the contract; the history belongs
here, in the record, and nowhere else. Dispatched to the skill by a one-shot
subagent (worktree proto-comment-standard).


### Write and edit bodies land on the VENDOR's structured patch, not a reconstruction of it

**The user's challenge that changed the design.** The orchestrator had the shim
building diff hunks itself — locating each match, capturing context lines at
edit time — and argued at length for why context had to be captured rather than
reconstructed. The user pushed back: "are you SURE this information isn't
returned by the SDK? Seems very surprising that it wouldn't be", noting Claude
Code plainly draws context around edits.

**It is returned, fully structured.** `SDKAssistantMessage` carries a per-tool
structured output object — "the tool's full Output object, not the string
content sent to the model... keyed by the matching tool_use block's name" — and
`sdk-tools.d.ts` declares them. `FileEditOutput` = { filePath, oldString,
newString, originalFile, structuredPatch[{oldStart, oldLines, newStart, newLines,
lines[]}], userModified, replaceAll, gitDiff? }. `FileWriteOutput` = { type:
"create"|"update", filePath, content, structuredPatch[...], originalFile,
gitDiff?, userModified? }. ROOT CAUSE of the error: the orchestrator reasoned
from the tool's INPUT schema and never looked for a structured output type, then
built an elaborate justification for work the producer already does.

**What landed.** `FilePatchHunk` = { FilePatchHunkRange old_range;
FilePatchHunkRange new_range; repeated string lines } with the ranges
encapsulated at the user's instruction rather than four bare integers.
`TurnAgentWriteSuccess` = { path; oneof outcome { created | updated }; patch;
user_modified } — the outcome arms being the vendor's own `type:
"create"|"update"`. `TurnAgentEditSuccess` = { path; patch; user_modified }.
Neither has an update arm.

**Write versus edit, since the distinction was unclear and is worth recording.**
The difference is in the INPUT, not the output. `FileWriteInput` = { file_path,
content } — hands over the whole new contents, creating the file or replacing it
wholesale. `FileEditInput` = { file_path, old_string, new_string, replace_all? }
— a surgical string replacement that must match, and must match uniquely unless
every occurrence was asked for. BOTH return a structured patch because the
producer diffs after the fact so a UI can draw the CHANGE rather than a whole
file; hunks are the presentation of the change, not a description of the
operation.

**`replace_all` needs no field.** Its effect is the number of hunks, so the
occurrence count is an observable rather than a claim a consumer must reconcile.

**`user_modified` is the allow-with-edits observable.** The vendor: "True when
the user edited the proposed content in the permission dialog before accepting."
It is the only signal that what landed is not what the agent proposed. Today it
is always false in this stack because our permission card offers no editing
affordance — the shim's `PermissionResponse.updated_input` supports it and no UI
does, already flagged in stage 3 — so the field is what makes the result honest
the moment that affordance is built, with no further contract change.

**Deliberately dropped from the vendor's output**: `originalFile` (the entire
pre-change file, which the hunks already summarise) and `gitDiff` (its counts
are derivable from the hunks, and its repository fields belong to a different
concern).


### The read tool: extent as a two-arm oneof; who highlights, and where the layers divide

**What changed ("b seems best", "no need for range, so two arms is fine").**
`TurnAgentRead` = { success | failure }, no update arm — a read either returns
or fails and nothing draws a partial one. Success = { ReadPath path; oneof
extent { whole | head } }, where `head` carries `total_lines`. A truncation BOOL
was rejected in favour of the arm, per the mode-selecting-bool convention: a
whole read then carries no truncation vocabulary, and a short one cannot omit the
figure that makes it legible. A third `range` arm for the tool's own
offset/limit was offered and declined.

**A HEAD, never a middle slice.** A short read is always the file's leading
portion, because a highlighter reading from offset zero has correct context while
text beginning mid-string or mid-comment is mis-highlighted until it happens to
resync.

**Highlighting: the daemon owns it, and the client stops deciding.** The user
asked whether truncated content can still be highlighted. Investigated: the
webapp does NOT use tree-sitter — it uses HIGHLIGHT.JS core with twenty
registered grammars, plus `languageForPath` and `highlightCode`
(`webapp/src/highlight.ts`). Being a regex/state-machine highlighter rather than
a parser, it handles a head fragment fine. The user then ruled that the parser
belongs in the daemon ("fast Go that can call out to a C parser"), so the daemon
highlights and the client paints spans it is handed. CONSEQUENCES:
`languageForPath` and `highlightCode` leave the webapp, highlight.js and its
twenty grammars leave the bundle, and the daemon needs a grammar set at least as
wide or files silently lose highlighting they have today.

**A retraction, kept visible.** The orchestrator argued resolved spans would
make the wire "much bigger". WRONG — packed varint deltas of (offset, length,
class) run the same order as the text itself. The user challenged it and the
claim was withdrawn; it had been asserted without estimating.

**The layer division, restated because the user corrected the orchestrator's
framing.** `conversation.v1` is the shim→daemon protocol AND the shared
conversation vocabulary that `frontend.v1` embeds — not a frontend-only surface,
and not exclusively the wire. So for a read: the PATH is embedded by the view
verbatim (one fact, one form), the CONTENTS are resolved by the daemon into
highlight spans (the one thing a client must not derive), and the truncation
becomes a composed affordance ("showing 200 of 4,312") rather than a bare bool
the client formats.

**Owed to frontend.v1's turn.** `FeedToolReadSpan.token_class` should be a
closed arm set rather than a string, since it names a paint class the stylesheet
must have a rule for.


### Thinking and response bodies land; the `update` arm is per-kind, and usage rides both arms

**What changed ("looks good, let's proceed").** `TurnAgentThinking` and
`TurnAgentResponse` gain their bodies. Thinking: update/success/failure, with
both the update and the success carrying `oneof reasoning { text | withheld }`,
and usage on the success. Response: update/success/failure, prose-so-far on the
update, whole prose on the success, partial prose on the failure, and usage on
ALL THREE.

**THE `update` ARM IS PER-KIND, derived not blanket.** The user proposed
dropping `update` entirely — growth as repeated `success` frames. Resisted, and
the reason is drawn: thinking's disclosure sits OPEN with a live indicator while
arriving and COLLAPSES when settled (`render.ts:852-859`), so repeated successes
make "collapse now" unrepresentable and `success` stops meaning finished. The
rule adopted instead: an `update` arm exists IFF the kind has an intermediate
state a consumer draws differently. Response and bash have one; grep, glob and a
task act do not, and get two arms.

**IDENTITY IS THE ENVELOPE'S, never a field on the kind.** The user asked how
two `TurnAgentBash` frames are told apart. `TurnProgress.progress_item_id`
answers it. Inferring instead — "a new update after a success is a new call" —
BREAKS OUTRIGHT, because the agent issues PARALLEL tool calls in one message and
their updates interleave. Ids come from the vendor where one exists
(`tool_use_id`, already how the webapp keys tool cards) and from
`messageId`+block index for text and thinking. Settled with it: the SHIM mints
ONE id per unit for its whole life, replacing the webapp's current
preview-id-then-record-uuid switch that is deduped by hand
(`store.ts:138-146`).

**USAGE ON THE UPDATE ARM, the user's catch.** The orchestrator put usage only
on `success`. Wrong: the vendor states usage when the message OPENS — final
input and cache counters, INTERIM output — and restates it on a later stream
frame, which `ResponseUsageCorrected`'s own comment already documents and the
fixture confirms (`message_start` carries usage, `message_delta` carries it
again). So usage rides the update too, and the footer's live token figure has a
producer.

**A bookkeeping arm dies as a consequence.**
`BookkeepingEntry.response_usage_corrected` exists ONLY because a flat log
cannot restate a record — "a producer that could only state usage on the
response would have to either withhold the response until the turn ended or
restate it, and neither is available to it". Under upsert semantics restating IS
the mechanism, so the correction is simply the next frame.

**FINALITY IS NOT MODELLED HERE, and the tracing that settled why.** The user
asked what the three bubble states are at the SDK level. Traced: (1) the
streaming updates are `content_block_delta`/`text_delta` frames drawn as a
preview; (2) a NON-green-bordered purple bubble between tool calls is a SETTLED
text block — an `assistant` message's block that arrived whole; (3) the
green-bordered one is THE SAME THING, plus a border the client assigns when the
turn's `result` lands, to whichever top-level text block was last
(`finalResponses`, `render.ts:2092`). So (1) and (3) are the same SDK object and
finality is NOT a wire fact — `nav.ts:73` says so outright: "Finality is not a
wire fact — it is derived per render". Since a producer cannot know while
writing a response whether anything follows it, the turn's own conclusion names
the answering response rather than a finality field living here.

**Unsettled, flagged rather than assumed.** `stop_reason` would be the natural
discriminator ("tool_use" intermediate, "end_turn" final), but every assistant
message in our own fixture reports `end_turn`, INCLUDING turns carrying
`tool_use` blocks (six occurrences, all `end_turn`). Either the fixture is
unfaithful there or the agent binary normalizes it; settling that needs a real
transcript, and it decides only whether the producer could state finality
directly.

**What thinking IS, recorded because it was asked and is not obvious from the
schema**: the model's scratchpad — reasoning on the way to an answer, not
addressed to the user, collapsed by default, frequently WITHHELD entirely
(adaptive-thinking models emit the block and a signature with no text, so the
UI draws no card at all and nothing survives once it closes), and charged as its
own token class, which is why a footer draws a thinking figure.


### The task tracker: acts, not task lifecycles; and the rule for when a unit earns its own stream

**What changed.** `turn.proto` gains `TurnAgentTaskAct` = { task; oneof act
{ created | changed }; state } plus `TurnAgentTaskState` (subject, description,
optional owner, status oneof) and `TurnAgentTaskId`. The `send_message` arm
lands beside it. The shown/hidden presentation oneof on
`TurnDetachableWorkCreated` is DROPPED.

**THE RULE the user's questions produced, which generalizes beyond tasks.** A
unit earns its own stream IFF something produces updates for it OUTSIDE any
turn. A shell call and a subagent qualify — the process keeps running and
emitting after the turn ends, and the vendor confirms membership in its
live-background-task set. A task tracker entry does NOT: every change to it is
an agent calling a tool, and every tool call happens inside some turn, so a
per-task stream would be the shim re-broadcasting turn events onto a channel it
invented rather than reflecting one.

**The distinction the user drew, in his terms.** A task "is inherently tied to
an agent, and can't be backgrounded (well, the agent can be backgrounded, but
relative to an agent, it cannot be backgrounded, like, say, bash call can)". So
two axes: work DETACHABLE from its agent (bash, subagent spawn — the closed
`TurnDetachableWork` set) versus work BOUND to its agent (thinking, responses,
reads, greps, task acts, skill use, questions). Bound work still changes streams
— not by detaching, but because the AGENT is on a different stream when it is a
backgrounded subagent. Consequence: a task created in one turn and completed by
a detached agent that owns it arrives as two acts on two different streams,
joined by `TurnAgentTaskId`.

**ACTS versus LIFECYCLES — an orchestrator conflation the user caught.** The
orchestrator first modelled `TurnAgentTaskUpdate` with `update`/`success`/
`failure` arms. Those describe the TASK's lifecycle, which spans turns, while a
progress item is one ACT, which is instantaneous. An act has a tool-call
outcome; a task has a lifecycle; they cannot share one oneof. ROOT CAUSE:
applying the bounded-stream frame convention to something that is not a stream.
The landed shape separates them — the arm says WHAT HAPPENED, `state` says WHERE
IT LEFT THE TASK — so every consumer reads `state` regardless of the arm and the
arm only decides whether to add a row or update one. Both act arms are therefore
EMPTY, including `created`: the user asked whether `owner` belonged on the
create, and it does not, because `state` rides every act and a second copy could
disagree with it.

**Adjacent-exclusivity fix, also the user's catch.** `active_form` was a sibling
optional on the state; it means nothing for a task that is not running, so it
moved INTO `TurnAgentTaskRunning`.

**Status arms are the VENDOR's set, not invented**: pending, running, completed,
failed, killed, paused — taken from the SDK's own task patch type
(`'pending' | 'running' | 'completed' | 'failed' | 'killed' | 'paused'`). The
orchestrator's first sketch had four and was corrected against the type. And
`stopped` is NOT an act arm: stopping is a change whose resulting status is
`killed`, so there is one spelling rather than two.

**The shown/hidden drop, and why.** Ambient work (`skip_transcript`) was going
to get a two-arm presentation oneof. The user: an observer "seems like it's a
normal agent response arm" — correct. The only concrete generator found in the
SDK is an auto-spawned background OBSERVER (`observer` on an agent definition:
"receives read-only activity digests and reports via the ObserverReport tool; it
never participates in the task"), which is an ordinary agent spawn; and no
producer in THIS stack is known to set the flag. Derived-not-invented applies:
the arm waits for a real producer.

**Still owed.** Whether `ToolCallTaskStop.task_id` names the same id space as
the vendor's background `task_id` — it does not change this shape (the producer
is an agent in a turn either way), but it decides whether a Stop tool call and a
`stopTask` control request can target the same object. Implementation-wave
question, recorded so it is not re-litigated as a contract one.


### conversation.v1's TURN UPDATE MODEL lands: TurnUpdate / TurnProgress / TurnDetachedWork

**What changed ("yeah looks great").** `turn.proto` gains the protocol model's
update structure. `TurnUpdate`'s arm states WHAT THE CONSUMER MUST DO — pipe it
back on the turn's stream (`progress`) or open a stream of its own for it
(`detached_work`) — which the user identified as the axis that matters at this
level. `TurnProgress` = { progress_item_id; oneof progress_item } over thirteen
kinds. `TurnDetachedWork` = { work; oneof origin { detached | created } }.
`TurnDetachableWork` is a SMALL separate type (agent, bash) so a kind absent
from it cannot claim to be detached.

**THE FACTUAL QUESTION THE USER FORCED, and its answer.** The orchestrator had
modelled detachment as always a TRANSITION of an item already announced as
progress. The user doubted it: can work be created detached with no in-turn
update at all? ANSWER: YES, and the SDK type proves it — `SDKTaskStartedMessage`
carries `tool_use_id?` as OPTIONAL, so a task can exist with NO originating tool
call (ambient/housekeeping work, which also carries `skip_transcript`). For the
ordinary `run_in_background` path the call IS streamed first
(`content_block_start name="Task"` -> `input_json_delta {run_in_background:true}`
-> `task_started` -> `tool_result "Agent running in the background with ID"`),
so there the call is never invisible — it simply never has a foreground RUNNING
phase. Both arms are therefore real, and the user's two-message shape is the
correct one.

**Amendments the orchestrator made to the user's sketch, each with its reason.**
(1) `token_usage_update` left the `TurnProgress` envelope — a read, a search or
a shell call has no token cost, so an envelope field is meaningless for most
arms; usage rides the arms that have it. (2) Three kinds the sketch omitted were
added: glob, workflow, unmodeled. (3) `TurnDetachableWorkDetached` carries NO
work payload beside the id — the consumer already received the description as
the progress unit named there, and a second copy could disagree with the first.
(4) The vendor's task handle moved to the `TurnDetachedWork` envelope, since
both origin arms have one. (5) The top-level `oneof result { update | success |
failure }` is NOT in conversation.v1: a turn concludes on the stream that
carries it, so the terminal arms belong to that rpc's response in shim.v1 —
which also resolves the `TurnResponse` vs `TurnAgentResponse` name collision the
user had flagged as unsettled in his own sketch.

**Verified, so it is not re-derived.** Identity is per BLOCK, not per response:
the webapp's item kinds are `user-turn | text | thinking | tool | permission |
result | failure`, `thinking` has its own renderer distinct from the response's
`TextStream`, and BOTH are keyed by `blockId` (`itemKey`, render.ts). So the
agent's reasoning and its prose are separate units, each holding one identity
across its fragments.

**OWED, not decided.** Ambient work carries `skip_transcript` ("consumers should
hide this from the inline transcript; it may still appear in a tasks panel") —
live, cancellable work that must not be drawn as a bubble. Either
`TurnDetachableWorkCreated` carries that flag or ambient tasks are excluded from
this surface; raised with the user, still open. Also owed: a ruling on
`SendMessage` / `TaskCreate` / `TaskUpdate` / `TaskStop`, which have no drawn
treatment (own arms, folded into unmodeled, or absent from the protocol).

**The package does not compile**, by design: every `TurnAgent*` arm body lands
at its own increment.


### NO COLLECTIVE NOUN for the protocol model's units: the stream frame names the kinds directly

**Settled ("(c) I agree with too").** The orchestrator had been calling the
protocol model's units "nodes"; the user asked for a better term. Three shapes
were offered — a `TurnItem` container, a `TurnPart` container, or NO collective
noun with the stream frame's oneof naming the concrete kinds. The third was
chosen: there is no abstract container message, so a response and a tool call
are DELIVERED INDEPENDENTLY, each stamped with its `TurnId`, and nothing has to
name the category they share.

**Words ruled out, with reasons, so they are not re-proposed.** "element" —
figma→idl owns it for UI elements; "entry" — `StoreEntry` owns it on the
datalayer side and reusing it re-blurs the split just made; "event" — these
units have state and SETTLE, which events do not; "node" — the orchestrator's
own placeholder, rejected by the user.


### THE PROTOCOL MODEL IS NODES, NOT A LOG: identity per THING, upserted ("model two, for sure")

**The two candidates, and what separated them.** Model 1 (today) gives identity
PER ARRIVAL: a draft (`ContentArriving`) and its final (`AgentSaid`) are
separate entries with separate ids, and a tool return is an entry joined to its
call by `tool_call_id`. Model 2 gives identity PER THING: a response holds one
identity from its first fragment to its last, growth is a re-send of that node,
and a return is an ARM of its call rather than a separate entry.

**Why model 2.** `frontend.v1` ALREADY consumes model 2 — a `FeedRow` upserted
by id, with `FeedAgent.result.update` carrying prose-so-far — so today the
DAEMON converts 1 to 2, and the stage-2 record states the bridge outright: "the
row exists from the first `ContentArriving` fragment under its future
`AgentSaid` id". That is a consumer minting an identity for a record it has not
seen, which is the oddest thing in the current design. With the shim now the
boundary owner and mapper, the fold belongs there.

**Two orchestrator errors on the way, kept visible.** (1) It claimed of the flat
log that "nobody consumes it that way", implying batch assembly; the user
corrected it — the log DOES drive incremental rendering, entry by entry. The
real distinction is not batch-vs-incremental but WHAT AN UPDATE IS ADDRESSED TO.
(2) Asked repeatedly to explain the distinction, the orchestrator explained it
in prose three times before showing two small proto blocks, at which point the
user settled it immediately. ROOT CAUSE: explaining a schema distinction in a
medium that cannot express it. A skill convention was dispatched for this
(worktree proto-examples-over-prose): every concept, distinction or concern put
to the user carries exemplifying protobuf whenever a schema can illustrate it.

**Consequences.**

- `ContentArriving` as a payload arm DIES; prose-so-far becomes the response
  node's `update` arm.
- `ToolReturned` as a standalone entry DIES; the return becomes the tool-call
  node's success/failure arm.
- The SHIM performs the fold — accumulating fragments into a response,
  attaching a return to its call, applying usage corrections — and the flat
  faithful log stays on the datalayer side (`StoreEntry`).
- The daemon stops inventing identities for records it has not seen.


### REVERSION: stage 1 (conversation.v1) is RE-ENTERED to design the PROTOCOL model; the decisions this reopens, by name

**Why the reversion.** conversation.v1 was recorded COMPLETE at `363323f1b` as
the shared record model. The datalayer/protocol split makes it something else —
the PROTOCOL model, shaped for consumer concerns — so its shapes are re-decided
rather than renamed. Per the skill's reopen rule the downstream decisions are
enumerated here before the stage is re-entered, and none is silently kept.

**Taken as the reason to detour rather than push on**: the sequence put
conversation.v1 FIRST precisely so importers are not reopened by the leaf
changing underneath them. Sketching shim.v1's turn stream — a carrier for this
model — before the model is settled would be the bottom-up inversion the
sequence exists to prevent.

**REOPENED, BY NAME.**

- THE GRANULARITY COLLISION, which is a SHAPE question and not a rename. One
  type, `MessageId`, currently names two granularities: a RECORD's identity
  (`MessageEntry.message_id`) and a RENDERABLE ROW's identity
  (`StoredMessage.message_id`, the thing a page counts). Every settled decision
  that embedded `MessageId` therefore has an unresolved question about WHICH it
  meant:
  - stage 2 `feed.proto`: `FeedRow.id`, `FeedRow.parent`, `FeedBreadcrumb.target`
    — all ROW identities.
  - stage 2 `footer.proto`: `FooterExpandedRow.target` — a jump target, a ROW.
  - stage 3 `endpoint_get_feed_page.proto`: the `parent` container arm — a ROW.
  - stage 3 `endpoint_interrupt.proto`: the detached target — an item, whose
    granularity is now a question.
- EVERY conversation.v1 TYPE NAME the settled stages embed, which move with the
  protocol model: `MessageId`, `MessageEntry`, `MessagePayload`, `UserSaid`,
  `AgentSaid`, `AgentContent`, `ToolCallId`, `ToolCallBlock`, `ToolReturned`,
  the `Permission*` family, `ApiRequestFailed`, `ContextCut`, the
  `DetachedWork*` family, `ContentArriving`, `StopReason`. Consumers in
  frontend.v1 (feed, footer, topbar, daemon_hold) and agentrepl.v1
  (submit_prompt, get_feed_page, interrupt, answer_permission, set_model) are
  repointed as each family settles.
- STAGE 4's turn-stream content arm, which was mid-sketch. Its content frame is
  a settled import once this stage closes.

**NOT reopened.** `TurnId` (already the coarsest unit and already correctly
named), the stage-2 figma→idl view shapes themselves, the stage-3 service
sections and RPC inventories, and every convention landed in 3b and stage 4.


### THE DATALAYER AND THE PROTOCOL BECOME TWO MODELS: `StoreEntry` (persistence) and conversation.v1's Turn family (protocol)

**The user's ruling.** "Current ExternalEntry should probably be just folded
along with InternalEntry into a StoreEntry message (we'll want to be careful
about this), and what's in conversation.v1 is the TurnResponse/TurnToolCall/
whatever model we settle on, which would be a protocol version of the message
(with StoreEntry being the datalayer level of the model)."

**How the argument got there, including the orchestrator's WRONG objection.**
The orchestrator objected that the split already existed — persistence concerns
quarantined in `store.v1.InternalEntry`, `conversation.v1` being the neutral
domain model the store merely embeds — and warned a protocol respelling would
be a THIRD spelling with no divergence detection. The user asked it to CONFIRM
that `ExternalEntry` is not sent to the store. IT IS: `store.v1.Entry` embeds
it (`store/v1/entry.proto:79`) and `EntryBatch` carries `repeated Entry`, so
the write path hands the store both halves. `ExternalEntry` is therefore not
"the protocol half" at all — it is the SHARED half, written AND served, which
makes `conversation.v1.MessageEntry` genuinely the persisted shape. ROOT CAUSE
of the orchestrator's error: it took `external` to mean "the outward-facing
projection" from the name and the daemon-import discipline, without checking the
write path. The objection collapsed on the fact and the user's position was
correct.

**The granularity collision this fixes, surfaced on the way.** One word,
"message", carries THREE granularities in the current package: a RECORD (one
thing that happened — `MessageEntry`), a RENDERABLE ROW (a unit composed of
many records, which is what a page counts — `StoredMessage.message_id`,
`message-page.proto`'s "one of them can own hundreds of durable records"), and a
TURN (a prompt and everything it produced). `MessageEntry.message_id` and
`StoredMessage.message_id` are the SAME TYPE naming DIFFERENT granularities.
That collision is the symptom of one model serving both persistence and
consumption.

**What is settled.**

- `store.v1` owns the DATALAYER model: `StoreEntry`, folding today's
  `InternalEntry` and the contents of `shim.v1.ExternalEntry`. Shaped for
  storage concerns — provenance, dedup, position, unconvertible material.
- `conversation.v1` owns the PROTOCOL model: the Turn family
  (`TurnResponse`/`TurnToolCall`/detached work and the rest, exact names
  settled at their increments). Shaped for CONSUMER concerns.
- The SHIM owns the mapping, being the component that writes persistence and
  serves the protocol. ONE mapping, one place, one author.
- `shim.v1.ExternalEntry` therefore ceases to exist as a shared half; the
  daemon-facing wire carries the protocol model.

**Accepted costs, stated because the redesign has otherwise held the
no-respell rule throughout.**

- A hand-maintained mapping has NO COMPILER to detect divergence. The mitigation
  is owed to the wave: tests that FAIL when a field is added on one side and not
  mapped.
- The guarantee being given up is the current design's "producing the daemon's
  view is the field access `entry.external`, so there is no mapping function
  that can forget a field, drift, or be updated on one side only". That
  guarantee was real; it is traded for two models each shaped for its own
  consumer.
- `BookkeepingEntry` is NOT renamed into the Turn family: its arms
  (`SessionBegan`, `SessionEnded`, `SessionIdentityChanged`,
  `AccountUsageObservation`) are SESSION-scoped, not turn-scoped, and a
  `Turn...` container would misname over half of them. Its no-message-id
  property is deliberate and stays: a page counts messages, so a boundary fact
  able to name one would silently spend a page slot.
- "Be careful about this" is the user's own caveat on the fold, recorded as
  such.

**The convention is being generalized into the skill** by a one-shot subagent
(worktree proto-datalayer-vs-protocol): a persistence model and a
client-protocol model of the same data are legitimately different, licensed by a
SINGLE owner of the mapping, and it does not license two spellings on either
side, an unowned mapping, or restating typed identities.


### STANDING POLICY: the STORE IS NUKED, NEVER MIGRATED — no backfill, no hydration, no schema migration

**The user's instruction, verbatim in substance.** "The store will be entirely
nuked of its contents, so we don't care about backfilling/hydrating/migrating
the database, and should explicitly avoid this. During development process, we
should be nuking the store as needed and never trying to persist it."

**What this LICENSES, and it is load-bearing for the whole redesign.** Every
renaming, re-shaping and re-homing in this redesign may break the durable
schema freely. No migration path is designed, no dual-read is written, no
compatibility arm is kept alive for old rows, and no field is retained merely
because persisted data names it. A durable-compatibility argument is NOT a
reason to keep a shape.

**What it FORBIDS.** An implementation agent must NOT write backfill,
hydration, or migration code for the store, and must NOT preserve a message,
field or arm on durable-compatibility grounds. Where existing contents are in
the way, the store is DROPPED and recreated.

**Why it is recorded here.** A downstream implementer arriving fresh would
otherwise treat the durable store as a constraint — it is the one artifact in
the system that looks like it must be migrated — and would either preserve dead
shapes or spend the wave writing migrations the user explicitly does not want.


### NO `seq` ON THE DAEMON-FACING WIRE: history is first/next with an OPAQUE continuation token, and the turn stream carries no position

**The user's ruling.** "Not sure if there's a legitimate reason for the daemon
to know about the seq value... the daemon only needs to ask the shim for the
first page, and then subsequently use whatever identifier is returned to get
the next page... I'm not sure if that identifier needs to be a leaky
abstraction (seq)... and I'm definitely not thinking that turn needs to return
it at all (this is additional complexity because the turn handler in daemon now
needs to integrate with the history handler, which seems just bad)."

**The coupling argument is the decisive one**: a turn frame carrying positions
makes the turn handler a participant in history, which is the wrong seam.

**The one real need, unpacked and satisfied without seq.** The daemon's only
position-shaped need is CATCH-UP AFTER ITS OWN DOWNTIME — a turn can run while
the daemon restarts, and its session state machine, accounting and turn ledger
need those durable records (today read by seq range). That need is ordered
PAGINATION, not seq. So the page identifier is an OPAQUE CONTINUATION TOKEN
minted by the shim and echoed back verbatim (the typed-echo-token convention);
`seq` stays the store's addressing, exactly as the current design already says
("a position is the store's addressing, not a fact about a conversation").

**Settled consequences.**

- The turn stream's content frame carries THE RECORD AND NOTHING ELSE — no
  position, no stored/live distinction.
- History is `first` / `next`, as `GetFeedPage` settled one layer up.
- The daemon persists an OPAQUE TOKEN instead of a number, so advancing a
  cursor past a position the store never assigned becomes UNREPRESENTABLE
  rather than guarded — the hazard `EntryDelivery`'s stored/live split was
  built to prevent.

**A DELIBERATE divergence from GetFeedPage, stated so it does not read as an
inconsistency.** There the DAEMON holds each container's walk position and the
webapp says only first/next, because a webview's walk dies with the webview.
Here the token is a value the DAEMON PERSISTS rather than a position the shim
remembers for it, because the daemon must survive its OWN restart and resume
where it left off. Same no-leaky-cursor discipline, different holder, for a
stated reason.


### 4b TURN SECTION: two rpcs — SubmitPrompt + UpdatePrompt; the DAEMON is the only queue and the only submitter; the heartbeat concept LEAVES shim.v1

**THE INVENTORY (the user's consolidation).** Two rpcs: `SubmitPrompt` (open a
prompt's stream) and `UpdatePrompt` (speak to an open one, `oneof action
{ interrupt | answer_permission }`). The user's framing: "shouldn't answer
permission/interrupt/cancel be part of one request RPC... so the bifurcation is
submitting a prompt and updating a prompt, semantically." Both are mid-flight
INPUTS to work in progress and differ only in what they say; each arm carries
its own exclusive payload, which is what makes it a proper oneof.

**Submit and the turn are ONE rpc.** A separate watch verb would need a turn id
minted by something and would reopen the accepted-vs-attached gap the bring-up
gate exists to eliminate. Submitting IS opening the turn.

**THE DAEMON IS THE QUEUE — the agent binary's queue is never ours.** The
orchestrator proposed modelling a `queued` frame for prompts the agent binary
enqueues; the user rejected the premise: "sounds a little like queued is being
handled by the shim. that sounds wrong. seems that if a turn is in flight
(connect is there) that the daemon should immediately know that?" Correct — the
daemon holds every turn stream, so it knows in-flight structurally, and it
already holds waiting prompts in the daemon-hold tray. Two queues was the
defect. So: the daemon submits ONLY when no turn is in flight, an open stream
means a RUNNING turn, and `SubmitPrompt` REFUSES if one is already in flight —
a HARD FAULT that should be logically impossible, never a state to manage.
CONSEQUENCES: `CancelQueuedPrompt` never exists; `still_queued`/`cancel_queued`
become defensive evidence (non-empty means something submitted behind our back,
a fault to surface); the tray is the ONE authority for waiting prompts.

**ONE SUBMITTER, and the shim's YIELD obligation is what makes it structural.**
The orchestrator surfaced the hole: the shim's own cache keep-alive turns
occupy the main thread without the daemon submitting, making the daemon a
SECOND submitter and the invariant merely probable. Two fixes were offered
(daemon drives keep-alives; or shim announces them). The user chose a third:
keep-alives stay ENTIRELY INSIDE THE SHIM as an implementation detail. That is
sound ONLY because the shim already discharges the guarantor — a keep-alive
YIELDS to real work, discarding trailing keep-alive turns before a real prompt
(`SessionRewound` + `KeepAliveDiscard`). That obligation is hereby the
invariant's guarantor rather than an optimization.

**Keep-alives are invisible on the CONTROL plane, visible on the RECORD
plane.** They make real API calls, so their cost lands in accounting whether or
not anything announces them, and `KeepAliveDiscard.dropped_turn_ids` marks the
discarded turns superseded for readers to exclude ("never deleted, only
excluded from replay"). No control-plane signal is needed or wanted. OWED TO
THE HISTORY SECTION: a paged read must handle superseded turns.

**THE HEARTBEAT CONCEPT LEAVES shim.v1 ENTIRELY.** Asked what the heartbeats
would map to in the new architecture, the answer is that they dissolve:

- `ConnectionHeartbeat` -> NOTHING. Transport liveness is HTTP/2's; 3b already
  retracted in-band pings.
- Liveness of a work item -> THE STREAM BEING OPEN. No frame states it.
- `AgentHeartbeat.live_work_ids` (the set) -> THE SET OF OPEN ITEM STREAMS. The
  daemon holds it rather than receiving it.
- The vendor's per-item `tool_progress` detail -> AN UPDATE FRAME on that
  work's own stream (the turn's stream for in-turn tool use, the item's stream
  for detached work).

So both heartbeat messages DIE. `AgentHeartbeat` additionally should not be
DURABLE at all: replaying "this was running" long after it ended tells a reader
nothing — a dead feature in the durable plane, not a message needing a home.

**Facts the current schema DISCARDS that now get modelled, because they finally
have a place.** Verified against `sdk.d.ts`'s `SDKToolProgressMessage`: the
vendor sends `elapsed_time_seconds`, an explicit `heartbeat?: boolean`,
`subagent_type`, and a `subagent_retry` block (agent_id, attempt, max_retries,
retry_delay_ms, error_status, error_category) — all of which our
`AgentHeartbeat` flattens to a bare id list. The retry block in particular
finally gives the footer's existing `retrying` activity arm a real producer.

**A retraction, kept visible.** The orchestrator proposed and then withdrew a
`HeartbeatKeepAlive` arm on a typed heartbeat oneof. It has NO upstream source
— nothing the vendor sends is about cache warming — so it would have been a
shim invention relaying a fact the daemon does not need. ROOT CAUSE: designing
a control-plane signal for a fact that already reaches the daemon on the record
plane.

**VOCABULARY, owed as a sweep.** Bare "CLI" is retired as ambiguous. VERIFIED
layering: the SDK (`@anthropic-ai/claude-agent-sdk`, npm 0.3.220) SPAWNS the
Claude Code binary — its own `manifest.json` ships per-platform binaries named
`claude` (~256MB, checksummed, engine version 2.1.220, a DIFFERENT number from
the npm package's) and drives it with `--input-format stream-json
--output-format stream-json` (`pathToClaudeCodeExecutable`,
`spawnClaudeCodeProcess`). The SDK is a thin client; the ENGINE — prompt queue,
permissions, task lifecycle, compaction, plugins, OAuth — lives in the binary,
which is WHY Claude Code has features the SDK does not. The user's initial read
(Claude Code wraps the SDK) is inverted. Agreed vocabulary: "the agent binary"
for the engine, "the SDK" for the npm wrapper, "the vendor" for the
Claude-vs-other axis. OWED: sweep bare "CLI" from the shim's AGENTS.md and the
proto docs.


### WHAT A TURN IS, verified against the SDK; and DETACHED WORK gets one stream per item

**THE DEFINITION (the user's, verified at his instruction).** A TURN is the
window during which the main thread cannot ACCEPT a prompt — where ACCEPTING
means the prompt is fed into context and produces output tokens. Being able to
TYPE a prompt, and having one handled, are different facts.

**HOW IT WAS VERIFIED, recorded so nobody re-purchases it.** The user asked
which method to use before any claim was made; the SDK's own type surface was
chosen over web research or our shim's behavior, because it is authoritative
for the exact version we run. In
`agent-shim/claude/shim/node_modules/@anthropic-ai/claude-agent-sdk/sdk.d.ts`
(version 0.3.220):

- The vendor CLI holds a COMMAND QUEUE. A prompt submitted during a turn is
  ENQUEUED, not handled.
- A queued prompt runs AS ITS OWN LATER TURN: the docs name "the drain loop,
  which starts the next queued turn immediately", and describe a batch being
  "dequeued and coalesced into one turn".
- The queue is first-class on the wire: `interrupt()` returns `still_queued`
  ("uuids of async user messages that survive this interrupt... These WILL run
  unless cancelled first"), with `cancel_queued: true` to sweep them and
  `cancel_async_message` to drop one by uuid. Capabilities
  `interrupt_receipt_v1` / `interrupt_cancel_queued_v1` gate both.

So the definition is CONFIRMED, not inferred: a prompt is handled iff no turn
is in flight on the main thread. Detached work and subagent-addressed messages
are outside a turn entirely — the queue docs state subagent-addressed messages
are out of scope for the main-thread queue.

**CLI vs SDK, stated because the record needs the distinction.** "The CLI" is
the Claude Code executable; the SDK (`@anthropic-ai/claude-agent-sdk`) SPAWNS
it as a subprocess (`pathToClaudeCodeExecutable`, `spawnClaudeCodeProcess`) and
drives it over stdio. The QUEUE LIVES IN THE CLI, so its enqueue-vs-handle
behavior is a Claude Code fact our stack can only observe, never alter. They
version independently, which is why `system/init` advertises `capabilities` for
feature detection rather than version sniffing.

**ONE STREAM PER DETACHED ITEM (the user's proposal).** A session has a turn
stream plus one stream per in-flight detached item; a stream ends iff its work
concludes, so "zero item streams" IS "no detached work in flight" — liveness
becomes structural instead of derived. It deletes the phantom-task reconciler
(`daemon/internal/sessioncontroller/phantomtask.go`) and `QueryLiveTasks`'
steady-state role.

**THE DISCRIMINATOR IS THE USER'S TURN DEFINITION, and the vendor makes it
observable.** `SDKBackgroundTasksChangedMessage` (`system:background_tasks_changed`,
sdk.d.ts:2913) is "the full set of live background tasks, emitted whenever
membership changes (start, completion, kill, A FOREGROUND AGENT BEING
BACKGROUNDED)" — a LEVEL signal with REPLACE semantics, existing expressly "so
a missed bookend cannot wedge a stale running indicator". Membership in that
set IS "not blocking the main thread". So detachment is a fact with a DEFINED
INSTANT (the membership change), not a property of how work was launched —
`run_in_background: true` is merely the common way to enter the set at birth.
Ctrl+B (`backgroundTasks(toolUseId?)`) MIGRATES an item from the turn stream to
its own stream, and is an announced membership change rather than a corner
case.

**ANNOUNCEMENT RIDES THE SPAWNING STREAM; there is NO roster stream.** The turn
stream announces what the turn spawns, AS IT HAPPENS (not batched at turn end);
an item's stream announces what IT spawns. One rule applied recursively, so the
stream tree mirrors the work tree and provenance is implicit in which stream
announced an item. The alternative — a session-scoped roster stream publishing
the live set — was considered and REJECTED: it is a second authority for a fact
the streams already state structurally.

**A CORRECTION, KEPT VISIBLE.** The orchestrator claimed a turn-scoped
announcement could not work because "at the instant the response is written the
SDK has not spawned anything yet", and used that to argue for the roster
stream. WRONG: the shim's own fixture shows `task_started` is emitted DURING
the turn, before the turn's result. The claim was true only of a submit
answered at ACCEPTANCE (today's `Ack`), which the orchestrator had silently
assumed. ROOT CAUSE: reasoning from the current unary-ack shape while designing
a streaming one. The user's push ("are you sure?") is what surfaced it. Its
consequence — that the TURN MUST BE A STREAM, because it is the announcement
channel for work that outlives it — is a load-bearing conclusion of the
correction, not of the original claim.

**Consequences, each accepted.**

- CANCELLING AN ITEM IS A CALL, never closing the stream client-side: the
  landed bounded-stream convention reads a stream ending without a terminal
  frame as a TRANSPORT FAILURE, so a client-side close would be misread as one.
  The item's stream then concludes with its failure arm.
- `ShimHello.live_task_set` SURVIVES, on a new footing: the level is
  per-CLI-process and emits nothing at startup, so a REATTACHING daemon — which
  was not there for the announcements — must be told the current membership
  once, at the handshake. The orchestrator first claimed the streams killed it;
  they do not. A cold CLI start correctly resets to empty, because a dead CLI's
  background tasks are dead too.
- The daemon must NEVER pair start/end edges to maintain membership; the SDK
  states ordering between the level and the edges is unspecified. The SHIM
  consumes the level and presents a derived contract; the vendor's warning
  binds the shim, not the daemon.
- `skip_transcript` on `task_started` marks ambient/housekeeping work. It rides
  the stream as a RENDERING property — the stream still opens, because ambient
  work is live work that must be cancellable and must not wedge an indicator.
- Cross-stream live ORDERING is not meaningful and is not the daemon's to
  track: the store's seq is the durable order and each item renders in its own
  component. The orchestrator raised this as a concern; the user rejected it and
  the orchestrator withdrew it.


### STAGE 4 AMENDED: shim.v1 is designed as an RPC SERVICE, walked suites -> inventory -> shapes; FOUR sections settled

**The user's amendment.** "I'm thinking we should give shim.v1 the same
treatment as agentrepl.v1: Connect RPC service + endpoint_*.proto files. We
devise the RPC suites first, then the RPCs within each, then the
request/response protobufs for each RPC one at a time, landing after each."

**What this REPLACES.** Stage 4's recorded walk order was by FILE (`core` ->
`entry-delivery` -> `message-page` -> `external` -> `bookkeeping`, top-down by
containment). That order is RETIRED for this stage: the package is now walked
by SERVICE structure — 4a suites, 4b per-suite RPC inventory, 4c per-RPC
request+response shapes one at a time. The record files (`external`,
`bookkeeping`, `message-page`) are walked as shared-package VOCABULARY when an
endpoint needs them, not as increments of their own. The 3b conventions carry
in unchanged (response spelling, errors derived not invented, no keepalive).
The pattern was codified into the skill by a one-shot subagent — PR
"docs(create-or-update-protobufs): RPC-service file model and
suites-inventory-shapes iteration" (explanation-engine #7496, MERGED).

**Process correction, kept visible.** The orchestrator's first suite proposal
was derived by mapping the existing files one-by-one onto services. The user
rejected the method itself: "this sounds like bad process... we need to have a
holistic view of the files, and work out a service architecture from that."
ROOT CAUSE: taking the on-disk decomposition as the specification — the same
inversion the skill's homeless-message rule forbids, applied to files rather
than messages. The sections below were derived after reading all five files
whole.

**THE FOUR SECTIONS (settled, "sounds good").**

1. SESSION — wiring the session up, identity and rotation, health, teardown,
   session-level bookkeeping, AND the session state that conditions turns:
   model (catalog, selection, change) and permission mode.
2. TURN — submitting a prompt, the turn's own stream (its content, its spawn
   announcements, its permission asks), interrupting it, and the CLI's prompt
   QUEUE.
3. DETACHED WORK — the per-item stream (its content, its spawns, its
   permission asks), stopping an item, and backgrounding in-flight foreground
   work.
4. HISTORY — bounded reads of durable conversation: paged reads and replay
   ranges.

**Two sections the orchestrator proposed and the USER dissolved, with the
verification that settled each.**

- PERMISSION is not a section. The user: "are model and permission not a part
  of turn?" Half right, and the half that is wrong is the useful one. A
  canUseTool ask belongs to whichever unit is RUNNING A TOOL — the turn, but
  equally a backgrounded subagent with no turn open — so the ASK is a frame on
  the asking unit's stream and the ANSWER is a call addressed to that unit.
  One concern, split across two sections by the unit that owns it, never a
  section of its own.
- MODEL is not part of a turn. VERIFIED in the SDK we run
  (@anthropic-ai/claude-agent-sdk 0.3.220, sdk.d.ts:2327): `setModel(model?)`
  is "Change the model used for SUBSEQUENT RESPONSES", callable mid-turn and
  effective inside one (a turn holds several responses); `setPermissionMode`
  (sdk.d.ts:2300) is "for the current SESSION". Both are session state that
  CONDITIONS turns, so both live in SESSION.

**There is NO standing live-conversation stream.** Live content rides the turn
stream and the item streams (see the detached-work entry); what remains of
"conversation" is HISTORY, pulled and bounded, which is why section 4 is named
for it. This is a real departure from the current package, where a standing
Subscribe carries every live record.

**Work order the user set:** turn + detached work first (turn leading, since it
announces detached work and its frame shape is the one detached mirrors), then
the remaining sections.


### STAGE 4 CONVENTION: a BOUNDED stream's every frame is `oneof result { update | success | failure }`; standing streams have no terminal arm

**Settled ("i'm fine with flattened version, just for consistency's sake").**
Every frame of a stream that CONCLUDES carries a one-level oneof: an update, or
one of the two terminal arms. A conclusion is therefore a MESSAGE the producer
sends, never the stream merely stopping — so a stream that ends WITHOUT a
terminal frame is a transport failure and is read as one, never as work
concluding. This is `ReplayDone`'s existing discipline ("a replay that simply
stops streaming would be indistinguishable from one still in flight")
generalized to every bounded stream in the package.

**One level, not two.** The user sketched `result { update | done }` with
`done { success | failure }` and then chose the FLATTENED spelling for
CONSISTENCY with the stage-2 feed-row ruling (`oneof result { update |
success | error }`, "option b, rewrite approved"), noting the nested sketch was
only more edifying as a demonstration. The orchestrator raised the nesting as
defensible ONLY if `Done` carried fields common to both outcomes (a conclusion
instant, a final accounting stamp); consistency won over that possibility, and
a shared terminal fact is duplicated across the two arms if one appears —
exactly the duplication the feed rows already accept.

**SCOPE: bounded streams only.** A turn stream and a detached-work stream
conclude and carry the terminal arms. The session's STANDING conversation
stream does not conclude, and giving it a terminal arm would invent an ending
it does not have.

**Does not reopen the keepalive retraction.** A terminal frame is a real fact
the producer knows and states, which is the opposite of a keepalive — a ping
standing in for a fact nobody observed. The 3b layering (the party that can see
the silence reports it) is untouched.


### shared.proto DELETED; HeldOfferMergeDequeue gets its body — STAGE 3 (agentrepl.v1) COMPLETE

**What changed ("1 - yes, drop; 2 - okay let's handle").**
agentrepl/v1/shared.proto deleted — imported by nothing; its contents all
superseded (MergeStatus → the feed bubble; MergeDequeueOffer → the tray's
HeldOffer; HibernationDetail → HostHibernation; the Refusal* messages remain
reference material in git history for deriving error arms at the wave).
frontend.v1 HeldOfferMergeDequeue = { HeldOfferHeadline {text} } — the daemon
composes the sentence; the two answers are AnswerHeldOffer's arms, never
fields of the card.

**Stage 3 closes**: agentrepl.v1 is service.proto (27 rpcs, seven sections),
one endpoint_*.proto per rpc, and nothing else; workspace.v1 is the identity
leaf. Stages remaining per the settled sequence: 4 (shim.v1), 5 (store.v1),
6 (state.v1).

### 3c: WatchHostWorkspace lands; `host_surface_pending.proto` is DELETED — every agentrepl.v1 section complete

**What changed ("okay looks good", after three user restructurings).**
endpoint_watch_host_workspace.proto: request { workspace.v1.WorkspaceRef };
response wraps HostWorkspace whole. HostWorkspace = { oneof session { none |
existing }; naming }. HostSessionExisting hoists the shared HostSessionId
(the user's factoring) over oneof standing { live | terminal{rehydratable} |
hibernated{detail; parked|reviving} }. HostSessionLive carries generation,
shim_attached, oneof vendor_info { HostVendorClaude{session_id, config_dir} }
(the user's restructure of the bare vendor strings), HostBackfill (the
BackfillState enum converted: none|pending|done|failed{detail}), oneof
composer { open | merging | draining | restarting } and the
generation-scoped faults — the last two RELOCATED INTO the live arm at the
user's prompting (adjacent exclusivity: hibernated+open was representable
nonsense as a sibling; a fault window dies with its generation).
HostComposerGate/Blocked and the composed gate sentence DIE — the other
lifecycle arms are blocked by their own nature, and Emacs rendering a fixed
treatment per arm is ordinary oneof rendering. HostHibernation* carried
verbatim from HibernationDetail (since_ms; idle_cutoff{cutoff_ms} | forced |
cache_expired{elapsed_ms, ttl_ms}). HostWorkspaceNaming { optional slug,
optional title } replaces the bare slug/title strings the user called out.

**DELETED: host_surface_pending.proto, entirely** — every message placed or
ruled dead per the settled worksheet (WorkspaceState with its fence and SSM
snapshot fields, SessionView, RuntimeFault→HostFault's future arms,
BackfillState→HostBackfill, merge fields → the feed bubble / roster / tray).

**The flow, recorded because the user asked for it twice**: Emacs connects,
RegisterWorkspace(dir)→minted ref per known worktree; per OPEN workspace one
WatchHostWorkspace(ref) subscription (snapshot first, whole-replace on
change); closes cancel; a daemon restart drops streams and Emacs re-registers
and re-subscribes. The daemon never calls Emacs. The earlier global-stream
sketch (empty request, WorkspaceRef keying each entry) was REJECTED by the
user — workspace-dependent and workspace-independent channels must be
distinct types; the ref moved from the entries into the request, and no
daemon-level host stream exists until a daemon-level pushed fact needs one.

**Left dangling, surfaced**: agentrepl/v1/shared.proto is now imported by
NOTHING (its MergeStatus/MergeDequeueOffer/Refusal*/HibernationDetail all
superseded or carried); and frontend.v1's HeldOfferMergeDequeue body is
still empty-on-purpose awaiting its walk.

### `workspace.v1` — a NEW LEAF PACKAGE for workspace identity; the identities are daemon-minted echo tokens

**The user's rulings.** (1) A path is never an identity — "directories can be
represented in many ways" — so RegisterWorkspace PROVIDES the path (string
dir, any spelling) and the daemon MINTS the identifier, returned on success.
(2) WorkspaceRef = { id (sole supported identifier, opaque); dir (normalized
directory, NOT to be used as an identifier) }. (3) Workspace-specific shared
dependencies live in a LEAF PACKAGE — frontend.v1 must not import
agentrepl.v1 (confirmed it does not today; that direction was already
removed in stage 2 and stays removed).

**What changed.** NEW proto/src/workspace/v1/workspace.proto: WorkspaceRef
{id, dir} and RepositoryRef {id, dir} (same minting logic — a repo's
main-worktree path has the same spelling problem; clients get one from the
roster's repo sections). agentrepl/v1/workspace.proto DELETED; every
agentrepl.v1 endpoint repoints to workspace.v1 (qualified). RegisterWorkspace:
request { string dir }, success { workspace.v1.WorkspaceRef } — the echo-token
loop for workspace identity. frontend.v1 imports the leaf: RosterRowWorkspace,
RosterCurrentWorkspace, RosterRepoKey and FeedMergeQueueWorkspace now EMBED
the imported refs (identity join keys are imported, never respelled — closing
stage 2's "typed workspace identity" flag with answer (b), and retyping the
roster/queue wrappers that had carried bare dir/value strings).

**Consequences.** The enumerated leaf contents are ONLY WorkspaceRef and
RepositoryRef today; a later workspace-specific shared fact joins the
package rather than a boundary package. The daemon owns normalization; a
client that constructs an id (rather than echoing one) is typed as wrong in
intent though not mechanically — the id's opacity comment is the contract.

### ReportHostAction NEVER EXISTS — the daemon→host command loop is deleted ("yeah delete them")

**Reopens the host-section settlement's item 2 (the report-back fold).** The
old pair: HostActionCompleted (Emacs reporting on a daemon-DISPATCHED action,
by action_id, over the old push-stream inbox) and WorkspaceMaterialized
(Emacs reporting its local buffer/perspective bookkeeping done). Both die
with nothing in their place: the daemon no longer gives Emacs orders — Emacs
watches the streams and REACTS (a workspace appears on the roster → open its
buffers; closes → tear down), same as the webapp; and the daemon never waits
on Emacs's buffers, so "I finished setting up" has no listener (answers 3
and 4: the inbox was an abstraction leak of the old push stream, and the
report is a dead feature). If a real daemon→host ask surfaces at the wave,
it re-enters as its own designed verb, never a generic action envelope.

**The HOST section is therefore three RPCs**: RegisterWorkspace,
SelectWorkspace (landed), and WatchHost (next — the pending-file judgment).

### 3c: SelectWorkspace lands

**What changed ("looks good").** endpoint_select_workspace.proto: request
{ WorkspaceRef }; success {} (the roster stream carries the new `current`) |
error {} (arms derived: unregistered workspace). Idempotent — re-selecting
the current workspace is a success.

### 3c: RegisterWorkspace lands — HOST section opens

**What changed ("looks good").** endpoint_register_workspace.proto: request
is JUST { WorkspaceRef } — no name, parent, or branch; the daemon derives
everything from the dir (the settled "Emacs registers; the daemon tracks"
model). IDEMPOTENT BY DIR — re-registration after reconnect/daemon restart
is the normal path, one success answer. Error arms empty until derived.

### 3c: ClientLog lands — DAEMON ADMIN section complete

**What changed.** endpoint_client_log.proto: request { WorkspaceRef;
ClientLogRecord { oneof level { ClientLogLevelDebug|Info|Warn|Error (the
user's naming) }; operation; message; Struct context } }; response
{ success {} | error {} }.

**Untyped field, ACCEPTED as a cost (the exception, stated at the field).**
ClientLogRecord.context is a Struct: arbitrary per-call-site diagnostic
key/values whose whole purpose is to carry whatever the call site had in
hand — no schema can exist ahead of the sites; nothing routes on or renders
it; it is written verbatim to the daemon's on-disk log for a human debugger.
The purpose of the RPC itself, restated for the record: the webapp runs in
an xwidget whose JS console is invisible and unpersisted, so without this
relay a webapp malfunction leaves no evidence anywhere; Emacs never calls it.

### 3c: SessionHealth lands

**What changed ("looks good").** endpoint_session_health.proto: request
{ WorkspaceRef }; response result { success { healthy | unhealthy{ repeated
SessionFault } } | error {} }. SessionFault = DaemonFault's discipline
({detail} now, kind oneof added with derived arms at the wave, from the
session controller's real fault sites) but DELIBERATELY ITS OWN TYPE — two
fault vocabularies with different producers, not one shared one.

### 3c: DaemonHealth lands — typed fault classes + dynamic detail

**What changed ("okay").** endpoint_daemon_health.proto: DaemonHealthRequest
{}; response result { success { oneof health { healthy | unhealthy{ repeated
DaemonFault } } } | error {} }. UNHEALTHY IS AN ANSWER, never an error.

**The user's ruling on fault representation.** Typed fault arms, "with
supported string for dynamic error details as needed — general classes of
failures represented with messages (recursively) as far as readily doable,
but always supporting detailed dynamic information by way of strings."
DaemonFault = { string detail } + a kind oneof ADDED WITH its first derived
arms at the wave (empty oneofs are illegal; inventing arms would violate
derived-not-invented). The pending file's RuntimeFault (string component/
fault_type/impact/cause) is this message's ancestor and is judged INTO those
arms at the host stream's turn.

### 3c: UpdateMergeQueue lands

**What changed ("looks good").** endpoint_update_merge_queue.proto: request
oneof action { pause | resume | evict{WorkspaceRef} }; success {} | error {}
(arms derived: already paused, not paused, no such queued merge).
Consolidates PauseMergeQueue/ResumeMergeQueue/EvictMerge. Purely inbound —
the queue's visible state rides the merge bubbles' queue tabs and the
roster's status arms.

### 3c: UpdateShutdownSchedule lands — DAEMON ADMIN section opens

**What changed ("okay these look good").** endpoint_update_shutdown_schedule
.proto: request oneof action { schedule{at_ms} | cancel | now }; success {} |
error {} (arms derived: nothing scheduled to cancel, a newer schedule
stands). Consolidates ScheduleShutdown/CancelScheduledShutdown/Shutdown.

**Clarified for the record (the user asked what it is for and why elisp).**
No UX motivation exists — it is DEPLOY TOOLING's drain-and-exit control,
purely inbound; elisp is merely today's plumbing to reach it, and nothing
about the schedule is pushed outward on this verb. The user-visible
consequences ride surfaces already modeled: held prompts in the tray during
the drain, footer status. Kept in agentrepl.v1 (typed, one service) rather
than a side mechanism. Review discipline correction, also applied from here
on: an RPC is presented with BOTH its request and response, one RPC at a
time unless RPCs genuinely share messages.

### 3c: WatchFooter, WatchDaemonHolds, UpdateHeldPrompt, AnswerHeldOffer land — FOOTER and DAEMON-HOLD TRAY sections complete

**What changed ("makes sense", after the UX walk).** Four endpoint files +
four rpcs (19 total). WatchFooter / WatchDaemonHolds: { WorkspaceRef } →
stream <Rpc>Response wrapping the view whole. UpdateHeldPrompt:
{ WorkspaceRef; TurnId turn; oneof action { release | drop } } — the TurnId
is the echo-token loop closed: minted at submission
(SubmitPromptSuccess.turn), served inside the tray's HeldPrompt entry,
handed back unchanged; release = deliver NOW (interrupting the running turn
when that is what delivery takes), drop = discard (the composer takes the
text back); doing nothing is the normal path. AnswerHeldOffer: addressed by
OFFER KIND (a workspace has at most one offer of a kind standing — a token
would over-address); merge_dequeue { keep | release }. All success arms are
"done — the tray's new state arrives on its stream"; error arms empty until
derived. The echo-token skill convention merged as explanation-engine #7492.

### CONVENTION (3b addition): every rpc returns `<RpcName>Response`; CORRECTION: ten rpcs had silently missed the service block

**The user's ruling.** No rpc returns a foreign type directly — a stream of a
frontend view returns `stream <RpcName>Response` wrapping the view as its
single field, declared in that rpc's endpoint file. Landed by a sonnet
subagent for the three violations (WatchFeedResponse{row},
WatchWorkspaceRosterResponse{roster}, WatchTopbarResponse{topbar}); every
later endpoint follows it directly.

**CORRECTION, KEPT VISIBLE.** The subagent's report exposed that
service.proto held only FIVE rpcs: the orchestrator's earlier service-block
edits for the sidebar and topbar sections (WatchWorkspaceRoster,
CreateWorkspace, the six workspace verbs, WatchTopbar, SetModel) had
SILENTLY NO-OP'D — python str.replace anchored on text that was not in the
file (it assumed AnswerPermission closed the block; SubmitPrompt did), and
nothing asserted the replacement took. The endpoint files and design entries
were always right; only the service block lagged. Rebuilt whole with all 15
rpcs in section order, compiles. ROOT CAUSE: unasserted textual replaces on
a file whose shape had drifted; the fix discipline is asserting every
replace (as the feed.proto edits did) or rebuilding the block wholesale.

### 3c: WatchTopbar + SetModel land; `AgentModel` — the typed-echo-token pattern (stage-1 api.proto reopened additively)

**The user's pattern, stated and codified.** "We should have a message
encapsulating fields that are dynamically determined by the backend and
reused by the frontend in future requests — the frontend can't/shouldn't
know the supported values — wrapped so the reuse is obvious." Named the
TYPED ECHO TOKEN: the provider mints a wrapper message, embeds it in what it
serves, and the request field is the SAME type echoed back unchanged — the
round-trip becomes a schema fact, and a client that invents a value is typed
as wrong rather than merely told not to. Dispatched to the skill as a
one-shot subagent (worktree proto-echo-token).

**What changed ("your agent model protobuf suggestion looks good").**
conversation/v1/api.proto: new `AgentModel { name }`; `ModelOption.value`
(string) becomes `AgentModel model = 1` — the token inside each served
option. endpoint_watch_topbar.proto: WatchTopbarRequest { WorkspaceRef } →
stream frontend.v1.TopbarView. endpoint_set_model.proto: SetModelRequest
{ WorkspaceRef; conversation.v1.AgentModel model } → success {} | error {}
(arms derived at the wave: model not among served options, no session). The
TOPBAR service section is complete.

### 3c: the six remaining sidebar verbs land — the SIDEBAR section is complete

**What changed ("those all look simple, let's land them").** Six endpoint
files, each `{ WorkspaceRef } → { success {} | error {} }` with errors
derived later: OpenWorkspace, CloseWorkspace (the only teardown),
MergeWorkspace (success = ENQUEUED; the merge's life from there is the
feed's bubble; it targets the workspace's parent by definition),
HibernateWorkspace, ReviveWorkspace, and RestartWorkspace.

**RestartWorkspace, per the user's spec.** It bounces ONLY the workspace's
shim (rebuild if out of date + restart the process), with `bool force`:
false = GRACEFUL — wait until no turn is in flight and no async/background
tasks run, holding incoming prompts via the daemon hold (tray-visible)
meanwhile; true = FORCED — interrupt the shim (current turn + background
tasks), bounce immediately, and do NOT resume the agent afterwards —
continuing is the user's, with a subsequent prompt. (`force` is a bool, not
a two-arm oneof, consistent with the new skill convention's scope: no
adjacent data changes interpretation under it. The graceful hold implies a
FIFTH daemon-hold reason — restart pending — for the tray's hold oneof:
flagged for the tray shapes at the wave.)

**Consequences.** No MergeWorkspace parameters exist (no target override);
no RestartWorkspace webapp/daemon rebuild semantics — never the webapp,
never the daemon. The sidebar section's eight RPCs are all on the service.

### 3c: CreateWorkspace lands — the daemon names AND creates; the package file model settled (`workspace.proto` shared vocabulary)

**File model (the user's ruling).** agentrepl.v1 is exactly: service.proto
(all RPC signatures), endpoint_<rpc>.proto (that RPC's request/response), and
<shared-package>.proto files for what MORE THAN ONE endpoint needs. So
workspace_ref.proto is FOLDED into a new workspace.proto (WorkspaceRef +
RepositoryRef), and no repository_ref.proto exists.

**CreateWorkspace ("i agree with the protos").** THE DAEMON DOES THE
CREATING — the user: it should name the workspace "because it's the thing
actually creating the workspace worktree/branch/etc". It derives the slug
(from the initial prompt when present) → branch → worktree dir, runs the git
itself, registers the workspace; NO host materialization round-trip exists —
Emacs sees the workspace on the roster and opens its buffer. This supersedes
the orchestrator's host-report-back guess. "Name" was clarified as the
coupled branch name / worktree dir / display name, no abstract identifier;
the dir IS the identity and returns as CreateWorkspaceSuccess.workspace.
Request: RepositoryRef + optional UserSaid initial_prompt + optional
base_ref (both optionals per the new skill convention — presence, never
sentinels; skill PRs #7490 bool→oneof and #7491 optional-null both MERGED).
RepositoryRef is an ADDRESS like WorkspaceRef — base_ref stays a request
parameter, per adjacent-exclusivity, and ref types stay pure addresses.
Error arms empty until derived.

### 3c: WatchWorkspaceRoster lands — the one global stream

**What changed (the user approved the workspace-management RPC prototypes).**
`endpoint_watch_workspace_roster.proto`: WatchWorkspaceRosterRequest {} —
empty on purpose, the roster is global — and the stream's message is
frontend.v1.WorkspaceRoster itself, whole. The SIDEBAR service section opens.
The bool→oneof skill convention is PR "create-or-update-protobufs:
mode-selecting bool is a two-arm oneof" (explanation-engine #7490, queued).

### 3c: AnswerPermission lands; kind ④ reopened — the permission card gains a QUESTIONS body (AskUserQuestion); SubmitPrompt gains its idempotency key

**The user's probe that found the gap.** "I'm not seeing how multiple
selection, user-input, etc are being handled — this is the AskUserQuestion
feature, right? Do we need to research the SDK api?" Researched in the shim's
own contract rather than asserted: the SDK has ONE gate (canUseTool → the
shim's PermissionRequest/PermissionResponse round-trip, core.proto:1043, with
allow-with-edits via updated_input), and AskUserQuestion is a TOOL riding
that same gate — its input is questions[]{question, header, options{label,
description}, multiSelect}, its answer is an allow whose updated_input
carries the selections. The sketched verdict-only endpoint could not carry
answers; the drawn card could not draw a question.

**feed.proto (stage-2 reopen).** FeedPermission gains `oneof body { tool
{headline, arguments} | questions }`; FeedPermissionQuestions is the batch
(1–4); each FeedPermissionQuestion is text + header + `oneof options
{ single_select | multi_select }` — the user's shape: THE ARM IS THE
SELECTION MODE (radios vs checkboxes), replacing the orchestrator's bool,
with dedicated arm messages so a mode-specific prop later (a "pick at most
N") lands on its arm. The mode is PER QUESTION because the SDK puts
multiSelect on each question — one batch can mix a radio and a checkbox
question, so lifting it to the body was unrepresentable. Free-text "Other"
is always offered by the drawn card, never an option. FeedPermissionSuccess
gains the `answered_questions` arm: given answers, one per question, each
{header, chosen[], other_text}, drawn as the verdict line.

**endpoint_answer_permission.proto.** Request { WorkspaceRef; ToolCallId;
oneof answer { allow_once | allow_for_session | deny{reason} | answers } };
AnswerPermissionAnswers = one AnswerPermissionAnswer per question in batch
order {chosen[], other_text}; the daemon translates to
allow-with-updated_input. Body/answer mismatch is a refusal. Success is
"delivered" — the card's state change arrives on the feed stream. Error
empty until derived.

**SubmitPromptRequest** gains `idempotency_key = 2` (the 3b retrofit).

**Flagged, not modeled:** general allow-with-edits (editing a Bash command
before allowing) — the shim wire supports it; no UI exists today.

**Skill update dispatched** (one-shot subagent, worktree
proto-bool-mode-oneof): a boolean that selects the INTERPRETATION of
adjacent data is a two-arm oneof of dedicated (possibly identically-shaped)
arm messages, never an adjacent bool — genericized, per the user's
instruction.

### 3c: Interrupt lands — the folded stop verb; the pending file sheds its ack payload

**What changed ("okay").** `endpoint_interrupt.proto`: InterruptRequest
{ WorkspaceRef; oneof target { turn | detached(MessageId) } }; success
outcome { interrupted_turn | interrupted_detached{count} | nothing_running };
error empty until derived. rpc Interrupt on the service.

**Absorbed.** The pending file's DetachedCancelOutcome/DetachedAgentsCancelled
are DELETED (its shim.v1 import too): `nothing_running` moves from the old
"refused with ok=false" to a SUCCESS ANSWER per the domain-outcome rule — the
old comment's worry ("a stop that missed looks like one that worked") is
answered by the arm being distinct, not by mis-classing a quiet session as a
failure; `count` keeps its frontend-vocabulary rationale (shim task ids stay
on the shim wire); shim.v1's DetachedCancelUnsupported becomes a derived
ERROR arm at the wave.

### 3c: GetFeedPage lands — no cursor exists on the wire

**The user's ruling.** "Cursor should be totally unknown to frontend. It
should only be able to get the first page, and the next page, for the given
source." The DAEMON holds each container's walk position (one webview per
workspace = one reader per container); `first` resets the walk to the newest
page, `next` continues older, and a `next` with no walk standing is a
refusal, not an empty page.

**What changed ("okay").** `endpoint_get_feed_page.proto`:
GetFeedPageRequest { WorkspaceRef; oneof container { top_level |
parent(MessageId) }; oneof page { first | next } }; response
{ frontend.v1.FeedPage success | GetFeedPageError error } — TWO error layers
on purpose (response error = not served: unknown workspace/container,
next-with-no-walk; page error = served with a hole). Error arms empty until
derived. NO page-size parameter — the daemon picks. And in feed.proto,
FeedPageHasMore LOSES its cursor field — an empty arm, purely "older rows
exist".

### 3c: WatchFeed lands; `WorkspaceRef` declared

**What changed ("okay let's continue").** `workspace_ref.proto` — the 3b
shared identity, `WorkspaceRef { string dir }` (one shared type here, unlike
frontend.v1's per-element wrappers, because requests are called, not drawn).
`endpoint_watch_feed.proto` — `WatchFeedRequest { WorkspaceRef workspace }`;
the stream's message is `frontend.v1.FeedRow` ITSELF, whole (an upsert by id).
`rpc WatchFeed(WatchFeedRequest) returns (stream frontend.v1.FeedRow)`.

**Settled with it.** A refused open closes the stream at the transport (no
in-band error arm until a real need shows); NO resume token — a reconnect
re-opens and re-pulls pages, the stream is "now" never "since" (a resume
token would rebuild the fence machinery stage 2 deleted).

### 3b REOPENED: the keepalive convention is RETRACTED — no keepalive frames anywhere

**The user's challenge, at WatchFeed's 3c turn.** An in-band keepalive arm
"bleeds implementation details"; the shim already heartbeats from the SDK, so
source-of-truth liveness should manifest from the actual source of truth; and
keepalive only fires when nothing is happening, when staleness is not a
user-facing fact.

**Retracted, with the layering that replaces it (each layer already modeled):**

1. Source-of-truth silence (SDK/shim quiet mid-turn): the DAEMON observes it —
   FailureShimDegraded (window-shaped) + footer status arms. Real facts from
   the party that can see the silence, not pings.
2. Daemon↔client pipe death: the connection fails; the client library sees it
   and unary calls fail loudly. No frame detects a dead pipe better.
3. A wedged publisher behind a healthy connection is a DAEMON-INTERNAL fault:
   the daemon's own watchdog surfaces it (RuntimeFault / DaemonHealth), not N
   clients timing frame cadence per stream.

ROOT CAUSE of the error: the orchestrator carried the stage-2 "keepalive
convention" forward from the HeartbeatView deletion without re-asking WHO
should detect each silence; putting the detector in every client was the same
client-derivation failure the redesign removes elsewhere.

**Consequences.** Stream messages carry views only: WatchFeed's stream message
is plain FeedRow (the oneof wrapper returns only if a real second arm — e.g.
an in-band error — is ever agreed). No stream defined at 3c gets a keepalive
arm. The stage-2 footer entry's "liveness is the keepalive" sentence is
superseded by this layering.

### STAGE 3b SETTLED: the cross-endpoint conventions

**Settled ("the conventions are sound yes" for 1–2; "kay proceed" on the
orchestrator's recommendations for 3–6).**

1. RESPONSE SPELLING: every response is `oneof result { <Method>Success
   success = 1; <Method>Error error = 2; }` — the standing convention, the
   package's one spelling (already in force on SubmitPrompt).
2. ERROR DERIVATION: every `<Method>Error`'s arms are DERIVED from the
   daemon's real refusal sites, never invented; entry-less failures name
   failure.proto evidence types.
3. KEEPALIVE: an EXPLICIT IN-BAND FRAME, not a transport ping — every
   component stream's message is `oneof { <view> | keepalive {} }` at cadence
   C; a client hearing nothing for T marks the component stale. An h2 ping
   proves the connection lives, not that THIS component's resolver lives;
   the guarded failure is a wedged publisher, which only an in-band frame
   disproves. C and T are implementation-wave constants, not schema.
4. WORKSPACE ADDRESSING: a shared typed identity `WorkspaceRef { string dir }`
   in agentrepl.v1, a field on every per-workspace request and stream-open.
   Requests are CALLED, not drawn, so a shared identity type is right here
   (the stage-2 flag answered); the URL-param approach left addressing
   outside the schema.
5. IDEMPOTENCY: a client-minted `idempotency_key` on SubmitPrompt ONLY — the
   one verb where a duplicate is costly and undetectable (a retried
   answer/interrupt/close is naturally idempotent by its target). The daemon
   refuses duplicates by key — the guarantee that replaced
   newQueueEntryID()/promptreceipt's request-id refusal.
6. OLD HEADER RESIDUE: "no paint attestation" carries forward as a
   service-level comment (responses never claim anything was rendered); the
   request_id/client_id envelope DIES (Connect's unary response IS the
   correlation; no client identity is load-bearing — ClientLog names its
   caller in-band); protojson note carries forward as fact (Connect serves
   binary or JSON per client; elisp uses JSON).

**Consequences.** Every stream message defined at 3c is a two-arm oneof
(view | keepalive). SubmitPromptRequest gains `idempotency_key` at its 3c
turn. WorkspaceRef is declared once in agentrepl.v1 when the first
per-workspace endpoint lands at 3c.

### Stage 3, HOST and DAEMON-ADMIN sections settled — the section walk is COMPLETE (seven sections)

**Settled ("sounds fine, let's continue"), on the orchestrator's stated
recommendations, each confirmable below.**

1. The former Host section SPLITS: **Host** (RegisterWorkspace,
   SelectWorkspace, WatchHost, ReportHostAction) and **Daemon admin**
   (UpdateShutdownSchedule, UpdateMergeQueue, DaemonHealth, SessionHealth,
   ClientLog) — the admin verbs are not host-natured; Emacs is just today's
   caller. Sections: feed, sidebar, topbar, footer, daemon-hold tray, host,
   daemon admin.
2. Consolidations per the established pattern: UpdateShutdownSchedule
   { schedule | cancel | now }; UpdateMergeQueue { pause | resume |
   evict{workspace} }; WorkspaceMaterialized + HostActionCompleted fold into
   ReportHostAction { oneof action } with arms derived at 3c from what Emacs
   actually reports.
3. COMPOSER GATING IS A RESOLVED ELEMENT on the host stream ("open" /
   "blocked: merging" / "blocked: reviving" as arms), never raw bools
   (merge_lease_held, hibernated) Emacs maps — the figma→idl gray-zone
   ruling: what Emacs DRAWS arrives resolved; what it coordinates with
   (session_id, generation, backfill, lifecycle booleans it does not draw)
   stays coordination residue, exempt.
4. Facts with no Emacs need LEAVE the host surface (each already has its
   drawn home): model/model_options, total_tokens/total_cost_usd/
   context_window, permission_mode/pending_permissions, merge_status,
   merged_at_ms. Also dead: fence (stage 2 deleted fencing), the
   merge_dequeue_offer (the tray's HeldOffer).
5. BackfillState (the never-blue signal Emacs reads) converts enum → oneof;
   `failed` carries typed evidence, shaped at 3c. Its known limitation (a
   sidecar read error that is not a malformed line manifests as PENDING
   forever) is carried in the pending file's comment and stays owed.
6. DELETE-WITHOUT-CLOSE IS DEAD: CloseWorkspace is the only teardown; no
   DeleteSession returns.
7. DetachedCancelOutcome/DetachedAgentsCancelled are not stream output: they
   become the folded Interrupt{detached}'s success arms at 3c.

**The full fact-by-fact enumeration of the host stream's contents** (what is
carried vs re-homed vs deleted) was walked with the user in conversation and
is the 3c worksheet for WatchHost; the pending file is deleted when 3c has
placed or deleted every message in it.

### COMMAND PANELS section dissolves into the feed's SubmitPrompt; the endpoint lands (the first agentrepl.v1 RPC)

**The user's model.** "The client sends normal user requests, and the daemon
MIGHT respond with a 'this is a programmatically handled command' message —
the webapp shouldn't know what's programmatically handled; that's up to the
daemon, transparently." So RunCommandPanel never exists, the recognition
table lives only in the daemon, and the SECTION LIST DROPS TO SIX: feed,
sidebar, topbar, footer, daemon-hold tray, host.

**What landed ("looks good").** `endpoint_submit_prompt.proto` + the first
rpc on the empty service. SubmitPromptRequest { UserSaid }; response
result { success | error }; success outcome { turn { TurnId } | command_panel
{ oneof panel { frontend.v1.StatusPanelView status } } } — both ANSWERS per
the domain-outcome rule; a HELD prompt is a `turn` success (the tray shows
the hold, not an error). agentrepl.v1 composing frontend.v1 is the
composition rule in force. The error arm set is DELIBERATELY EMPTY until this
endpoint's 3c turn (derived from the daemon's real refusal sites, spelled per
3b). Deliberately absent: a client idempotency key (3b question), workspace
(the connection's), any echo of the prompt (the feed pushes the row).

**Consequences.** A new panel command (/context, /login) is a new panel arm
deployed daemon-side; old clients fail to match loudly instead of mis-sending
it as a prompt. StatusPanelView stays a frontend.v1 component; only its
transport is settled (unary, in the submit response — no stream, no fetch).

### Stage 3, DAEMON-HOLD TRAY section: three RPCs

**Settled ("looks good").**

1. WatchDaemonHolds — per-workspace DaemonHoldTray stream, whole-replaced.
2. UpdateHeldPrompt — ONE verb per the user's consolidation ("all held prompt
   updates along one channel"), the AnswerPermission precedent: `{ TurnId;
   oneof action { release (deliver now — today's "force") | drop (discard) } }`.
   A later action is a new arm; the refusal vocabulary (no such hold, already
   delivered) is declared once.
3. AnswerHeldOffer — its own verb, NOT folded into UpdateHeldPrompt: an offer
   is a different item kind with answers of its own shape (merge-dequeue
   keep/release today), and one RPC whose arms half-apply per item kind would
   reintroduce adjacent-exclusivity.

**The old table's third queue verb ("accept") is DEAD: accept is the default.**
A held prompt delivers itself when its hold clears; waiting requires no verb.

### Stage 3, FOOTER section: one RPC

**Settled ("sounds good").** WatchFooter — per-workspace stream of FooterView,
whole-replaced. Nothing else: the strip's states are daemon-resolved, the
tokens cell rides the view, and an expanded row's click is client-side
navigation to a feed bubble by MessageId — no command exists in this section.

### Stage 3, TOPBAR section: two RPCs; the breakdown menu nests INTO TopbarView

**Settled.** WatchTopbar — per-workspace stream of TopbarView, WHOLE; and
SetModel. Nothing else.

**The walk that got there, in the user's terms.** The orchestrator proposed
fetch-on-open for the breakdown menu; the user: "shouldn't token updates be
streamed? I don't see a pull architecture there being correct" — right, a
token figure changes while you look at it. The orchestrator then proposed a
separate WatchTokenBreakdown stream; the user: just WatchTopbar, tokens along
the same channel. The orchestrator then proposed a two-arm partial-replace
oneof (topbar | token_breakdown); the user: "the token breakdown is PART of
topbar — one level deeper" — so TopbarView gains `token_breakdown = 6` and
the stream is the plain view, whole. The partial-replace oneof is RETRACTED
as a delta creeping back in; the topbar is a few hundred bytes and whole-view
replace is the convention. Confirmed semantics: no oneofs among the view's
elements (none are mutually exclusive — a push can carry a new warning AND
fresher tokens, because a push is the whole topbar as it now stands, never an
event naming what changed); the breakdown is ALWAYS POPULATED, like a folded
section still carrying its rows, so opening the menu needs no round-trip.

**Presence ruling (the user asked about `optional` on warnings).** Message
fields carry presence natively; the convention is: EVERY element of a view is
always set, and "nothing to show" is expressed INSIDE the element (an empty
warnings list is the daemon saying nothing is wrong; the selector's unset
`selected` is the documented no-selection). An optional strip would spell "no
warnings" two ways. An unset element is a malformed frame, not a state.

**Consequences.** The TokenBreakdown* messages keep their names (the menu is
its own family). The daemon's topbar resolver owns session accounting
composition on every push.

### Stage 3, SIDEBAR section: the RPC inventory — nine RPCs; roster UI prefs go webview-local (stage-2 sidebar shape reopened by name)

**Settled.** Names and purposes only; shapes are 3c:

1. WatchWorkspaceRoster — the GLOBAL roster stream (the one stream with no
   workspace). Renamed from WatchRoster by the user.
2. CreateWorkspace, 3. OpenWorkspace, 4. CloseWorkspace, 5. MergeWorkspace,
   6. HibernateWorkspace — one verb each; EACH IS ONE RPC SHAPE FOR BOTH
   CALLERS (the webapp's roster clicks AND Emacs — Emacs speaks the same
   messages as protojson over its transport; Connect serves JSON natively, so
   one schema, two codecs). The user's instruction.
7. ReviveWorkspace, 8. RestartWorkspace — renamed from *Session by the user:
   the workspace is the address; one live session per workspace is the rule.
9. (none) — SetWorkspaceRosterView NEVER EXISTS, see below.

**Roster UI preferences: WEBVIEW-LOCAL (the user's selection, recommended).**
The stage-2 sidebar shape is REOPENED BY NAME and re-landed: grouping mode,
section folds and the nav cursor leave WorkspaceRoster. Folds and cursor are
per-client by nature (a shared fold would fold every webview; the cursor is
where YOUR keyboard is, and daemon-holding it makes every keystroke a
round-trip echoed to all webviews). Consequence of local grouping: the roster
carries BOTH groupings fully resolved (`repository` and `task` as siblings,
no longer a oneof) and the client draws the pane its local preference picks —
a selection between resolved views, like folding, never a derivation.
DELETED: RosterNavCursor, RosterFold, WorkspaceRoster.nav, the view oneof;
headers lose their fold element (task header renumbers).

**Flagged, still open for this section's 3c turn:** whether
delete-session-without-close is real (the old CreateSession/DeleteSession
pair), or CloseWorkspace is the only teardown.

### Stage 3, FEED section: the RPC inventory — five RPCs

**Settled ("okay i'm sold").** Names and purposes only; shapes are 3c:

1. WatchFeed — ONE server stream of FeedRow upserts per workspace, top-level
   and nested rows alike; the client routes by `parent`. One stream, not one
   per container: nesting is data, not topology, and reconnect is one re-open.
2. GetFeedPage — unary; a FeedPage for a container (top-level, a bubble, a
   merge phase) from a cursor. Replaces FirstPage/NextPage/Resync.
3. SubmitPrompt — a UserSaid; returns the daemon-minted TurnId or a typed
   refusal.
4. Interrupt — "stop that", with a TARGET ONEOF: the running turn, or a
   detached bubble by MessageId. The user's fold: CancelDetachedWork "is
   essentially an interrupt" — one user intent, one verb, the difference
   lives in the target arm, the refusal vocabulary is shared. The old
   Interrupt's second job (the merge-dequeue trigger) is already dead — that
   moved to the held-offer answer in stage 2.
5. AnswerPermission — allow once / allow for session / deny, by ToolCallId.

**Consequences.** CancelDetachedAgents does not return; the daemon routes the
target arm to the vendor query interrupt or the task stop respectively.

### STAGE 3a SETTLED: `agentrepl.v1`'s seven sections, the walk order

**Settled ("looks good, let's settle").** The service is organized by
COMPONENT, not by mechanism — each drawn component owns its stream, its pulls
and its commands together (the figma→idl file rule applied to the API), plus
two non-drawn sections. The RPC names below are SCOPING ONLY; each section's
actual RPC set is settled one RPC at a time at its turn:

1. Feed — stream, paging, prompt/interrupt/permission/cancel-detached.
2. Sidebar — roster stream, workspace-lifecycle clicks, view preferences.
3. Topbar — stream, model selector, session token-breakdown delivery.
4. Footer — stream.
5. Daemon-hold tray — stream, held-prompt/offer actions.
6. Command panels — programmatically-rendered slash commands (/status today;
   /context, /login later — the section is named for the CLASS, so a later
   command is a new panel kind, not a new section). Generalized from the
   orchestrator's too-specific "status panel".
7. Host — register/select workspace, report-backs, host stream, shutdown
   scheduling, health, merge-queue control, client log relay (that one's
   caller is the webapp, noted; Host = the non-drawn operational surface).

**The transport reasoning re-verified with the user on the way (Connect
stands, not reopened).** The user probed whether commands should ride the
feed's connection ("one connection for feed, along which commands are sent
and updates received") and whether Connect forgoes bidirectional RPC. Answers
recorded: browser fetch cannot do gRPC bidi (no duplex request in WebKit, no
trailers, no half-close — the page never owns the connection); the
"protobuf-powered WebSocket" is what agent-repl HAS today, and its cost is
the hand-rolled correlation layer (requestId, pending-ack map, ack-vs-push
races) that Connect deletes by making commands ordinary unary requests; all
endpoints multiplex over the ONE HTTP/2 connection, so one-connection is true
at the socket level and endpoint-per-component at the API level. The user:
"ah got it, that makes a lot of sense."

**Dropped from the old table at this level** (each re-judged, not silently):
PublishWorkspaceRoster (Emacs no longer authors the roster);
FirstPage/NextPage/Resync (→ feed paging + streams); CreateSession/
DeleteSession (subsumed by workspace open/close — whether delete-without-
close is real is FLAGGED for the sidebar section's turn).

### `token_breakdown.proto` folds into `topbar.proto`

**What changed.** The file is deleted; `TokenBreakdownView`, `TokenBreakdownSection`,
`TokenBreakdownHeading`, `TokenBreakdownRow` move verbatim into `topbar.proto`
under a banner. The banner states the uncoupling: the breakdown is the
SESSION's accounting, opened by the topbar; the footer's turn-level tokens are
a different fact and a different component, never a shared type.

**Why, in the user's terms.** "Token_breakdown should be in topbar" — the menu
is the topbar's subcomponent, so its messages live in the topbar's component
file; a separate file wrongly presented it as a sibling component. The
orchestrator's suggestion to re-home it under the footer is RETRACTED: the
topbar's token figure is the SESSION's, the footer's is the TURN's — different
values, architecturally uncoupled (checked: TokenBreakdownView's only consumer
is the topbar menu; the footer already has its own FooterTokens* messages).

**Consequences.** Stage-3 sections: no token-breakdown section; its delivery
(pushed vs fetched on open) is settled in the TOPBAR section's turn. If the
footer ever grows a turn-breakdown menu it gets footer.proto-family messages.

### `feed.proto`: `FeedApiFailure` folds into `FeedAgent.error` — SIX row kinds

**What changed.** `FeedApiFailure*` deleted; `FeedRow` is user, agent,
detached, permission, context_cut, merge (renumbered 4–9).
`FeedAgentError.reason` gains `api_request_failed { FeedAgentApiFailureMessage
{text}; FeedAgentApiFailureWhen {at_ms} }` — the vendor's recorded
`conversation.v1.ApiRequestFailed`, drawn as the failure of the response it
refused. When the vendor refused before a single token, the row is CREATED BY
the failure: its id is the `ApiRequestFailed` record's, `partial` is empty,
the head still names the author.

**Why, in the user's terms.** "FeedApiFailure should be folded into the agent
oneof, yes" — closing the flag from the previous entry. An API failure IS the
response failing; a separate row drew one fact as two kinds.

**Consequences.** The daemon's feed resolver keys a refused-before-first-token
response's row on the `ApiRequestFailed` record id, and on the `AgentSaid`'s
id otherwise (the two never coexist for one response). No row kind is
error-only any more; the one-arm kinds are `FeedUser {success}` and
`FeedContextCut {success}`.

### `feed.proto`: EVERY row kind carries `oneof result { update | success | error }`; kinds ⑤ `FeedFailure` and ⑨ `FeedPreview` are DELETED; `FailureKind` shrinks to the entry-less residue

**Why, in the user's terms — the conversation, in order.**

- Asked what the live preview IS in the UI (the type-out of the agent's
  response before it settles, at the tail, same id as the settled row), the
  user: "shouldn't that be WITHIN the streamable messages themselves, rather
  than adjacent them?" — i.e. a state arm on `FeedAgent`, not a ninth kind.
  Agreed: one drawn box in two states, and it surfaced an on-disk
  adjacent-exclusivity defect — the usage stamp sat on the head but means
  nothing while arriving.
- The user then generalized: `update` and `success` over `arriving`/
  `settled`, PLUS an `error` arm, and "all feed arms should have their own
  error representation represented in internal arms" — `oneof result {
  update (if applicable) | success | error }` on every kind. Rulings: (1)
  flatten to ONE level everywhere (option b) rather than keep `state →
  outcome` — "rewrite approved"; (2) kinds that cannot update or fail carry a
  ONE-ARM oneof; (3) `FeedFailure` is not needed "unless the error
  specifically and inherently does not correlate to any feed entry";
  everything else is that entry's own error.

**What changed.**

- `FeedRow` is SEVEN kinds: user, agent, detached, permission, api_failure,
  context_cut, merge (renumbered 4–10). `FeedFailure` (+ headline/detail/
  evidence/lifecycle) and `FeedPreview` deleted.
- `FeedAgent { head{author}; result { update{prose-so-far blocks: text |
  thinking} | success{usage stamp; blocks; stop notice} | error{partial
  blocks; headline; oneof reason — the failure.proto evidence messages
  imported whole: query_termination, vendor_max_turns, vendor_max_budget,
  vendor_execution_error, vendor_turn_failed, vendor_network_down,
  turn_undriven, keep_alive_window_unclosed/_inverted } } }`. The row exists
  from the first `ContentArriving` fragment under its future `AgentSaid` id.
- `FeedTool.result { update | success{ returned{blocks} | detached{bubble} }
  | error{ failed{blocks} | denied{reason} } }`.
- `FeedDetached.result { update | success{ended_at_ms, summary, exit} |
  error{ended_at_ms, exit, reason { failed | cancelled | lost }} }`; the head
  keeps only constant props; steps → `update|success|error`.
- `FeedPermission.result { update (open) | success{ allowed_once |
  allowed_for_session | denied; at_ms } | error{ abandoned; at_ms } }` — a
  DENIAL IS AN ANSWER (domain outcome, not a failure).
- `FeedApiFailure.result { error{headline, message, when} }` — one arm, and it
  is `error`: the row IS a failure.
- `FeedContextCut.result { success{tokens; cut{cleared|compacted}} }` — one
  arm; `compacted` gains a `cold_read` NOTICE (`FailureCompactionColdRead`
  imported) — a compaction that read cold still compacted.
- `FeedMerge.result { update | success{ended_at_ms, commit} |
  error{ended_at_ms, reason { failed | abandoned }} }`; each
  `FeedMergePhase.result { update | success{ended_at_ms, summary} |
  error{ended_at_ms, summary} }`.
- `FeedPage.result { success{rows, edge, breadcrumbs} | error{headline;
  history_replay_truncated} }` — a page that could not be completed is the
  PAGE's error, not a row.
- `failure.proto`: `FailureKind` shrinks 28 → 17 arms (renumbered): machinery
  session/shim/internal residue + client-local. The entry-correlated arms
  (query_termination, turn_undriven, keep_alive_window_*,
  history_replay_truncated, compaction_cold_read, the five vendor arms) leave
  the oneof; their EVIDENCE MESSAGES stay, imported by feed.proto's error
  arms. Header rewritten to say so. `host_surface_pending.proto`'s
  `SessionView.death` repoints to `FailureKind` (holding file, stage 3).

**Two corrections to what the orchestrator said in the conversation, kept
visible.** (a) It first mapped `history_replay_truncated` and
`compaction_cold_read` to `FeedContextCut.error`; on writing them, the first
is a PAGE failure (the re-pull, not a cut) and the second is a cost notice on
a compaction that happened, so they landed as `FeedPage.error` and a
`cold_read` notice on `compacted`. (b) `shim_store_write_rejected` was listed
as entry-correlated ("the entry being written"); the daemon has no row for a
write that never landed, so it stays in `FailureKind`.

**Consequences.**

- The webapp's separate `BubbleTyping` line under a bubble
  (`async-render.ts:336`) goes away — a subagent's streaming is a nested
  `FeedAgent` in `update`; `smooth.ts` paces the reveal off the row's update.
- A stream that dies can no longer leave a cursor blinking: the daemon
  pushes the row's `error` arm under the same id.
- Every client renderer switches on `result` the same way for every kind and
  for the page; the daemon's row resolvers emit the terminal arm with
  `ended_at_ms` inside it.
- `FeedApiFailure` stays a row (the vendor may record an API failure with no
  `AgentSaid` at all); whether it should instead be the in-flight
  `FeedAgent.error` when one exists is FLAGGED, not decided.
- Failures in `FailureKind`'s residue are drawn by footer/topbar/gate state
  and named by stage-3 error responses; nothing draws them as rows.

### `feed.proto` kind ⑧: `FeedMerge` — a tabbed phase bubble, the queue as its first tab

**The drawing agreed with the user** (in the kind's comment): a merge bubble
whose head is the branch line + clock + live/settled, and whose body is a TAB
STRIP — `queue ✓ │ rebase ✓ │ conflicts ✓ 3 │ tests ● 8/12` — with the
selected tab's nested rows beneath.

**What changed.** `FeedMerge { FeedMergeHead head; repeated FeedMergePhase
phases }`. `FeedMergeHead { glyph; label; runtime; oneof state { live |
settled { ended_at_ms; oneof outcome { merged{commit} | failed{summary} |
abandoned } } }; fold }`. `FeedMergePhase { FeedMergePhaseLabel label; oneof
state { live | settled { ended_at_ms; oneof outcome { succeeded{summary} |
failed{summary} } } }; oneof kind { queue | rebasing{base} |
resolving{paths, current_path} | building{detail} | testing{suite, progress}
| landing } }` — every working kind carries a `FeedMergePhaseRowCount`; the
queue kind carries `FeedMergeQueue { repeated ahead; current; repeated
behind }` of `FeedMergeQueueEntry { workspace; label; oneof status {
merging { oneof phase — the same FeedMergePhase* messages } | waiting } }`.

**Why, in the user's terms — the conversation that shaped it, in order.**

- The orchestrator first proposed a merge row modeled like `FeedDetached`
  (head with `queued`/`running`/`settled`, nested rows). The user: "we haven't
  actually decided on the merge UI" — waiting in the queue and merging at
  the head are two different representations. The orchestrator then argued
  the WAITING half was live state, not conversation, and belonged in a
  tray; the user DISAGREED: "the waiting half is part of the conversation.
  it's analogous to the other async bubbles (when running async bash you're
  'waiting' for the output, which is populated in the bubble)". One bubble
  for both. RETRACTED and recorded: the tray argument was wrong on the
  bubble analogy — a bubble's early life is exactly "waiting".
- `merging` "should most certainly not carry nothing" — conflict resolution
  is agent emission under the hood, so the bubble "turns into a sort of
  agent bubble". First answer: a phase oneof on the head. The user's
  amendment: the phases are SEPARATE BUBBLES coalesced under TABS.
- A repeat pass (tests break, resolve again): option (b) — a SECOND tab, "no
  special allowance there; visually you want it apparent there's been two
  rounds of something that should normally be one." Consequence accepted:
  phases are APPEND-ONLY and each is MONOTONIC (live once, settled once);
  the tab label carries the round ("conflicts (2)"), resolved by the daemon.
- `FeedMergePhase` is only for phases the run ACTUALLY ENTERED — a tab
  appears because work began; there is no pending tab.
- The user: "a queued merge should be just another phase — another Merge
  bubble, and therefore typically the first tab." This dissolved the
  `ahead`-vs-`phases` body oneof entirely; body = tabs.
- The queue tab's content is a SNAPSHOT replaced whole on every publish (the
  user: "every new update overwrites the content already existing"), like a
  shell spool; the daemon republishes on any queue change AND on any phase
  change at the front, so a waiting user sees the front's progress. The
  user's shape: the same message reaches every workspace in the queue; the
  head workspace's status is `merging` and it is SHOWN NOTHING about the
  queue ("it doesn't need to, it's merging"). The queue tab does not keep
  updating once we reach the front — it settles and the phase tabs take over.
- The user proposed a four-field `ws_merging / ws_ahead / ws_current /
  ws_behind` shape; the orchestrator MISREAD it as double-naming the head
  and objected — the objection was wrong (the user's status oneof made
  `merging` and placement exclusive) and was withdrawn. The landed shape
  keeps the user's structural position (`ahead`/`current`/`behind`, so
  "you are here" is never derived by comparing ids) with one entry type and
  the front's status as an arm on its entry.
- The tab BADGE (asked by the user as a thing to nail down before wrapping
  feed): a tab draws from exactly two fields — its label and its state arm,
  which picks the treatment (`live` / `settled{succeeded|failed}`); the
  badge's detail is the kind arm's resolved evidence. No enum, no client
  mapping.

**Consequences.**

- Nested rows parent to the PHASE, not to the merge, so "which pass produced
  this fix" is structural. The `landing` phase owns the merge's own narration
  (initiating prompt, closing message), so the body has no loose rows.
- The mirrored front-workspace phase in a queue entry imports the real
  `FeedMergePhase*` messages — one vocabulary for tab badge and queue row.
- Deliberately NOT reused: `FeedDetached*`. A merge is not detached work
  (daemon-orchestrated, has a `queued` life the vendor has no arm for), and
  two families = two drawn components.
- Stage 3 owes: the merge-queue push cadence (on every front-phase change)
  and `FeedMergeQueueWorkspace` as a jump target across workspaces.

### `feed.proto` kind ⑦: `FeedContextCut` — the divider, with the token delta modeled

**What changed.** `FeedContextCut { FeedContextCutLabel {text};
FeedContextCutTokens {before_text, after_text}; oneof cut { cleared {} |
compacted { FeedContextCutSummary {markdown}; FeedContextCutFold } } }`.

**Why, in the user's terms.** The first sketch composed the sizes into the
divider's line and had `cleared` empty; the user: "there's tokens before +
after in the conversation equivalents, which should be modeled" — so the
delta is its own element, a sibling of the cut oneof because EVERY cut has
one (a clear reloads system prompt/skills/memory, so after is small, not
zero). And the compacted comment was wrong: "it's not a summary of what was
discarded, it's a compression of the previous conversation" — reworded.

**Consequences.** Both sides are daemon-formatted strings; the client does no
arithmetic and no unit rounding. The summary is markdown the daemon flattens
from the record's `AgentContent` (prose only — a compaction summary has no
tool calls).

### `feed.proto` kind ⑥: `FeedApiFailure`

**What changed.** `FeedApiFailure { FeedApiFailureHeadline {text, tone};
FeedApiFailureMessage {text}; FeedApiFailureWhen {at_ms} }` — the recorded
`conversation.v1.ApiRequestFailed` drawn as a terminal row; the daemon
composes sentence and color from the record's kind. "Okay."

### `feed.proto` kind ⑤: `FeedFailure`; `FeedDetachedRuntime.ended_at_ms` moves into `Settled`

**What changed.** `FeedFailure { FeedFailureHeadline {text, tone};
FeedFailureDetail {text} (unset when none); FeedFailureEvidence
{ FailureKind kind }; oneof lifecycle { open | resolved{at_ms} | terminal } }`.
DELETED: `FailureCardView`, `FailureCardOpen/Resolved/Terminal`. The holding
file's `SessionView.death` now names `frontend.v1.FeedFailure`.
`FeedDetachedRuntime` loses `ended_at_ms`; it lives on `FeedDetachedSettled`
— it means nothing while live (adjacent-exclusivity, caught in the audit the
user asked for).

**Why, in the user's terms.** "Yes" — with two instructions: no serialized-
shape comments at line ends in the protos (checked: none exist in any landed
file — that style was only in the orchestrator's chat sketches, and it stops
there too; every field keeps a description above it); and an audit of the
feed kinds so far for (a) mutual exclusivity modeled as oneofs and (b) real
mapping to UI components — reported in the conversation; the one defect
found is the `ended_at_ms` fix above.

**Consequences.** `FailureKind` is embedded as the drawn evidence — the one
place the shared vocabulary type rightly sits inside a frontend element,
because it IS what is drawn and `agentrepl.v1` names the same arms.

### `feed.proto`: kind suffixes dropped (`FeedUser`, `FeedAgent`, `FeedDetached`, `FeedTool`, …); kind ④ `FeedPermission`

**Naming ruling.** The user asked what a "card" and a "bubble" ARE at the UI
level. Answer: chrome patterns the orchestrator named (line / boxed panel /
collapsible container) — real as CSS, not schema facts, and already
misapplied once (`FeedApiFailureRow` would draw with the failure card's
chrome). Dropped: every top-level kind is the family name (`FeedUser`,
`FeedAgent`, `FeedDetached`, `FeedPermission`, `FeedFailure`,
`FeedApiFailure`, `FeedContextCut`, `FeedMerge`, `FeedPreview`; `FeedToolCard`
→ `FeedTool`), children already family-prefixed. If two kinds ever share a
DRAWN frame element with props, it becomes an element message they embed —
schema when there is a box, never a suffix.

**Why some kinds have headline/arguments and others not** (the user's
question): the differentiator is WHAT the element is about — `FeedTool`,
`FeedPermission` and (reduced) `FeedDetached`'s head draw a TOOL CALL, so
each has its own headline/arguments wrappers filled by the daemon from the
same origin (per-component copies, not a shared type; a `FeedCard` union
was considered and refused — the tool card is nested in the agent row, not
a top-level row, and an intermediate node must be a drawn box).

**What changed.** `FeedPermission { ToolCallId call; FeedPermissionHeadline;
FeedPermissionArguments {lines}; oneof state { open {} | answered { oneof
answer { allowed_once | allowed_for_session | denied{reason} | abandoned };
at_ms } } }`. Buttons are `agentrepl.v1` requests (stage 3).

### `feed.proto` kind ③: `FeedDetachedBubble`; child naming settled as FAMILY prefix

**Naming (the user's ruling, option b).** Children of a row kind take the
FAMILY prefix, not the full parent name: `FeedUserAuthor`, `FeedAgentHead`,
`FeedToolHeadline`, `FeedDetachedHead` — the kind suffix (`Row`/`Card`/
`Bubble`) appears only on the top-level kind message. `FeedToolCard*`
children renamed to `FeedTool*` accordingly. "Bubble" is this codebase's word
for a collapsible container of nested rows (the user's own phrasing; the
webapp's `async-bubble.ts`); Row = a line-ish item, Card = a boxed item
without nested rows.

**What changed.** `FeedDetachedBubble { FeedDetachedHead head; oneof body {
agent {row count} | shell {spool} | workflow {steps} | skill {doc, row
count} | unmodeled {row count} } }`; `FeedDetachedHead { glyph; label;
runtime {started, optional ended}; oneof state { live | settled { outcome
succeeded|failed|cancelled|lost{how}; exit } }; fold }`. Nested rows are
ordinary `FeedRow`s with `parent` = the bubble, paged via the same
page/breadcrumbs. DELETED old bodies: `DetachedWork`, `DetachedWorkLiveness`
/`Live`/`Settled`, `DetachedWorkOutcomeKilled`, and every `DetachedWork*Update`
/`OutputAppend` delta message — a row upsert replaces the whole bubble.

**Why, in the user's terms.** "Your protos look fine" with the naming
ruling. Kind is the arm (mirrors `ToolCallBlock.call`'s detachable arms +
unmodeled); label/glyph resolved from the origin; the shell spool is a
resolved whole with `truncated` for paging inside the bubble.

**Consequences.** The webapp's `async-bubble.ts` reads one bubble message;
its 3-tier identity ladder and append handling go. The daemon caps the spool
it sends and serves the rest as pages inside the bubble.

### `feed.proto` kind ②: `FeedAgentRow` — head, blocks (prose · thinking · tool card), stop notice

**What changed.** `FeedAgentRow { FeedAgentHead {author, usage stamp};
repeated FeedAgentBlock { prose | thinking {shown{text,tokens} | redacted} |
FeedToolCard }; FeedAgentStopNotice {max_tokens|interrupted|refusal|
unsupported} }`. `FeedToolCard { ToolCallId call; FeedToolCardHeadline
{icon,title,subtitle}; FeedToolCardArguments {lines}; oneof outcome {
running | returned {succeeded{blocks} | failed{blocks}} | detached {bubble
MessageId} | denied {reason} } }`. DELETED old bodies: `AgentEmission`,
`AgentResponse`, `ToolCallVerdict`, `ResponseUsageStamp`,
`AgentToolOutcome`.

**Why, in the user's terms.** "Sounds good." A resolved element under the
precedence principle — this is the reopened "carries `AgentSaid` whole"
decision, decided: NOT embedded. The per-tool headline/arguments are the
daemon's projection of `ToolCallBlock.call`, so the "Bash has a `command`"
knowledge lives once, in the resolver. `ToolCallVerdict.spawned_message_id`
became the `detached {bubble}` outcome; the stop notice draws only stops
worth drawing.

**Consequences.** `daemon/internal/frontend/translate.go`'s re-encoding
becomes the row resolver; the webapp's `render.ts` per-tool branches
(`render.ts:1691-1740`) are deleted — it draws headline/arguments verbatim.

### `feed.proto` kind ①: `FeedUserRow`, and the feed's drawn block vocabulary

**What changed.** `FeedUserRow { FeedUserAuthor author {label};
FeedUserBody body { repeated FeedUserBlock } }`; `FeedUserBlock` = oneof
`FeedTextBlock {text}` | `FeedImageBlock {src, alt}` | `FeedUnsupportedBlock
{kind}`. The three block messages are the feed's DRAWN block vocabulary,
shared by every row kind in this file.

**Why, in the user's terms.** "Sounds okay" — after asking what the row is
for: it draws a `UserSaid` record, the user's own prompt in the history
(the webapp renders these today). A resolved element, not an embedded
`UserSaid` (precedence principle): the record's image reference (a host path
or a URL) becomes a `src` the webview can fetch — a resolution only the
daemon can make; `unsupported` draws its kind only, the raw stays in the
record.

**Consequences.** The daemon's feed resolver serves images (or maps paths to
served URLs); the webapp's user-card renderer reads blocks, not the record.

### `frontend.v1/feed.proto`: the container — `FeedRow` (nine kinds) and `FeedPage`; `status_panel.proto` split out; the whole tree compiles again

**The drawing agreed with the user** (in the file header): the feed as a
scrolling list of rows — user, agent (with tool cards), detached bubble,
permission card, synthesized failure card, api-failure row, context-cut
divider, merge bubble, live preview — plus the paging edge. "I don't think
we need to change anything here on the UI organization front."

**What changed.**

- `FeedRow { MessageId id; MessageId parent; TurnId turn; oneof row { user,
  agent, detached, permission, failure, api_failure, context_cut, merge,
  preview } }` — a row is the unit; a live update re-pushes the WHOLE ROW
  (upsert by id). `FeedPage { repeated FeedRow rows; oneof edge { has_more
  {cursor}; at_start {} }; FeedBreadcrumbs breadcrumbs }`;
  `FeedBreadcrumb { MessageId target; string label }`.
- The nine kind messages are declared EMPTY, each filled at its own
  increment; the old bodies (`AgentEmission`, `AgentResponse`,
  `ToolCallVerdict`, `ResponseUsageStamp`, `AgentToolOutcome`,
  `FailureCardView` + lifecycle, the `DetachedWork*` family) are kept
  VERBATIM under a "PENDING" banner and judged kind by kind.
- DELETED: `Message` (→ `FeedRow`), `ConversationDelta`, `DetachedWorkDelta`,
  `TypingCut` (transport arms of the dead stream; the live channel's shape is
  stage 3's), `TaskCatalog`/`TaskEntry`/`TaskStatus*` (superseded by the
  detached bubble and the footer's expanded rows), `CompactionSummaryItem`
  (folds into the context-cut divider), `PageScope*`,
  `ConversationHistoryPage`, `HistoryHasMore`/`HistoryAtStart` (→ `FeedPage`
  + edge; scope survives only as a stage-3 request parameter),
  `DaemonInterceptedCommandItem` — the user: "should not be discernible by
  the UI, it seems like an implementation detail" (answer 3).
- `SessionInitView`/`SessionInitRow` → NEW `status_panel.proto`
  (`StatusPanelView { repeated StatusPanelRow { label; value } }`, no
  workspace/fence): the `/status` panel is its own component opened by the
  command, not a feed row.
- EVERY PACKAGE COMPILES for the first time since the transport reversal.

**Consequences.** Nine kind increments follow, one at a time, each judged
under the precedence principle (a resolved element, not an embedded record).
The `agentrepl.v1` holding file compiles now that `feed.proto` does.

### `workspace` and `fence` leave every `frontend.v1` view (YAGNI); the feed page carries no `scope`

**What changed.** `TopbarWorkspace`/`TopbarFence`,
`TokenBreakdownWorkspace`/`TokenBreakdownFence`,
`DaemonHoldWorkspace`/`DaemonHoldFence`, `FooterWorkspace`/`FooterFence` and
their view fields are DELETED; the views renumber contiguously. (The
sidebar's `RosterRowWorkspace` stays — it is the ROW's identity on a global
stream, not addressing.) All frontend files compile except `feed.proto`.

**Why, in the user's terms — three questions, answered.**

- "Why is FeedWorkspace needed? Is that not implicit in the URL?" — It is:
  every component stream is per-workspace, so the workspace is the stream's.
  "Okay, let's drop it then. YAGNI." Dropped everywhere, not deferred to 3b.
- "What is fence … do we need this then?" — It guarded the old MULTIPLEXED
  stream against a late push from a retired session generation. With one
  ordered stream per component per workspace a push cannot overtake a newer
  one on its own stream, and cross-stream staleness has no comparison target
  (`WorkspaceState.fence` left `frontend.v1`). Liveness is the keepalive,
  ordering is the stream. Dropped; if a cross-stream generation check is
  ever needed it is one convention field at 3b.
- "Is scope implicit in ancestors?" — Yes: the last breadcrumb IS the
  container the page is of, and no crumbs = the top-level feed; `scope` on
  the page was a second spelling. It survives only as a REQUEST parameter at
  stage 3. And `ancestors` as bare ids would have made the client resolve
  labels — a derivation — so it becomes a drawn element,
  `FeedBreadcrumbs { repeated FeedBreadcrumb { MessageId target; string
  label } }`, outermost first, empty for the top-level feed.

**Consequences.** This SETTLES two of the STAGE-3b questions early, by the
user's ruling: per-workspace addressing is the stream's, and there is no
fence. The daemon's fence minting and the webapp's byte-compare gate are
deleted in the wave. `feed.proto`'s container is `FeedPage { rows; edge;
breadcrumbs }`.

### `frontend.v1/failure.proto`: the vendor family shrinks to what nothing recorded; `shim.v1` projected out

**What changed.** `FailureKind` goes from 40 arms to 28: machinery 1–17
unchanged; VENDOR is now five arms — `vendor_max_turns`, `vendor_max_budget`,
`vendor_execution_error`, `vendor_turn_failed`, `vendor_network_down`
(18–22, messages renamed `FailureVendor*`, each still carrying
`VendorFailureContext`); client-local 23–28 unchanged (renumbered). DELETED:
the ten API-class arms and their `FailureApi*` evidence messages
(authentication, billing, rate limit, invalid request, server error,
overloaded, oauth-org, model-not-found, request-failed, unknown), and
`api_refusal`, `api_max_output_tokens`. The four `shim.v1` types
`QueryTerminationFailure` named are PROJECTED into local messages
(`QueryTerminationVendorIdentityUnavailable`, `…UnexpectedEof`,
`…IteratorFailure {cause}`, `…StartupFailure {cause}`); the `shim.v1` import
is gone. `VendorFailureContext`, `FailureCardRef` and every machinery
evidence message unchanged. Compiles.

**Why, in the user's terms.** It stays a shared vocabulary file (the user:
"unless failure needs to be reused in other files, I don't see the point" —
it IS reused: `agentrepl.v1` error arms name its evidence), so the feed's
card stays in `feed.proto` and there are no `*_card.proto` files. The vendor
cut: a vendor API failure the vendor RECORDED is a
`conversation.v1.ApiRequestFailed` record and reaches the feed as its own row
(the daemon resolves it into a frontend-shaped card there, per the
precedence principle); a synthesized failure card carrying the same fact
would draw one failure twice. Only what nothing recorded — the SDK ending
the query on its own terms, or no response at all — is a card kind here.
`api_refusal`/`api_max_output_tokens` are `StopReason` arms on `AgentSaid`
(answer 5). The user: "let's proceed."

**Consequences.**

- `daemon/internal/errclass` classifies API failures into
  `ApiRequestFailed.kind` for the record path, not into ten `FailureKind`
  arms; the ten `errclass` types that mapped to them lose their frontend
  arm.
- The wave must VERIFY that every vendor API failure the daemon sees LIVE
  also lands as a transcript record; if some do not, the feed would miss
  them and this decision reopens.
- `agentrepl.v1` error arms (stage 3) that named the deleted vendor evidence
  now name `FailureVendor*` or the record type.
- The user asked to PAUSE at the end of stage 2 for approval before
  continuing to stage 3.

### PRINCIPLE: figma→idl takes precedence over no-respell at the backend→frontend boundary (no proto landed)

**Decided, the user's words spat back and confirmed:** `frontend.v1` is
composed ONLY of element messages shaped for the drawn element. When an
element's props derive from an internal type — a `conversation.v1` record, a
`shim.v1` wire type, a daemon-internal fact — the DAEMON RESOLVES a
frontend-shaped message for that element: a deliberate re-spelling of the
internal fact into UI vocabulary. "One canonical form / never re-spell /
import the encompassing message" yields to figma→idl at that boundary.

**Why it is safe there and nowhere else.** The daemon is the single resolver,
so the frontend copy is a resolved VALUE re-published on every change and
self-corrects like any duplicated value; the internal type keeps its one
canonical form for records/transport; the frontend never becomes a second
AUTHOR of the fact, only a projection of it. NOT licensed: re-spelling within
`conversation.v1`/`shim.v1`/`store.v1`; `frontend.v1` re-declaring another
surface's type for any reason other than drawing; typed identities
(`MessageId`, `TurnId`, `ToolCallId`) are still imported — join keys, not
props. Being added to the skill by a one-shot subagent
(`proto-figma-idl-precedence`), genericized.

**REOPENED BY NAME.** The superseded record's "`frontend.v1` stops flattening
`AgentSaid`; it carries it whole and stamps alongside" was the no-respell
rule applied across exactly this boundary. Under this principle the feed's
agent card may be a resolved element message rather than an embed — decided
at `feed.proto`'s turn, not kept by default.

### `footer.proto` closes: PLUMBING leaves `frontend.v1`; three state enums and `HeartbeatView` die

**What changed.**

- DELETED outright: `RenderState`, `SessionConnectivity`, `SessionStatus` —
  three state ENUMS (a live violation of the state rule) whose only readers
  were the daemon's resolvers; their projections are now the footer's
  `FooterStatus`/`FooterSubStatus`, the sidebar's `RosterRowStatus` and the
  topbar's `TopbarConnectivity`. And `HeartbeatView` — its own comment said it
  had nothing to tick with.
- MOVED VERBATIM to `agentrepl/v1/host_surface_pending.proto` — a HOLDING
  FILE, explicitly not a design, deleted when stage 3 has placed or deleted
  every message in it: `RuntimeFault`, `WorkspaceState` (with the three enum
  fields `reserved` and marked "STAGE 3"), `SessionView`, `BackfillState`,
  `DaemonView`, `DetachedCancelOutcome`, `DetachedAgentsCancelled`. These are
  the daemon's resolution inputs and the HOST surface Emacs reads (session
  identity, controller generation, catalog entries, backfill, boot id) plus
  one RPC ack payload — none is drawn.
- `footer.proto` now imports only `conversation/v1/message.proto` and
  `conversation/v1/turn.proto`; `frontend.v1` no longer imports `shim.v1` or
  `agentrepl.v1` from the footer. Every `frontend.v1` file except `feed.proto`
  and `failure.proto` (their turns next) compiles.

**Why, in the user's terms.** "Your read sounds good." On heartbeats: "still
useful for animation on status? Or maybe status should just be repeated to
simulate heartbeats" — answered: animation is client-side for as long as the
status arm is active (a state, not a push); LIVENESS is a property of the
connection, so it becomes a STAGE-3b convention (every component stream
carries a keepalive at cadence C; a client that hears nothing for T marks the
component stale) rather than a heartbeat arm on each view or an identical
view re-pushed with a second meaning. The "long tool still working" evidence,
when the daemon has any, is a `StatusActivity` update.

**Consequences.**

- The daemon's SSM keeps its state machine internally; the wire carries only
  the projections. `daemon/internal/ssm/resolve.go`'s rank table stays; the
  `frontendv1.RenderState` Go type it emits does not.
- The `fence` every component view carries is anchored on
  `WorkspaceState.fence`, which is now in the holding file — STAGE-3b decides
  fence vs stream order vs keepalive together.
- Stage 3 owes: a host stream/response set for what Emacs reads out of
  `WorkspaceState`/`SessionView`/`DaemonView`; the `DetachedCancelOutcome` ack
  arms; and the deletion of the holding file.

### `frontend.v1/footer.proto` — StatusActivity settled: six typed arms, no free-text escape

**What changed.** `FooterStatusActivityNote { string text }` and its `note`
arm are DELETED. `FooterStatusActivity` is six typed kinds: merging_commit,
hook, retrying, authenticating, blocked_on_user, rate_limited.

**Why, in the user's terms.** "Intuitively, it seems like it's floating in
the ether? Shouldn't this be specific to the error route?" — Yes: a line with
no kind belongs only where the daemon genuinely CANNOT classify, and that
place already exists — the error route's unclassified funnel on the failure
card. An activity the daemon can name but has no arm for is a modeling gap;
the fix is the arm. Same stance as `UnsupportedBlock`'s "not a fallback",
applied one step further: not even a guarded escape.

**Left as landed, flagged:** `FooterStatusActivityRetrying.status` and
`FooterAllowance.status` carry the vendor's verbatim status words — the
vendor's full vocabularies are not in evidence; typed arms when they are.
`rate_limited` stays an activity (transient, outranks ordinary windows) rather
than a `blocked` sub-status.

**The footer strip is now at submessage resolution end to end**: Status (9
empty arms), SubStatus (5 families), StatusActivity (6 kinds), Clock, Tokens
(5 elements), and the expanded rows. What remains in `footer.proto` is the
PLUMBING block and `DetachedCancelOutcome`, both awaiting the user's ruling.

### `frontend.v1/footer.proto` — the Tokens section's submessages; `ContextCostAlert` and the accounting cell find their home

**The drawing agreed with the user:**

```
│ 18.2k in · 3.1k thought  ⚠  ✓ │
     input     thinking     │  └ accounting verdict badge (hover: phrases)
                            └ expensive-turn alarm (hover: "41k over 20k")
```

**What changed.** `FooterTokens` is five element messages, ALL siblings:
`FooterTokensInput {tokens}`, `FooterTokensThinking {tokens}`,
`FooterTokensFirstToken {latency_ms}`, `FooterTokensExpensiveTurn`,
`FooterTokensAccounting`. `ContextCostAlert` → `FooterTokensExpensiveTurn
{ TurnId turn; uncached_input_tokens; threshold_tokens; at_ms; oneof origin
{ prompt {}; cold_keep_alive {} } }` — the shim's 27-value `PromptOrigin`
PROJECTED to the two cases the alarm renders differently, verified against
`shim/v1/core.proto:97-140`; `frontend.v1` no longer names `PromptOrigin`.
`FooterAccountingCell` + `Accounting*` → `FooterTokensAccounting { summary;
oneof verdict { complete; incomplete{missing}; invalid{problems} } }` and
`FooterTokensAccounting*` arms. The "PENDING INCREMENT 2" block is gone.

**Why, in the user's terms — and a rejected shape, kept visible.**

- "ContextCostAlert needs to be modeled in the token protobuf shipped for the
  corresponding footer section" — done; and the accounting verdict is a fact
  about the turn's tokens, so it sits on the same cell as a badge; the
  topbar's `TopbarAccountingWarning` points at THIS.
- The user asked whether the cell had mutually exclusive siblings. The
  orchestrator first proposed a `live` / `settled` oneof; the user's next
  questions dissolved it: first-token is per MESSAGE (unset until the current
  message's first token, then held) and excludes nothing; the expensive-turn
  alarm renders THE MOMENT it trips (mid-turn), so it is not a "settled" fact;
  the cell always shows ONE turn's cost at increasing completeness, and a new
  turn resets it. So: siblings, no mode oneof; the only exclusivities are
  `verdict` and `origin`.
- The user then asked, non-rhetorically, why not an event shape —
  `oneof { update{ oneof {input|thinking|expensive} }; done{ accounting } }`.
  Answer recorded: it models the TRANSPORT (a change sequence), not the VIEW.
  The client would accumulate updates into the cell (client-side derivation;
  a fence discard or reconnect loses a field with no whole push to recover
  from); the inner oneof claims input and thinking are never drawn together
  (they always are); `done` would erase the figures when the verdict lands;
  and ordering becomes load-bearing inside one component. Every component
  here is pushed WHOLE; if a ticker's rate ever mattered, the fix is
  daemon-side coalescing, not deltas on the wire. The user: "okay proceed".

**Consequences.** `daemon/internal/progress` publishes the whole cell per
change and projects `PromptOrigin` to two arms; the webapp's expensive-turn
and accounting renderers read the tokens cell.

### `frontend.v1/footer.proto` increment 2: the expanded section is rows of in-flight detached work, and nothing else

**The drawing agreed with the user:**

```
├──────────────────────────────────────────────────────────────────┤
│ ⚙ Explore   "find the roster resolver"               0:31   ▸ │  subagent  → click: its feed bubble
│ ⛓ workflow  review-changes · verify 3/5              2:10   ▸ │  workflow  → click: its feed bubble
│ $ bash      go test ./...                            1:04   ▸ │  shell     → click: its feed bubble
```

**What changed.** `FooterExpanded { repeated FooterExpandedRow rows }`;
`FooterExpandedRow { conversation.v1.MessageId target; FooterExpandedRowRuntime
runtime { started_at_ms }; oneof row { FooterExpandedSubagent {agent_type,
description}; FooterExpandedWorkflow {name, current_step};
FooterExpandedShell {command}; FooterExpandedUnmodeled {tool_name} } }`.
Each row is a jump target: `target` is the item's `DetachedWorkStarted`
message, and activating the row navigates to that feed bubble. No heading
(the strip is the header; an invented "in flight (3)" title was dropped
because it maps to no UI element). `footer.proto` imports
`conversation/v1/message.proto` for `MessageId`.

**Why, in the user's terms.** "The expanded section should be fundamentally
restricted to rows. Those rows should have one of some number of types, and
they should be reserved for asynchronous work … clicking the SubAgent takes
you to the subagent's feed-level bubble, clicking the Workflow takes you to
the workflow's feed-level bubble." The row kinds are exactly the detachable
origins of `ToolCallBlock.call` (`agent`, `workflow`, background `bash`), plus
`unmodeled` so detached unmodeled work cannot vanish from the list; a skill is
not async work.

**Homeless as a result — the user rules, one at a time (five answers):**
the four rows the orchestrator first drew all had homes elsewhere and are
NOT footer: failure line → the feed's failure card + the `blocked` status
(answer 5); gate line → the `asleep`/`merging` statuses (5); merge note →
`SubStatus`/`StatusActivity` (5). Two messages remain under the "PENDING
INCREMENT 2" banner awaiting the ruling: `ContextCostAlert` (the
expensive-turn alert — a daemon-synthesized feed card? an activity note?) and
`FooterAccountingCell` + `Accounting*` (the settled turn's reconciliation —
the topbar's `TopbarAccountingWarning` today references "the footer cell's
evidence", so the verdict arms need a home if the cell goes).
`FooterFailureRow` is deleted outright (answer 5).

### `frontend.v1/footer.proto` increment 1: the main strip, reimagined as three typed resolution levels

**The drawing agreed with the user (his reimagining of the strip):**

```
│ merging   │ cherry-picking 3/7  │ 4f2a1c: fold tokens into api │ 0:42 │ 18.2k in · 3.1k thought │
  Status      SubStatus             StatusActivity                 Clock   Tokens
  coarsest ──────────── resolution increases left → right ────────► finest
```

**What changed.** `ProgressView` is replaced by `FooterView { FooterWorkspace;
FooterFence; FooterStrip; FooterExpanded (empty, increment 2) }`.
`FooterStrip { FooterStatus; FooterSubStatus; FooterStatusActivity;
FooterClock; FooterTokens }`:

- `FooterStatus` — nine EMPTY arms: idle, thinking, waiting, interrupted,
  merging, background, blocked, asleep, disconnected. The coarsest state;
  color per arm from the shared vocabulary, resolved by the daemon.
- `FooterSubStatus` — a oneof of per-status FAMILIES, each a oneof of steps
  with the step's small facts: thinking {submitting, thinking, clearing,
  compacting}; merging {enqueuing, queued{position,depth},
  before_action{action}, cherry_picking{commits}, testing{commits}, conflict,
  after_action{action}, failed, merged} — the merge run's own phase
  vocabulary projected; disconnected {starting, degraded, severed, dead,
  start_failed}; idle {ready, done}; blocked {auth, usage_limit,
  vendor_error}. Unset for statuses with no substructure.
- `FooterStatusActivity` — typed KINDS whose payload is mostly composed text:
  merging_commit{sha,subject}, hook{name}, retrying{attempt,status},
  authenticating{line}, blocked_on_user{detail}, rate_limited{session,weekly
  FooterAllowance}, note{text} (the daemon's free line for a thing with no
  kind yet).
- `FooterClock { optional turn_started_at_ms }`; `FooterTokens
  { input_tokens; thinking_tokens; optional ttft_ms }`.

DELETED: `ProgressWindow`, `RateLimitWindow`, `InterruptWindow`,
`FooterPhase`, `FooterMergeChip`, the deprecated `ProgressView.state` copy,
the interrupt chip (an `interrupted` status now), and the counters
(`pending_permissions`, `queue_depth`, `live_task_count` — held → the tray's
heading; permissions → the feed's cards; tasks → the tray/sheet).
KEPT VERBATIM under a "PENDING INCREMENT 2" banner: `ContextCostAlert`,
`FooterFailureRow`, `FooterAccountingCell`, `Accounting*`; under "PENDING":
`DetachedCancelOutcome`/`DetachedAgentsCancelled` (an ack payload, stage 3);
and the whole PLUMBING section (`RenderState`, `SessionConnectivity`,
`SessionStatus` enums, `RuntimeFault`, `WorkspaceState`, `SessionView`,
`BackfillState`, `DaemonView`, `HeartbeatView`) — not drawn anywhere; its
home is decided after the footer.

**Why, in the user's terms.** "The main phase should be on the left … quite
general, so no 'merge testing', just 'merging'. The second section should be
where the finer resolution comes in. The third section [interrupt chip] I
don't see a reason for at all; we should have an 'interrupted' main status
cleared on subsequent status update. The fourth section, activity, should be
the finest resolution … dynamic output as determined by the daemon. Clock and
tokens are good. Counters can go." And: "the first three should all be typed
(just each 'less' typed than the next by having more of its information
implicit in text fields)." Names chosen by the user: Status, SubStatus,
StatusActivity.

**Consequences.**

- The daemon's footer resolver picks ONE activity (today `ProgressView`
  ships all windows and the webapp applies precedence — client derivation,
  gone) and projects the interrupt outcome to a status, so `frontend.v1` no
  longer names `shim.v1.InterruptOutcome`.
- The status/sub-status arm sets are the daemon's `RenderState` re-cut along
  the six colors + merge family; whether `RenderState` itself survives (it is
  a state ENUM, a live violation) is the PLUMBING decision.
- `FooterAllowance.status` stays the vendor's verbatim word: the vendor's full
  rate-limit vocabulary is not in evidence; typed arms when it is.
- Rows and sheet (failure row, expensive-turn row, merge note, gate row,
  accounting cell) are increment 2, drawing first.

### `frontend.v1/daemon_hold.proto` (was `prompt_queue.proto`): the held tray; `conversation.v1/turn.proto` adds `TurnId`

**CORRECTION, KEPT VISIBLE.** The seven-arm `DaemonHold` type and its
`hold.proto` proposed in the previous entry are WITHDRAWN. The user doubted
`hibernated` belonged; the daemon's evidence agreed and went further:
`ErrSessionHibernated` is raised on open/create (`createestablish.go:372`,
`openfailure.go:52`) — the revival GATE, holding nothing;
`ErrPromptRefusedByMergeState` REFUSES a prompt (`mergepromptgate.go:71`),
holding nothing; the uninterruptible context cut is a CLASSIFICATION verdict.
The four real holds (shutdown drain, keep-alive turn, revival pending, build
refresh) are exactly the arms already on disk and are entry-scoped. ROOT
CAUSE of the error: the orchestrator generalized from the WORD "not yet"
across refusals, gates and holds without checking which of them held
anything. There is no shared hold type; the hold oneof stays on the entry.

**What changed.**

- `prompt_queue.proto` → `daemon_hold.proto`. `QueueView` → `DaemonHoldTray {
  DaemonHoldWorkspace; DaemonHoldFence; DaemonHoldHeading; repeated
  DaemonHoldItem }`; `DaemonHoldItem { oneof item { HeldPrompt prompt;
  HeldOffer offer } }`.
- `QueueEntry` → `HeldPrompt { conversation.v1.TurnId turn;
  conversation.v1.UserSaid said; HeldPromptQueuedAt queued_at; oneof
  classification (5 arms, renamed `HeldPrompt*`, semantics verbatim); oneof
  hold (4 arms, renamed, semantics verbatim) }`. `QueueClassificationHold.
  accepted` (bool) is `HeldPromptAccepted { bool }` inside the
  hold_for_turn_end arm. `HeldPromptKeepAliveHold.turn_id` (string) is
  `TurnId turn`.
- NEW `HeldOffer { oneof offer { HeldOfferMergeDequeue merge_dequeue } }` —
  a question the daemon holds for the user's answer; the merge-dequeue card's
  home. `HeldOfferMergeDequeue` is EMPTY ON PURPOSE: its body is decided when
  `agentrepl.v1.MergeDequeueOffer` is walked (stage 3), not guessed here.
- NEW `conversation/v1/turn.proto` — `TurnId { string value }`, reopening
  stage 1 additively. Shared vocabulary in the leaf (the `SessionCommand`
  argument): a submission's response returns one, the tray holds under one,
  the shim's turn bookkeeping names one, feed rows are stamped with one.

**Why, in the user's terms — the questions that shaped it.**

- "Should HeldPrompt be implemented in terms of conversation.v1.UserSaid?" —
  YES: a held prompt IS a `UserSaid` not yet forwarded; `string text` was a
  partial re-spelling of `UserContent` that would drop images. Consequence:
  `SubmitPromptRequest` (stage 3c) carries `UserSaid` too — one canonical form
  client → daemon → tray → shim → record.
- "Should there be a canonical identifier?" — YES, and there were TWO: the
  daemon-minted `QueueEntry.id` (`queue.go:591`) and the client's `request_id`
  carried on the same entry (`queue.go:32`), which becomes the shim's turn id
  (`core.proto:252`). One identity: the turn.
- "Who mints these IDs? I'm concerned the webapp might be minting prompt
  ids." — Today clients do (`fe-80-fdb1`), justified by a race the one-stream
  design had (a push could beat its ack). Under SDUI clients reconcile
  nothing, so THE DAEMON MINTS `TurnId`; `SubmitPromptSuccess` returns it. A
  client-minted idempotency key on the request is a separate stage-3b
  question and is not the turn's identity.
- "How does the webapp's feed resolve a response to a request?" — it
  doesn't: unary responses answer requests; the daemon STAMPS feed rows of a
  turn with `TurnId` (the existing stamps-alongside pattern), so a client that
  wants to highlight its own prompt matches the id it was returned; every
  other effect is a pushed view update. Optimistic rows and the client's
  pending-request map (`command-dispatch.ts` `onAck`) go away.
- Hibernation/revival gate: NOT a tray item — a workspace-level gate;
  placement still open for the footer drawing.

**Consequences.**

- The tray's stream is `WatchDaemonHolds` (stage 3a); the footer counter reads
  "N held".
- `daemon/internal/sessioncontroller/queue.go` drops `newQueueEntryID()`; the
  entry is keyed by the daemon-minted turn id, and duplicate submissions are
  refused by idempotency key rather than by a second id.
- `promptreceipt.go`'s "refuse a turn claim with no request id" becomes
  "refuse a duplicate idempotency key"; the guarantee survives, the owner
  changes.
- Stage 4 (`shim.v1`): `turn_id`/`request_id` strings become `TurnId`.
- Stage 2 `feed.proto`: rows of a turn carry a `TurnId` stamp.

### The prompt queue leaves `footer.proto`: `prompt_queue.proto`, its own component and stream

**What changed.** The "daemon-held prompt queue" section — `QueueView`,
`QueueEntry`, the five `QueueClassification*` arms, the four `QueueEntry*Hold`
arms — moved VERBATIM into `frontend/v1/prompt_queue.proto`. `footer.proto`'s
header no longer claims prompt intake or a composer; its unused
`session_command` import is dropped. Shapes are unchanged by the move and are
judged in the queue file's own increment.

**Why, in the user's terms — three questions, answered in order.**

- "Are we modeling the prompt queue as part of the footer?" — No, and the
  first footer drawing was wrong: the webapp renders queued prompts as
  `queued-card`s (`render.ts:507-570`), the composer is HOST-native (Emacs
  runs the webview with `composer=0`), and the footer strip carries only the
  `N queued` counter.
- "Where do queued prompts come from? The daemon, right?" — Yes,
  exclusively: a prompt submitted while a turn runs is held, classified and
  delivered later by the daemon; the vendor never sees it until then, so it
  is daemon-owned pending intent, correctly absent from `conversation.v1`.
- "Do we want the UI component of the queue to be in the feed? Or should it
  be separate and monitored separately?" — SEPARATE: the feed is history +
  the live turn (scrolls, pages, appends); the queue is the FUTURE
  (whole-list-replaced on every change). Drawn as its own "pending" tray at
  the feed's tail, above the footer; own stream (`WatchPromptQueue`, stage
  3a); the feed knows nothing about it. Agreed: "okay makes sense".

**A vocabulary decision recorded here, to be landed at the queue's shape
increment:** the daemon's "NOT YET" appears in six places with three
spellings (per-entry hold arms; `WorkspaceState.merge_lease_held`;
hibernation/revival gate; scheduled-shutdown drain; the uninterruptible
context cut; `Failure*` refusal arms). One type — `DaemonHold`, a oneof of
reasons (merge lease, hibernated, revival pending, shutdown drain, keep-alive
turn, build refresh, context cut) — declared once in `frontend/v1/hold.proto`
and EMBEDDED wherever a view or response says "not yet": `QueueEntry.hold`,
a footer gate element, the host stream Emacs watches (it owns the composer),
and stage-3 error arms. It is a TYPE with no RPC of its own. What does NOT
generalize: the queue's classification verdict (interject / hold-for-turn-end
/ pending / error) — that is ordering, not deferral.

**Consequences.** `footer.proto` shrinks to the strip, rows and sheet plus
the PLUMBING section, whose fate is the footer increment's; the webapp's
queued-card rendering moves from the feed renderer to a tray component; the
feed's paging never sees a queued entry.

### `frontend.v1/sidebar.proto` follow-up: the message tree is the UI tree

**What changed.** No semantics change; the element-message and
message-tree-is-UI-tree conventions applied. Every bare field became a
message and every drawn box became one message with nesting equal:

- `WorkspaceRoster { view; RosterMergedSection recently_merged;
  RosterCurrentWorkspace current { dir }; RosterNavCursor nav { dir } }`.
- `RosterRepoSection { RosterRepoKey key; RosterSectionHeader header;
  RosterRows rows }`; `RosterTaskSection { RosterTaskKey key;
  RosterTaskSectionHeader header; RosterRows rows }`;
  `RosterMergedSection { RosterSectionHeader header; RosterRows rows }`.
- `RosterSectionHeader { RosterLabel; RosterFold }` shared by repo and merged
  sections; `RosterTaskSectionHeader { RosterLabel; RosterFold;
  RosterTaskDone }` its own, because the done check is drawn IN the task
  header and repos have none — the user's earlier `RosterSection` reuse
  survives as the shared header + `RosterRows`, not as one message
  coalescing header and rows.
- `RosterRow { RosterRowWorkspace; RosterRowName; status (26 arms,
  unchanged); RosterRowCurrent; children; RosterRowWhen; RosterRowDetail;
  RosterRowClosed }`.
- `RosterRowWhen` is a ONEOF — `last_selected { at_ms }` | `merged
  { at_ms }` — chosen by the daemon: precedence (merged wins) is resolved
  server-side, and the client renders whichever arm arrives (the user's
  correction of the orchestrator's two-optional-fields sketch).
- `RosterRowDetail { RosterRowDetailBranch; RosterRowDetailParentBranch;
  RosterRowDetailSummary }` — three lines, each present/absent by message
  presence, not empty string.
- The file header carries the ASCII layout the shapes were checked against.

**Why, in the user's terms.** "Are our messages making this organization
implicit? Or are we coalescing adjacent fields into different components in
the UI hierarchy?" — the answer was that two places coalesced (section header
vs rows; the three detail lines), and both were re-partitioned. Approved:
"apply your RosterSection changes you just suggested, then let's move on."
The constraint itself — message tree = UI tree, ASCII-checked, agreed with
the user BEFORE shapes are sketched — is being added to the skill by a
one-shot subagent (`proto-message-tree-mirrors-ui-tree`).

**Consequences.** Renderers read one message per box; presence replaces the
empty-string and zero-sentinel conventions in the row; the webapp's
when-column precedence code is deleted (the daemon decides).

### `frontend.v1/topbar.proto`: every field an element message; health views out; `ModelOption` moves to `conversation.v1`; the breakdown menu gets its own file

**A NEW CONVENTION, stated by the user during this increment and being added
to the skill by a one-shot subagent (`proto-ui-element-messages`):** in
figma→idl / SDUI, a component's view message contains NO dangling primitives
— EVERY field, including ones like `workspace` that carry addressing, is
wrapped in a dedicated, appropriately-named message, so the UI's
subcomponents are implicit in the schema and no field is conflated with a
neighbor. And when the SAME fact appears in two component views, each
component wraps it in ITS OWN message (`TopbarWorkspace`,
`TokenBreakdownWorkspace`) rather than sharing one — "that's EXACTLY
PERFECT: duplicate information represented with dedicated messages implies
separate UI subcomponents." The orchestrator's first two sketches (scalars
allowed for addressing; a shared wrapper considered) were both corrected by
the user; both corrections are the convention now.

**What changed.**

- `TopbarView` is seven element messages and nothing else: `TopbarWorkspace
  { dir }`, `TopbarFence { token }`, `TopbarTitle { text }`,
  `TopbarSessionLine { text }`, `TopbarModelSelector { ModelOption selected;
  repeated ModelOption options }`, `TopbarConnectivity` (unchanged shape),
  `TopbarWarningStrip { repeated TopbarWarning }`. `model_display` (a string)
  is gone — the selection is the whole option, presence = selected.
- `DaemonHealthView` and `SessionHealthView` DELETED from `frontend.v1`: not
  drawn by anything, they are the answers to two host commands and become
  `agentrepl.v1` responses at stage 3 (`bool healthy + string reason` → a
  healthy/unhealthy oneof there).
- `shim.v1.ModelOption` DELETED; `conversation.v1.ModelOption` ADDED to
  `api.proto` (an API fact: the models the vendor offers), reopening stage 1
  ADDITIVELY — nothing landed in `api.proto` changes. `shim/v1/core.proto`'s
  `ModelCatalog.models` and `frontend/v1/footer.proto:427` repoint to it;
  `frontend.v1` no longer imports `shim.v1` from the topbar (footer still
  does, its turn next).
- `token_breakdown.proto` is a NEW FILE holding `TokenBreakdownView` and its
  tree, moved out of `topbar.proto`; `TokenBreakdownWorkspace`,
  `TokenBreakdownFence`, `TokenBreakdownHeading` are new element wrappers;
  `share_permille` is `optional` (was a -1 sentinel).
- `TopbarConnectivity.tone` stays a string: a color-class NAME from the
  shared `proto/vocab/render-colors.json` vocabulary, a rendering token, not
  a state. The state it derives from (`SessionConnectivity`, footer.proto) is
  an enum and is raised at the footer's turn.
- `workspace` and `fence` are KEPT (wrapped) and marked STAGE-3b: whether a
  per-workspace stream's element still names its workspace, and whether
  cross-stream fencing survives per-component streams, are conventions.

**Why, in the user's terms.** "This message looks good, I approve" — after
the two corrections above.

**Consequences.**

- The daemon's topbar resolver (`daemon/internal/frontend/topbar.go`) and
  the webapp's topbar renderer read element messages; the health-view
  producers move to stage-3 RPC handlers.
- Every consumer of `shim.v1.ModelOption` (shim, daemon, webapp) reads
  `conversation.v1.ModelOption`.
- RETROACTIVE, PENDING THE USER'S CALL: `sidebar.proto`'s `RosterRow` has
  dangling primitives that are elements (`dir`, `name`, `branch`,
  `parent_branch`, `summary`); the same convention applies there as a
  follow-up increment (`RosterRowName`, `RosterRowDetail {…}`, etc.).
- The typed workspace identity question (one `WorkspaceRef` type vs
  per-component wrappers) is ANSWERED by the convention: per-component
  wrappers, because they name subcomponents. A shared identity TYPE may still
  be wanted for `agentrepl.v1` requests (stage 3), where nothing is drawn.

### `frontend.v1/sidebar.proto`: daemon-resolved, global, view-only; `revision`/`boot_id` gone; `RosterSection` shared

**Two settlements above the shapes, both by selection.**

- SCOPE — GLOBAL. The sidebar's stream carries no `workspace`; every webview
  (one per workspace) watches the same roster and the daemon's stream order
  is the only order. Recorded together with the rendering topology it rests
  on: one WKWebView xwidget per workspace, bound for life, in its own pinned
  Emacs buffer (`lisp/frontend.el:847`), each with its own already-
  workspace-scoped socket (`webapp/src/address.ts:71`); consolidation to one
  webview was REFUSED (four objections in the discussion: buffer/xwidget
  binding, per-workspace socket, `SPC .` alignment, no cross-view sharing of
  process memory anyway), and endpoint-per-component ≠ connection-per-
  component — component streams multiplex over the one socket a webview
  already opens.
- FILE CONTENTS — VIEW ONLY. The user chose AGAINST the orchestrator's
  recommendation (view + events per figma→idl). The superseded record's
  "a request type is `agentrepl`, never `frontend`" (drawn/called) STANDS,
  unreopened: sidebar clicks are `agentrepl.v1` requests with plain fields.
  Recorded so the figma→idl "events in the same file" reading is not
  re-proposed for the other components: in this tree, events are called,
  not drawn.

**What changed in the file.**

- `WorkspaceRoster.revision` and `boot_id` DELETED with the whole
  epoch/monotonicity comment — no outside publisher, nothing to order. Fields
  renumber: `view` 1–2, `recently_merged` 3, `current_dir` 4, `nav_dir` 5.
- `RosterRepoSection` and `RosterTaskSection` now WRAP the shared
  `RosterSection { rows, folded, label }` with their own key (`repo_key`;
  `task_id` + `done`); `RosterTaskSection.title` becomes `section.label`.
  The user's amendment: "RosterSection should be reused."
- `RosterRow.last_viewed_at_ms` and `merged_at_ms` are `optional` (presence,
  not zero sentinels).
- Header and `status` comment rewritten for ONE resolver (the daemon
  coarsens `RenderState` onto the dot); the "sidebar.el's wire table is the
  third face" sentence is gone. Arm set unchanged (26 empty arms); `none`
  re-described as "registered, no session ever created".
- Kept, marked for stage 3a: `nav_dir` and `folded` — UI preference the
  daemon holds only if an `agentrepl.v1` verb tells it; else deleted or made
  webview-local there.

**Why, in the user's terms.** "Looks good", with the `RosterSection` reuse.
Emacs has nothing to do with `WorkspaceRoster` under the settled model: "it
can only register a workspace, and select a workspace. It doesn't have any
need to be able to work with the underlying workspace representation message
for the frontend."

**Consequences.**

- `frontend.v1` sidebar messages now flow in exactly ONE direction, daemon →
  webapp; the old inbound `agentrepl.v1` import of `sidebar.proto` for
  `PublishWorkspaceRosterRequest.roster` never returns.
- The webapp's `rosterFromFrame` staleness check on `revision`/`boot_id`, the
  elisp publisher and the daemon roster retainer are deleted in the wave.
- Consumers reading `RosterRepoSection.rows/folded/label` and
  `RosterTaskSection.rows/title` read `.section.*`.
- `RosterRow.dir` stays a bare `string`. A typed workspace identity is a
  real question (same argument as `MessageId`), but the fact originates at
  the daemon; where a daemon-owned identity type lives is a stage-3
  conventions call, flagged.

### Sidebar producer settled: the DAEMON owns the roster; Emacs sends COMMANDS, not state (no proto landed yet)

**Decided (stage 2, `sidebar.proto`, above any shape).** `WorkspaceRoster`
becomes a daemon-RESOLVED `frontend.v1` view. Emacs's contribution collapses
to commands over `agentrepl.v1` — in the user's words, "Emacs only needs to
REGISTER a workspace (subsequently the daemon tracks the information like
parentage, dir, etc.), and to SELECT a workspace (when the user switches tabs
in Emacs) … two separate messages passed along two separate RPCs, like
`RegisterWorkspace` and `SelectWorkspace`." The exact RPC names and set are
stage-3a inventory; the PRINCIPLE is settled here.

**Why.** Today Emacs authors the whole roster (`sidebar.el`), re-deriving each
row's status from what the daemon told it into a second vocabulary
(`RosterRowStatus`), which the webapp maps a third time — three spellings of
one status, and the source of the done-vs-interrupted class of bug. The
daemon already owns `dir` (registry), 24 of 26 status arms (`RenderState`),
branch/merge facts and summaries; the roster UNIVERSE and the selection were
the only genuinely Emacs-held facts, and both are events, not state.

**What it removes, and why each removal is safe.**

- `WorkspaceRoster.revision`, `boot_id` and the epoch/monotonicity rules —
  they guarded against an out-of-order STATE publish; a command has no stale
  roster to resurrect. The daemon's own stream ordering replaces them.
- Emacs's `publishWorkspaceRoster` path and the daemon's roster retainer
  (`daemon/internal/frontend/roster.go`) — the daemon resolves, so there is
  nothing to retain from outside.
- `RosterRow.current` / `last_viewed_at_ms` as Emacs-supplied — the daemon
  stamps both on `SelectWorkspace`.
- `closed` vs gone — marked by the daemon from the open/close/unregister
  traffic it already brokers.
- The presence-snapshot alternative the orchestrator proposed first
  (`WorkspacePresence` per row) — REJECTED by the user in favor of commands;
  recorded so it is not re-proposed.

**Residue, decided at stage 3a:** `nav_dir` (the Emacs keyboard cursor
riding the roster), the repo/task grouping mode and section folds are UI
preference, not workspace fact — either webview-local or one small
`SetRosterView`-style RPC.

**Consequences.**

- Daemon restart: the registry is durable; Emacs re-registers idempotently on
  reconnect (`RegisterWorkspace` must be idempotent by `dir`).
- Every consumer of `revision`/`boot_id` (elisp publisher, daemon retainer,
  webapp `rosterFromFrame` staleness check) is deleted in the wave.
- The `sidebar.el` status table (24 arms) is deleted; the daemon's roster
  resolver coarsens `RenderState` onto the sidebar's dot vocabulary once.
- Scope (global stream, no `workspace`) and file contents (view vs
  view+events) remain the two open sidebar questions above the shapes.

### `conversation.v1/session_command.proto`: no change — STAGE 1 COMPLETE

**What changed.** Nothing. `SessionCommand` stays an enum (a closed set of
command names, not a state; its per-value facts are schema options, not
sibling fields, so the adjacent-exclusivity test passes), `SessionCommandSpec`
stays an enum-value option, and the file stays in `conversation.v1` as the
leaf every surface reads.

**Why, in the user's terms.** Approved. The `context_cut` fold considered
earlier is refused: `ContextCut` is a record, `SessionCommand` a vocabulary no
record carries — different concerns.

**Stage 1 (`conversation.v1`) is complete** at this commit. The package is
nine files — `message`, `tool_call`, `agent`, `user`, `content_blocks`,
`context_cut`, `api`, `detached_work`, `session_command` — and compiles.
`frontend.v1` does not (`feed.proto` names deleted `conversation.v1` types),
which is stage 2's starting condition, by design.

**Carried into stage 2 as requirements from stage 1:**

- A daemon-synthesized MERGE feed item that coalesces the vendor records
  produced while a merge ran (from `detached_work.proto`).
- Typed identities to embed instead of strings: `MessageId`, `ToolCallId`.
- The tool card's per-tool presentation is resolved by the daemon from
  `ToolCallBlock.call`; the client never digs.
- `StopInterrupted` producer question owed to stage 4 / the wave.

### `conversation.v1/tokens.proto` folds into `api.proto`

**What changed.** `tokens.proto` is DELETED; `TokenUsage`, `TokenCacheHits`
and `TokenCacheMisses` move VERBATIM into `api.proto` under a "USAGE
ACCOUNTING" section banner that carries the old file's provenance header.
No shape change. Importers repointed: `conversation/v1/agent.proto`,
`shim/v1/bookkeeping.proto`, `state/v1/durable.proto` now import
`conversation/v1/api.proto`. Every non-frontend package compiles.

**Why, in the user's terms.** Approved as proposed: `TokenUsage` is the API's
own accounting of a request — the same actor as `ApiRequestFailed` — so one
file per concern puts them together: "the vendor API's outcomes: what it
charged, or why it refused".

**Consequences.** `conversation.v1` is now eight files: `message`,
`tool_call`, `agent`, `user`, `content_blocks`, `context_cut`, `api`,
`detached_work`, plus `session_command` (next and last). `state.v1` and
`shim.v1` gained no new dependency, only a renamed import.

### `conversation.v1/detached_work.proto`: the origin call is the whole description; `DetachedWorkKind` and `DetachedMerge` are gone

**What changed.**

- `DetachedWorkStarted { string origin_tool_call_id; string label;
  DetachedWorkKind kind }` becomes `DetachedWorkStarted { ToolCallBlock
  origin }`.
- DELETED: `DetachedWorkKind` and all six arms — `DetachedAgent`,
  `DetachedShell`, `DetachedWorkflow`, `DetachedSkill`,
  `DetachedUnclassified`, `DetachedMerge`.
- `DetachedLost { string inference }` becomes `DetachedLost { oneof how {
  file_vanished, went_silent, swept_up } }` with three empty arm messages.
- `DetachedWorkProgressed`, `WorkflowStepObserved`, `WorkflowStep` and its
  three arms, `SkillBodyResolved`, `DetachedWorkEnded`, `DetachedProcessExit`,
  `DetachedSucceeded`, `DetachedFailed`, `DetachedCancelled` — unchanged.
- The file now imports `tool_call.proto`.

**Why, in the user's terms.**

- The kind IS the origin call's `call` arm now that `ToolCallBlock` is typed:
  `bash` (run_in_background) is a shell, `agent` a subagent, `workflow` a
  workflow, `skill` a skill, `unmodeled` an unmodeled tool that detached.
  `DetachedShell.command`, `DetachedSkill.skill_name/args` and
  `DetachedUnclassified.tool_name` were re-spellings of `ToolCallBash.command`,
  `ToolCallSkill.skill/args`, `ToolCallUnmodeled.tool_name`. Import the
  encompassing message; answer 5 (already represented). `label` was a
  presentation the daemon resolves from the origin (answer 2).
- `DetachedMerge` — THE USER'S RULING, verbatim in substance: "Merge is a
  daemon-synthesized action: workspace merging can be represented specially
  on the frontend, but it's not something the vendor has any knowledge of. It
  can never be 'detached' because the agent isn't orchestrating it, the
  daemon is, exclusively." Dropped from this surface (answer 3, abstraction
  leak). It resolved a contradiction the file carried in its own comments
  ("NO TOOL SPAWNS IT — the daemon opens it" vs `message.proto`'s "the daemon
  is not an author here").
- `DetachedLost.inference` — three enumerated ways in its own comment; a
  closed set that answers "how did we conclude this" is a oneof.

**A STAGE-2 REQUIREMENT this creates, recorded so it is not lost.** The user
wants merging supported in the conversation as a `frontend.v1` feature: a
daemon-synthesized feed item that COALESCES every vendor record produced
while the merge ran (the merge is a Claude skill execution under the hood,
plus the resulting actions/responses) into ONE feed entry, and the daemon —
not any producer — determines which `AgentSaid`/`ToolReturned`/… belong to
that item. `frontend/v1/feed.proto`'s turn must model that row.

**Consequences.**

- `frontend/v1/feed.proto:587` references `conversation.v1.DetachedWorkKind`
  and no longer compiles; `conversation.v1` does. Intended — `frontend.v1` is
  stage 2 and is redesigned there; the break is not patched around.
- The daemon's async classification by tool NAME (`Agent`/`Task`/`Workflow`)
  becomes a switch on `origin.call`; the webapp's `asyncShape`/`classifyAsyncSource`
  likewise. Cards derive their label from `origin` server-side.
- A producer that emits `DetachedWorkStarted` must hold the originating
  `ToolCallBlock` at detachment time; the shim sees the call before the result
  that reveals detachment, so it does. The sidecar reading history has the
  call in the same transcript.
- Open, flagged not decided: whether a *skill* is really "detached" work (its
  records are the main agent's; the nesting is a UI window). Naming only.

### `conversation.v1/api.proto`: `FailureRaised` → `ApiRequestFailed`, with the vendor's error kinds as arms

**What changed.** `FailureRaised { summary, detail, retry_in_ms }` becomes
`ApiRequestFailed { string message; oneof kind { rate_limited, overloaded,
authentication_failed, permission_denied, invalid_request,
request_too_large, not_found, internal, unmodeled { type } } }`.
`retry_after_ms` is `optional` and lives ONLY on `ApiRateLimited` and
`ApiOverloaded`. The `MessagePayload` arm renames `failure_raised` →
`api_request_failed` (tag 4 unchanged).

**Why, in the user's terms.** "Looks good." The concern is the API's own
outcome, and the vendor's error taxonomy is a documented closed set
(400/401/403/404/413/429/500/529 with named types), so it is arms plus
`unmodeled`, not two prose strings. The zero-sentinel `retry_in_ms` becomes
presence, confined by adjacent-exclusivity to the two arms it applies to.
Recoverability stays the daemon's judgement.

**Consequences.**

- Every consumer of `FailureRaised`/`failure_raised` (daemon translate,
  webapp decoder, elisp) renames and switches on `kind`.
- To verify at implementation: whether the producer sees the error TYPE
  structured (SDK error object) or only the CLI's `"API Error: 429 …"` text.
  If only text, the arm is derived from the status code that text carries,
  and the wave records that derivation as the producer's, once.

### `conversation.v1/context_cut.proto`: `ContextTokenDelta` shared by both cuts

**What changed.** New `ContextTokenDelta { int64 tokens_before; int64
tokens_after }`. `ContextCleared` (was empty) gains `ContextTokenDelta tokens
= 1`; `ContextCompacted` loses its two bare `int64`s and gains
`ContextTokenDelta tokens = 2` beside `summary`. `ContextCut` unchanged.

**Why, in the user's terms.** "Clearing context doesn't mean context goes to
zero — there's still system prompt and skills and whatnot that get reloaded
into context." So a clear has a before/after exactly as a compaction does,
and one message carries it for both. This is a duplicated-VALUE-per-arm case
made into a shared TYPE within the namespace — allowed, because it is one
fact (a size change) with one canonical form, not two arms re-spelling each
other.

**Consequences.**

- The producer must observe the post-clear context size. If the vendor does
  not report it on a clear, `tokens_after` cannot be filled honestly; the
  implementation wave verifies what the CLI writes on `/clear` before the
  field is populated, and surfaces a gap rather than writing zero.
- Consumers reading `ContextCompacted.tokens_before/after` read
  `.tokens.tokens_before/after`.
- Naming: "delta" carries endpoints, not a difference; the comment says why.

### `conversation.v1/content_blocks.proto`: `ImageBlock` location as two arms; `UnsupportedBlock` is not a fallback

**What changed.** `ImageBlock.source` (a string that was "a path or URL")
becomes `oneof location { ImageBlockPath path { path }; ImageBlockUrl url
{ url } }`; `media_type` renumbers to 3. `TextBlock` unchanged.
`UnsupportedBlock`'s shape is unchanged; its message comment now says, at the
user's instruction, that it is NOT A FALLBACK: populated only for a block
whose shape is genuinely, realistically unknowable in schema, never because
modeling was inconvenient, the shape varies, or "we'll type it later" — a
recognizable kind found in it is a producer defect.

**Why, in the user's terms.** "Looks good, but update the docstring for
UnsupportedBlock to inform readers/users that this block should only be
populated by messages that are truly not realistically knowable in schema,
and not as a lazy fallback."

**Untyped field, ACCEPTED as a cost.** `UnsupportedBlock.raw` (`Struct`) —
the one qualifying reason: the producer holds no schema for a block kind it
has never seen; nothing renders from it. Accepted by the user explicitly in
the same breath as the docstring instruction.

**Consequences.**

- Consumers reading `ImageBlock.source` switch on the arm; a renderer that
  sniffed `://` to choose between `<img src>` and a file fetch reads the arm.
- The sidecar's/shim's converters must NOT route a knowable block here; the
  implementation wave audits every `UnsupportedBlock` construction site
  against the vendor's block kinds and models what is knowable (the vendor's
  document/PDF block is the likely first candidate).

### `conversation.v1/user.proto`: shapes unchanged, `UserSaid` comment states its two readings

**What changed.** No shape change. `UserSaid`'s comment now states (a) the
nested-prompt reading — under a `DetachedAgent` container it is the spawning
agent's prompt, read from the parent chain, which `MessageAuthor`'s deletion
made implicit; and (b) that a session command is NOT a `UserSaid`, because
the daemon recognizes one before forwarding and it earns no user message
(`session_command.proto:66`), whereas a custom command/skill expands into a
prompt and is one.

**Why, in the user's terms.** Approved. The user asked the UX reason for
`UserContent` being a repeated block list rather than text + images: a person
interleaves words and pictures, the vendor's user message is an ordered block
array, and the feed draws it in composed order — one text + repeated images
would lose placement and force one text run.

**Consequences.** None new. Considered and NOT proposed: a `UserSaid` arm for
a session command — never a record by `session_command.proto`'s own rule;
`session_command.proto` stays a leaf and is judged at its own turn.

### `conversation.v1/agent.proto`: `ThinkingBlock` as two arms, `StopRefusal` added, comments repaired

**What changed.**

- `ThinkingBlock` loses `string text` + `bool redacted`; it gains
  `oneof thinking { ThinkingBlockShown shown { text }; ThinkingBlockRedacted
  redacted {} }`.
- `StopReason` gains `StopRefusal refusal = 5`; `unsupported` renumbers to 6.
  `StopInterrupted` is KEPT, with a note written on the arm that no producer
  is yet verified to observe it as a stop reason (it may only exist as a
  user-role "[Request interrupted]" record).
- `ContentArriving`: shape unchanged; `block_index` is stated as the
  zero-based NODE index into `AgentContent.blocks`; the "tool arguments are a
  typed Struct" sentence now says they arrive typed as `ToolCallBlock.call`;
  the dead `DESIGN-protobuf-surfaces.md` pointer now names the superseded
  file.
- `AgentSaid`, `AgentContent`, `AgentContentBlock` unchanged.

**Why, in the user's terms.** Approved as sketched. Two questions were asked
and closed on the way: (1) `AgentStopped` as its own payload arm instead of
`StopReason` on `AgentSaid` — NO, every settled response has both content and
a stop reason (one turn is several `AgentSaid`, each with its own
`stop_reason`), and turn-level "the agent stopped" is `shim.v1` bookkeeping;
(2) `content` and `stop_reason` as a oneof — NO, they always coexist:
`stop_reason` says how the content ENDED (tool_call with blocks present,
max_tokens with truncated blocks, refusal with empty blocks), and a oneof
would make the ordinary tool-call response unrepresentable.

**Consequences.**

- Every consumer reading `ThinkingBlock.text`/`.redacted` switches on the
  arm; a renderer that showed "reasoning hidden" for `redacted=true` reads
  `redacted` presence instead.
- `StopRefusal` is a new arm every consumer's stop switch must handle
  (compile-surfaced). `stop_sequence` and `pause_turn` deliberately remain
  `unsupported`.
- The `StopInterrupted` question is owed an answer at the shim's turn
  (`shim.v1`, stage 4) or by the implementation wave; if no producer sets it,
  the arm is deleted then, not silently kept.

### `conversation.v1/tool_call.proto`: typed tool arms, `ToolCallId`, outcome and scope arms

**What changed.**

- `ToolCallId { string value }` is new: the typed identity of a tool call,
  the same remedy as `MessageId`. `ToolCallBlock.tool_call_id` and
  `ToolReturned.tool_call_id` embed it. `DetachedWorkStarted.origin_tool_call_id`
  (still a `string`) converts at `detached_work.proto`'s turn.
- `ToolCallBlock` loses `string tool_name` and `google.protobuf.Struct
  arguments`; it gains `oneof call` with fourteen arms — thirteen typed tools
  (`bash`, `read`, `write`, `edit`, `grep`, `glob`, `agent`, `workflow`,
  `skill`, `send_message`, `task_create`, `task_update`, `task_stop`) and
  `unmodeled { string tool_name; Struct arguments }`. THE ARM IS THE TOOL;
  a name beside a typed arm would be a second spelling.
- `ToolCallGrep.output` is a oneof (`GrepOutputContent { line_numbers,
  context_* }`, `GrepOutputFilesWithMatches {}`, `GrepOutputCount {}`), not
  an enum: line numbers and context exist only in content mode.
- `TaskStatus` is a oneof of four empty arms; `ToolCallTaskUpdate` uses it as
  `optional`, with every other field `optional` because an update carries
  only what changed.
- `ToolReturned` loses `bool is_error` and `content`; it gains `oneof outcome`
  with `ToolReturnedSucceeded { content }` and `ToolReturnedFailed { content }`.
- `PermissionAllowed` loses `bool for_session`; it gains `oneof scope` with
  `PermissionAllowedOnce {}` and `PermissionAllowedForSession {}`.
- `PermissionAsked` unchanged; its comment now states WHY it carries the call
  whole (stream-plane timing).

**Why, in the user's terms.** "Toolcalls look good." The typing removes a
client deriving from an untyped blob: the webapp branched on `tool_name` in
eight places (`render.ts:1691-1740`, `async-stream.ts:146-190`,
`permission-preview.ts:33-45`, `stream-member.ts:94`) to dig `command`,
`file_path`, `pattern`, `summary`, `skill`, `status` out of a Struct by string
key. The thirteen typed tools are exactly the tools those sites branch on. The
grep oneof came from the user's heuristic, stated during this increment: if
any value of an enum corresponds to adjacent information exclusive to that
value, that is a strong (sufficient, not necessary) indicator the enum should
be a oneof with the exclusive information confined to the arm.

**Two untyped fields, each ACCEPTED as a cost by its own selection.**

- `ToolCallUnmodeled.arguments` (`Struct`) — the producer holds no schema for
  a tool an MCP server registered at runtime; nothing branches on a key
  inside it.
- `ToolCallWorkflow.args` (`Value`) — arbitrary user-authored JSON only the
  workflow script reads; the producer cannot know its shape and nothing
  renders from it.

**Consequences.**

- The thirteen arms' field sets are the vendor's public tool schemas AS
  KNOWN, not read off the shim: the comment on `ToolCallBlock` says so, and
  the implementation wave VERIFIES each arm against real transcripts before
  the shim converts into it. A vendor field no arm carries is not lost — the
  store's internal half retains the source record when conversion dropped
  structure (superseded record, "A record may be partially convertible").
- `conversation.v1` is no longer vendor-TOOL-neutral: it names Claude Code's
  built-in tools. It remains vendor-CONTENT-neutral (the block model). This
  was weighed against keeping `Struct` and having the daemon resolve a typed
  per-tool card in `frontend.v1`; the user chose typing at the record.
- The shim's converter grows a per-tool switch; adding a tool later is adding
  an arm, and an unhandled arm is a compile-surfaced gap in every consumer.
- Every consumer of `tool_name` — the webapp's eight sites, the daemon's
  async classification (`Agent`/`Task`/`Workflow` by name), permission
  preview — reads the arm instead. `Task` (the older name for `Agent`) maps
  onto the `agent` arm at conversion; the wire does not carry the alias.
- The `StopInterrupted` question stays open for `agent.proto`: whether any
  producer observes `interrupted` as a stop reason.
- Open, NOT modeled: an allow carrying the user's EDITED input
  (`updatedInput`). Whether the transcript preserves it is unverified.

### `conversation.v1` is one file per concern: `message.proto` keeps the record and the arm oneof, the arm bodies move to dedicated files

**What changed.** The regrouped `message.proto` was split along its section
banners, every message VERBATIM, into:

- `message.proto` — `MessageId`, `MessageEntry`, `MessagePayload` (the record
  and WHAT it can say); imports every body file below.
- `tool_call.proto` — `ToolCallBlock`, `ToolResultContent`,
  `ToolResultContentBlock`, `ToolReturned`, `PermissionAsked`,
  `PermissionAnswered`, `PermissionAllowed`, `PermissionDenied`,
  `PermissionAbandoned`.
- `agent.proto` — `AgentSaid`, `AgentContent`, `AgentContentBlock`,
  `ThinkingBlock`, `StopReason` and its five arms, `ContentArriving`.
- `user.proto` — `UserSaid`, `UserContent`, `UserContentBlock`.
- `content_blocks.proto` — `TextBlock`, `ImageBlock`, `UnsupportedBlock`, and
  the "neutral by design" / "narrowed per site" preamble.
- `context_cut.proto` — `ContextCut`, `ContextCleared`, `ContextCompacted`.
- `api.proto` (first `failure.proto`, renamed) — `FailureRaised`.
- `detached_work.proto` — all twenty-one detached-work messages.
- `tokens.proto`, `session_command.proto` — untouched.

`frontend/v1/feed.proto` now imports the four body files it actually
references (`agent`, `detached_work`, `tool_call`, `user`)
instead of `message.proto`, which it did not use. Every package except the
deliberately empty `agentrepl.v1` compiles.

**Why, in the user's terms.** "The message.proto that contains the message +
payload arm, but the arm implementations themselves are in dedicated files —
so all the tool-related stuff is in toolcall.proto, etc." Grouping by concern
at FILE granularity, not just section granularity: a concern is one file, one
review, one import for whoever needs only that concern.

**Consequences.**

- The intra-package import graph is now: `message` → every body file;
  `agent` → `content_blocks`, `tokens`, `tool_call` (an
  `AgentContentBlock` holds a `ToolCallBlock`); `context_cut` →
  `agent` (a compaction summary is `AgentContent`); `tool_call` and
  `user` → `content_blocks`; `detached_work` and `failure` → nothing
  yet (`detached_work` will import `tool_call` once `origin_tool_call_id`
  becomes a `ToolCallId`). Top-down order within stage 1 is therefore
  `message` → `tool_call` → `agent` → `user` →
  `content_blocks` → `context_cut` → `api` → `detached_work` → `tokens` →
  `session_command`, walked one file per increment.
- File name is `tool_call.proto` (snake_case, matching `session_command.proto`),
  not the `toolcall.proto` the user typed; the user may rename. The user DID
  rename the other two: `agent_response.proto` → `agent.proto` and
  `user_message.proto` → `user.proto`, and `failure.proto` → `api.proto` (the names
  above are the renamed ones). `api.proto`'s concern is the vendor API's OWN
  outcomes — the actor is the API, not agent/user/tool — which is why
  `FailureRaised` did not fold into `agent.proto`: the agent said nothing.
  Open for `tokens.proto`'s turn: `TokenUsage` is also an API fact, so
  whether `tokens.proto` folds into `api.proto`.
- A consumer that imported `content.proto` or `payloads.proto` for one type
  now imports the concern file that owns it — narrower, and the compiler says
  which.

### `content.proto` folds into `message.proto` too, and the file is regrouped by concern

**What changed.** `content.proto` is DELETED; its eleven messages moved
VERBATIM into `message.proto`, which now imports `tokens.proto` and
`google/protobuf/struct.proto` directly. `frontend/v1/feed.proto`'s import of
`content.proto` was removed (it already imports `message.proto`). The whole
file was then REORDERED into contiguous sections, each under a banner comment,
with every message's text and leading comment unchanged: THE RECORD
(`MessageId`, `MessageEntry`, `MessagePayload`) → TOOL CALLS (`ToolCallBlock`,
`ToolResultContent`, `ToolResultContentBlock`, `ToolReturned`, the four
`Permission*`) → THE AGENT'S RESPONSE (`AgentSaid`, `AgentContent`,
`AgentContentBlock`, `ThinkingBlock`, `StopReason` and its five arms,
`ContentArriving`) → THE USER'S MESSAGE (`UserSaid`, `UserContent`,
`UserContentBlock`) → BLOCKS COMMON TO EVERY AUTHOR (`TextBlock`,
`ImageBlock`, `UnsupportedBlock`) → CUTS AND FAILURES (`ContextCut` and its
arms, `FailureRaised`) → DETACHED WORK (all twenty-one). The two deleted file
headers ("neutral by design", "narrowed per site") and the floating "a tool
returning is NOT a block" comment were carried into the new file header and
the tool-call section banner respectively; nothing else was dropped.

**Why, in the user's terms.** A holistic view: "I can't know if
`ToolResultContent` is the right message to use because we don't have it
visible in this file and thus you haven't grouped them together for me to
see." Grouping by concern rather than by oneof-arm order is the convention
being added to the skill by the second one-shot workspace
(`proto-group-by-concern`); this is its first application. Sketching now walks
the file SECTION BY SECTION — tool calls, then the agent's response, then the
user, common blocks, cuts and failures, detached work — one concern per
increment.

**Consequences.**

- `conversation.v1` is now three files: `message.proto` (the record model
  whole, 730 lines), `tokens.proto`, `session_command.proto`. Whether
  `tokens.proto` also folds is not decided; it comes up in its own turn.
- The regrouping is a pure reorder — no message changed shape, `protoc`
  compiles the package — but it is a real diff and any binding regeneration
  will churn declaration order in generated code. Harmless; noted.
- The tool-call section sketched before the fold (`ToolCallId`,
  `ToolReturned` outcome arms, `PermissionAllowed` scope arms) is
  re-presented WITH `ToolCallBlock`, `ToolResultContent` and
  `ToolResultContentBlock` in view, which is what the user asked for.

### `conversation.v1/message.proto`: a typed `MessageId`, `MessageAuthor` deleted, and `payloads.proto` folded in

**What changed.**

- `MessageId { string value }` is new — the typed identity of a message.
  `MessageEntry.message_id` and `MessageEntry.parent_message_id` are now
  `MessageId`, and the `optional` on the parent is gone because message-typed
  presence is native and an empty identity is unrepresentable.
- `MessageAuthor`, `AuthorUser`, `AuthorAgent`, `AuthorDetachedAgent` and
  `MessageEntry.author` are DELETED. `MessageEntry` renumbers contiguously to
  `message_id = 1`, `parent_message_id = 2`, `payload = 3`.
- `payloads.proto` is DELETED and its 413 lines of arm bodies (`UserSaid`
  through `ContentArriving`) moved VERBATIM into `message.proto` below
  `MessagePayload`. `message.proto` now imports `content.proto` and
  `tokens.proto` directly. `frontend/v1/feed.proto`'s import of
  `payloads.proto` was repointed at `message.proto` — the one edit outside the
  file, mechanical, so the fold does not leave a dangling import.
- The `MessagePayload` ARM SET (13 arms) is confirmed unchanged at this level:
  names and purposes only; the bodies' shapes are the next increment.

**Why, in the user's terms.**

- `MessageId`: the superseded record already named a bare `string message_id`
  on another surface an identity re-spelling — "can be assigned any string at
  all". A re-spelling can only be remedied by importing the owner's type, and
  the owner had no type. Now it does; every surface embeds it.
- `MessageAuthor`: the field carried the user's own `FIXME: is this actually
  useful?`, and it is not. Author is fully derivable from payload arm plus
  parent chain (an `AgentSaid` under a detached-agent container is the
  subagent; at top level it is the agent; `ToolReturned` always updates the
  agent's response). Two spellings of one fact, exactly what dropping
  `top_level_message_id` removed before. Answer 5 of five: already
  represented. `AuthorDetachedAgent.detached_work_message_id` was the parent
  chain restated.
- The fold: the user asked for `MessagePayload` "in the same file"; on
  clarification, that meant the whole payload model — record, what it can say,
  and what each thing it says looks like — reads as ONE file. The
  `MessagePayload` extraction itself survives (a oneof is not a type;
  extraction is what lets another surface embed the payload set with its own
  stamps).

**Consequences.**

- Every consumer holding a message id as `string` — daemon, webapp, elisp,
  shim, sidecar, store, and every other proto surface (`shim.v1`, `store.v1`,
  `frontend.v1`, `state.v1`) — now has a typed field to embed and a bare
  scalar to retire. Those surfaces are walked later in this sequence; each
  will meet `MessageId` as an existing type. The `tool_call_id` and
  `origin_tool_call_id` scalars are NOT touched here: they are vendor-minted
  correlation values, and whether they get the same treatment is a
  `payloads`-shape question next.
- The old `ToolReturned` arm comment argued from `author` ("author stays the
  AGENT on this record"); with the field gone the comment was rewritten to
  state the same fact without it. That is the one comment edited outside the
  agreed sketch, and it is recorded here because it is.
- `content.proto:123` still says "It is `ToolReturned` in payloads.proto";
  the file no longer exists. Left for `content.proto`'s own turn.
- The store's parent-chain walk at ingest and the daemon's lineage audit read
  a `string`; both now read a `MessageId.value`. Implementation-wave work,
  not contract.

**Verified.** `protoc` compiles `conversation/v1/*.proto` after the fold.
Nothing outside `conversation.v1` referenced `MessageAuthor` or its arms.

### `agentrepl.v1` starts from a clean slate: every RPC and every `endpoint_*.proto` is deleted

**What changed.** All 34 `endpoint_*.proto` files under `src/agentrepl/v1/`
are deleted, and `service AgentRepl` is emptied to `{}`. `service.proto`
survives as the file the RPCs will be re-added to, one at a time, each with
its own `endpoint_<snake_case_method>.proto`. `shared.proto` is NOT deleted:
it is the only `agentrepl.v1` file anything outside the package imports
(`frontend/v1/footer.proto` reads it for `MergeStatus`, `MergeDequeueOffer`,
`HibernationDetail`), and its fate is decided at the `frontend.v1` footer step
and the `agentrepl.v1` conventions step, not by this deletion.

**Why, in the user's terms.** We are going to end up nuking a lot of
`agentrepl.v1` RPCs and their `endpoint_*` files anyway; what is there now is
so far removed from what we want to land that it is better to start from
scratch than to confuse ourselves with preexisting junk.

**Consequences, stated so they are not silently lost.**

- The old `service.proto` header carried normative prose that is NOT
  automatically carried forward: the "no paint attestation on this service"
  invariant, the `request_id`/`workspace`/`client_id` envelope-field
  semantics, and the protojson-on-the-wire note. Each re-enters at the
  `agentrepl.v1` conventions sub-stage as its own question. None is settled
  by having once been written.
- The 34 deleted files were the ONLY spelling of the per-method error arms
  (`Refusal*` messages derived from daemon handlers, per the superseded
  record's "Per-method errors, DERIVED not invented"). That derivation
  evidence — which handler emits which refusal — is in the superseded
  record's prose and in git history (`cfc849d60^`), not on disk. When an
  endpoint is re-added, its error arms are re-derived, and the old file is
  reference material, not a template.
- The build was already broken by the transport reversal; this widens the
  break to every daemon, webapp and elisp site that named a request or
  response type. That is intended, per the land-whether-or-not-it-breaks rule.
- Ten deleted endpoint files carried the comment "see
  DESIGN-protobuf-surfaces.md for why a directory is not available here" (the
  `--go_opt=paths=source_relative` argument for the `endpoint_` prefix). The
  argument still holds and lives in the superseded record; re-added files
  cite the new record.

### ORCHESTRATOR REVIEW of the SIMPLE-ADD wave: 12 new-support groups reverted to the deferred doc

The subagent-landed SIMPLE-ADD wave (c7814df86) was audited whole by the
orchestrator against four questions the user set: validity, complexity,
deletions, and prescribed frontend propagation. Findings: zero deletions
(every removed line was a comment or an empty message body gaining a field);
four validity defects; nine unexpectedly complex shapes; and — decisive —
almost nothing in the wave had a frontend UI change prescribed.

The user's ruling: anything that is NEW feature/support goes to the deferred
metadocument; only additions that service ALREADY-LANDED surfaces stay.
Reverted (tags retired at each site, full semantics preserved in c7814df86
and summarized in `figma-to-idl-redesign.deferred.md` under "SIMPLE-ADD wave
groups reverted at orchestrator review"): the vendor handshake block, run
accounting, workflow phases/progress/totals, prompt provenance, model
fallback, token fallback credit + cache-miss diagnostics, MCP permission
policy, server-side context edits, refusal detail, auth status, the sixteen
vendor-session-event SessionUpdate arms (tags 8-23; context_budget_warning
24 KEPT), the non-text read extents and the attached-file block.

Kept because they complete landed surfaces: the sixteen turn-stop arms (the
two stop-hook arms remain UNDER SCRUTINY against the stop-hook ruling), the
AgentToolFailure payload and settle instants, the API error classes (feed
arms already landed), MCP health arms (panel row still owes a `disabled`
arm), fast-mode cooldown, task `deleted` status, bash
termination/interrupt-cause/spill, detached output path, compaction detail,
`errors` on AgentFailure, AgentBackgrounded, AgentInterrupted cause,
subagent additions, model capabilities + effort, grep facets, task DAG
edges, unmodeled mcp_server, thinking estimate, sandbox arms, FileBlock's
UserContentBlock sibling arms, and SessionRuntime.agent_binary_version.

**Consequences, stated so they are not silently lost.**

- The validity defects found on DEFERRED shapes travel with them
  (SessionAuthStatus adjacent-exclusivity; ModelUsage bare uint64 ceilings;
  the AgentRefusalCategory enum-of-unsettled-vocabulary) and must be fixed
  if the shape returns — the deferred doc records each.
- Two KEPT defects are still owed before design-complete: the stop-hook arm
  decision, and the MCP panel's missing `disabled` row arm.
- Every kept group still needs its frontend propagation prescribed; that is
  the next review pass, group by group.

### `frontend/v1/feed.proto`: `max_output_tokens` joins the turn-error taxonomy

`ApiRequestFailed.max_output_tokens` (the request asked for more output than
the model will produce) landed in the SIMPLE-ADD wave beside billing_error
and oauth_org_not_allowed, but only those two got feed arms. The third now
has one: `FeedTurnErrorMaxOutputTokens max_output_tokens = 17`, empty, the
vendor's wording on the envelope's message element. DISTINCT from
`max_tokens` (11): that response was cut at the ceiling mid-arrival, this
request was refused outright and retrying it unchanged cannot succeed.

### `frontend/v1/feed.proto`: the settled tool card gets its frozen clock

The SIMPLE-ADD wave's `AgentActivitySettledAt` (kept at review: it completes
the landed tool cards) now has its drawn half: `FeedToolCallReturned.runtime`
(9), a daemon-composed sentence ("ran 4.2 s") from the call's start and
settle instants, drawn beside the ok/err badge. UNSET when either instant is
missing — the card shows no elapsed figure rather than a ticking or invented
one. The failure text itself needs NO new shape: `FeedToolCallReturned.failed`
already draws the error content through the same output forms, which is
exactly where `AgentToolFailure.content` resolves.

### `frontend/v1/mcp_panel.proto`: the `disabled` badge completes the health projection

`SessionMcpServer` (conversation) carries connected | failed | needs_auth |
pending | disabled; the /mcp panel row drew only the first four, so a
configured-but-switched-off server had no honest rendering. `McpPanelDisabled
disabled = 6` closes it — empty, the arm is the badge, WEBAPP owns the
treatment.

### RULING UPHELD BY EVIDENCE: the stop-hook arms stay on `AgentFailure`

The wave-review scrutiny of `stop_hook_prevented` and `hook_stopped` is
resolved in their favor, at the DECLARED-TYPES tier: `TerminalReason`
(sdk.d.ts:6909, "Why the query loop terminated") names both as first-class
loop terminals, and `SyncHookJSONOutput.continue?: boolean` +
`stopReason?: string` is the hook contract that ends the loop. Stop hooks DO
end the turn at the SDK level; the earlier "stop hooks never determine the
turn terminal" ruling concerned DAEMON-synthesized terminals and is not
contradicted — these arms relay the vendor's own stated terminal, never a
shim invention. Both arms ride the existing feed error route like every
other turn-stop arm.

### RENDERING PRESCRIPTIONS for the kept wave completions

Each kept SIMPLE-ADD group that completes a landed surface now has its
frontend disposition, so the reconciliation pass inherits decisions rather
than questions:

- Tool failures: `AgentToolFailure.content` resolves through the EXISTING
  `FeedToolCallReturned.failed` verdict + output forms (the card already
  says "a failed call's output is its error text"); no new shape.
- Settle instants: drawn as the `FeedToolCallReturned.runtime` composed
  clock (landed above); the shell bubble and footer rows keep their own
  clocks and are unaffected.
- Task `deleted`: NO footer arm. The checklist is a whole-list push, so a
  deleted task is prescribed as ROW OMISSION — the daemon drops the row and
  the next push carries the tracker without it.
- Bash termination (detached): ALREADY MAPPED — the shell bubble's
  terminator carries the exit code; `AgentBashKilled` draws as the bubble's
  existing ended-without-status treatment.
- Bash interrupt cause (foreground): daemon composes a trailing line in the
  card's text output ("[stopped by user]" / "[timed out after 120 s]"); the
  cause needs no arm on the card because the treatment is one composed line.
- Bash spill: the daemon appends the spill path to the composed
  `FeedToolCallOmitted` sentence ("1.2 MB more not shown · full output at
  /path"); host-local path, shown not linked.
- Fast-mode cooldown: conversation-complete, frontend EXPECTED UNMAPPED —
  no frontend surface draws fast mode at all today, so the cooldown arm
  waits with the rest of the fast-mode vocabulary.
- Detached output path (`DetachedWorkOutput`): frontend EXPECTED UNMAPPED
  for now — the shell bubble already shows the spool's CONTENT; the
  host-local path adds nothing drawable until a "open the spool" affordance
  exists, which would be a new feature.

### DESIGN-COMPLETE GATE PASSED (2026-08-27)

The user approved the consequences recap whole: six surfaces settled at
b24ced959 (plus vetting item 12 added at the gate), breaking consequences
accepted under the land-whether-or-not-it-breaks rule, the exempt set /
fidelity principle / identity ruling standing, debts routed to the vetting
register (items 1-12, run-or-defer next) and the deferred metadocument.
Design iteration is CLOSED; changes from here are vetting verdicts landing
as increments, then reconciliation, then /cross-system-fanout.

### VETTING VERDICTS LAND (2026-08-27): six deletions/additions from the run pass; /cost and /usage leave the daemon

The eleven vetting runs concluded with the contract overwhelmingly upheld;
the user's verdicts on the raised consequences landed as seven commits:

- `AgentQuestionNote` DELETED (tag 4 retired): item 6 proved the wire
  collapses a selection's typed text into one comma-joined answer string —
  no producer can fill a distinct note.
- `AgentPermissionAbandoned` DELETED (tag 3 retired): the vendor declares
  permission prompts have no park deadline.
- Unsourced fields DROPPED: `ContextCompacted.cumulative_dropped_tokens` +
  `.tools_before_cut`, `ContextCleared.tokens`,
  `SessionIdentityRotated.reason` (tags retired at each site).
- `AGENT_EFFORT_LEVEL_MAX = 5` added — the vendor-declared level the enum
  missed.
- /cost AND /usage LEAVE the daemon-handled command set (panel files
  deleted, SubmitPromptCommandPanel tags 2-3 retired): their only
  structured sources are an explicitly EXPERIMENTAL method and an
  undeclared-shape one — "not worth the added complication"; the commands
  fall through to the vendor. /context, /todos, /agents, /mcp, /help,
  /status stay.
- FOOTER coverage verdicts: `FooterSubStatusInterrupted`
  {by_user | host_shutdown} (finding 5), `FooterSubStatusBlockedBilling`
  (finding 4), the Tag-8-RETIRED note (finding 2), the stale
  no-substructure comment fixed (finding 1). Findings 3 and 6 await the
  user's follow-up questions; finding 12 (the built-ins slate) goes to a
  multi-select.
- Producer notes corrected per item 1: ArtifactOutput is TYPED (read
  fields, never parse prose); grep omitted figures are shim-subtracted;
  hook duration and spawn depth are shim-derived.

### BUILT-INS SLATE RULED (2026-08-27): six families get modeled; MCP-resource family and console misc go exempt

Vetting item 1's unhomed-built-ins slate resolved by multi-select:

- MODELED, with drawn homes the user assigned: EnterPlanMode/ExitPlanMode
  and ReportFindings as FEED RESPONSE BUBBLES (purple, the Response/Artifact
  treatment); EnterWorktree/ExitWorktree in the FEED; CronCreate/Delete/List
  in the EXTENDED FOOTER as a chip + panel, the tasks precedent — never the
  feed.
- MODELED, UI to be designed ("not obvious yet" — drawings owed before
  shapes): PushNotification, REPL.
- EXEMPT (join the exempt set): ListMcpResources/ReadMcpResource/
  RefreshMcpTools, SendFeedback, ClaudeDesign, Projects,
  ShowOnboardingRolePicker, ProposeSkills.

Each modeled family walks the ordinary increment protocol: drawing agreed,
then the encompassing sketch agreed, then landed end to end.

### PLAN MODE lands — a coalesced read-only purple bubble with an Emacs edit affordance

First of the six ruled built-in families, iterated to agreement and landed
on the user's explicit go. `AgentPlanMode` (item arm 28) carries the
vendor's pair faithfully: enter and exit are separate units with their own
identities and NO wire pairing key — the daemon coalesces by the episode
invariant (at most one open plan episode per agent), so `FeedPlan` (unit
arm 9) is ONE purple response-styled bubble upserting planning → planned →
failed on a single FeedId. Decisions reached in iteration:

- The document is RENDERED MARKDOWN (the response bubble's prose
  treatment), never raw text; EnterPlanMode draws NO tool card anywhere.
- The bubble is READ-ONLY; revisions go through the composer. The one
  affordance is the ✎ EDIT BUTTON — present iff the exit named the plan
  file — whose click the WEBAPP RAISES TO THE HOST: the Emacs client opens
  `FeedPlanEditTarget.path` as a doom popup, right side, half width. The
  vendor cannot observe disk edits, so the round-trip is save-then-tell-
  the-agent; the user declined a host-side edited marker.
- `plan_was_edited` (the VENDOR dialog's edit flag) is carried faithfully
  but DORMANT in this UX; file_path is MAPPED (the edit target);
  is_agent/has_task_tool/awaiting_leader_approval are EXPECTED UNMAPPED;
  requestId is excluded by the identity ruling.
- Exit-with-no-enter is legal (a session started in the plan permission
  mode never calls EnterPlanMode); the bubble is then born planned.
- DAEMON PRESCRIPTIONS: the episode closes on exit settle, call failure,
  or the turn's terminal while planning (composed "ended without a plan"
  line into the failed arm); and the episode MUST close before processing
  the `conversation_reset` that plan acceptance can emit.
- The vendor's `get_plan` control verb (path-less read of the current
  plan) is NOT modeled — recorded in the deferred doc as the enabler for a
  future live-updating planning state.

  While planning:                     After the exit:
  ┌───────────────────────────┐  ┌──────────────────────────────┐
  │ ▐ Plan        (purple bg) │  │ ▐ Plan   [✎ edit] (purple bg)│
  │ ▐ ◌ planning…             │  │ ▐ Pagination fix  (markdown) │
  └───────────────────────────┘  │ ▐  1. Cap the re-pull…       │
                                 │ ▐  2. Thread the cursor…     │
                                 └──────────────────────────────┘
  ✎ click → Emacs doom popup, right side, 50% width, on the plan file.

### REPORT FINDINGS lands — the purple defect-list bubble; the shared editor-popup subroutine becomes a stated code-level prescription

Second of the six built-in families, landed on explicit go.
`AgentReportFindings` (item arm 29) carries the tool's typed report:
findings in the tool's own most-severe-first order, per-finding verdict
{confirmed | plausible} and re-report outcome {fixed | skipped |
no_change_needed} as oneofs, category as an open shown-never-switched
string, `level` reusing the canonical AgentEffortLevel; short_summary is
carried EXPECTED UNMAPPED. `FeedFindings` (unit arm 10) is the purple
bubble: composed heading, rows with verdict badge / category chip /
location / summary / folded failure scenario / outcome badge. Decisions
from iteration:

- RICH DECORATION IS THE WEBAPP'S: no glyphs ride the wire — badges and
  chips are typed arms the client styles.
- Every location is a JUMP TARGET, and the user mandated CODE-LEVEL
  consistency with the plan bubble's edit button: ONE shared webapp link
  component, and ONE shared Emacs subroutine ("open path[:line] in a doom
  popup, right side, half width") used by both affordances — a fanout
  implementation requirement, stated here so it survives to the planning
  docs.
- NO client sugar (no per-row fix buttons, no rating): the vendor has no
  findings-feedback verb, and interaction stays conversational — a
  re-report with outcomes is the round-trip the badges draw.

  ┌────────────────────────────────────────────────┐
  │ ▐ Findings · 3 · high              (purple bg) │
  │ ▐ [CONFIRMED] [correctness]                    │
  │ ▐   daemon/server.go:214        (jump target)  │
  │ ▐   The sweep drops held prompts on restart    │
  │ ▐   ▸ failure scenario          (folded)       │
  │ ▐ [PLAUSIBLE] [efficiency]  …                  │
  │ ▐   [fixed]   (outcome badge, re-report only)  │
  └────────────────────────────────────────────────┘

### THE WORKTREE PAIR lands — and the context-cut divider GENERALIZES to `FeedSessionSeparation`

Third of the six built-in families, landed on explicit go, with the user's
structural direction: worktree moves draw as DIVIDERS in the context-cut
theme — same rule, same geometry, SAME RENDERING SUBROUTINE — blue accent,
different label.

- conversation.v1: `AgentWorktree` (item arm 30) — enter and exit as
  faithful tool calls (start/success/failure, typed Enter/ExitWorktree
  fields; requested action vs stated outcome kept apart; message lines and
  tmux name EXPECTED UNMAPPED; discarded counts optional because unstated
  is not zero). NO coalescing: the two moments can be far apart, and each
  settled act is its own divider — everything drawn between them happened
  inside the tree.
- frontend.v1: `FeedContextCut` is RENAMED AND GENERALIZED to
  `FeedSessionSeparation` (row arm 12 respelled `separation`): one divider
  row kind whose `kind` oneof is {cleared, compacted, worktree_entered,
  worktree_left}, label composed by the daemon, `tokens` optional and set
  on the context arms only. The worktree payloads carry path (a jump
  target through the SAME shared editor-popup subroutine — dired for a
  directory), branch, and the kept|removed outcome with a loud composed
  discard line.
- STRUCTURAL INVARIANT, stated in the schema and owed to the fanout's
  /structural-invariants pass: ONE renderer subroutine draws EVERY
  separation arm; an arm selects only accent color and label/payload text.
  A per-arm divider renderer is a defect.
- The conversation.v1 `ContextCut` family is deliberately NOT renamed: at
  that tier a context cut and a worktree tool call are different facts
  (one is a session-shape event, the other a permission-gated tool call
  with unit identity), and only their DRAWN treatment unifies.

     ── ⎇ entered worktree · ~/…/wt/fix-pagination · fix-pagination ──
        (work inside the tree)
     ── ⎇ left worktree · removed · 3 files, 2 commits discarded ──

### CRON lands — the ⏱ chip and a TRUE-LIST panel with a client-ticked countdown; footer-only like tasks

Fourth of the six built-in families, landed on explicit go. conversation.v1
gets `AgentCron` (item arm 31): create/delete/list as faithful acts, the
listed set carried WHOLE (replace semantics), job ids as the vendor's cron
ids carried OPAQUE (they name jobs, not transcript records — the task-id
precedent under the identity ruling). frontend.v1 gets `FooterChipCrons`
(chip 5, count, hidden at zero) and `FooterExpandedCrons` (panel 6): a TRUE
LIST like the agents panel — rows of vendor-composed human schedule,
daemon-truncated prompt, RECURRING and DURABLE markers (durable survives
the session; everything else in the footer dies with it), and the
user-required countdown: the DAEMON resolves the next-fire INSTANT from the
cron expression (accepted complexity) and ships epoch ms; the client ticks
"5m 12s" per the clock convention. No feed cards anywhere — footer-only,
the tasks precedent. Not jump targets: jobs have no bubble.

### PUSH NOTIFICATION lands — the daemon publishes the FACT; each surface applies the policy it alone has knowledge for

Fifth of the six built-in families, landed on explicit go after the model
was corrected in iteration: the user's first sketch had the DAEMON deciding
by Emacs focus state, and the settled model moves policy to where the
knowledge lives — the daemon never asks "is Emacs focused."

- conversation.v1 `AgentPushNotification` (item arm 32): the message, and
  the VENDOR's own delivery outcome as a oneof — sent {push_sent,
  local_sent, sent_at_ms} | not_sent {config_off | user_present |
  no_transport}. The vendor's phone push is independent of our local
  presentation; the input's "proactive" literal is not carried (a constant
  is not a fact).
- agentrepl.v1 `WatchHostWorkspaceResponse` becomes a push oneof: the
  whole-state `host` arm as before, plus the `notification` EVENT arm
  {text, at_ms}. EMACS OWNS PRESENTATION POLICY (stated at the arm):
  unfocused → OS notification whose click raises the frame and selects the
  tab (plain elisp, decider and actor are one process); focused+unselected
  → tab-bar blink; selected → nothing.
- frontend.v1 sidebar: `RosterRow.attention` (32), an empty presence
  marker; the daemon sets it on the notification and clears it on the
  EXISTING SelectWorkspace verb. THE CANONICAL BLINK CADENCE is specified
  ONCE on `RosterRowAttention` (two blinks, 500 ms on/off, then steady) —
  the webapp sidebar and the Emacs tab-bar both implement exactly that spec
  and cite the message; divergence is a defect (user-mandated code-level
  consistency, the editor-popup precedent).
- frontend.v1 footer: `FooterStatusActivityNotification` (arm 11), the
  composed line shown until the next activity replaces it.

### FOOTER ACTIVITY GAINS ITS ENVELOPE INSTANT — and two implementation invariants lock down for the fanout

`FooterStatusActivity.at` (12, message-wrapped, NOT optional): when the
activity began standing, on the ENVELOPE so every arm carries it by
construction — a per-arm spelling would be eleven copies of one fact and
the twelfth arm forgets it. Client ticks the relative age per the clock
convention; a push without it is a loud daemon fault; wakeup's future
deadline stands apart as its own fact.

TWO IMPLEMENTATION INVARIANTS, user-mandated at this gate, inherited
verbatim by the fanout's planning docs:

1. UNSET NON-OPTIONAL FIELDS ARE ILLEGAL, EVERYWHERE, IMMEDIATELY.
   - REQUESTS: a request carrying an unset non-optional field is answered
     with an ERROR to the producer at once — never "handled", never
     defaulted. Integration tests thereby DETECT producer gaps: the
     consumer checks the producer, and the orchestrator remediates.
   - RESPONSES AND STREAM PUSHES: a non-optional response field MUST be
     set; a consumer receiving one unset RAISES A LOUD ERROR itself (on a
     stream there is no producer to answer), sized to be caught during
     integration remediation.

2. LOGGING STANDARDS FOR REMEDIATION.
   - Every logical branch carries a DEBUG statement; warnings log at
     WARNING, errors at ERROR — level discipline is part of review.
   - Integration/e2e orchestration (expected to run at TWO LEVELS given the
     project's size) turns on >=WARNING logging BEFORE tests run and
     PERUSES the logs EVEN WHEN TESTS PASS; any warning found is remediated
     to zero — fixed, or deliberately downgraded (e.g. to info) — never
     left standing.
   - When issues are detected, subsequent remediation runs enable DEBUG
     logging to trace the path.

### IMPLEMENTATION CONVENTIONS lock down: proto→code mapping, validation, logging, and the six-orchestrator protocol

User-settled at the design gate's implementation detours; the fanout's
planning docs inherit ALL of this verbatim, for all five systems, both
directions (producers and consumers).

**Proto→code mapping.**
- Every MESSAGE has one core implementation function ("base") per
  language: validation lives there ONCE — unset non-optional fields and
  empty strings with required semantics are ERRORS; an unset oneof is an
  ERROR BY DEFAULT, a documented fallback only where the schema comment
  explicitly sanctions absence.
- Every NON-PRIMITIVE use site (message-typed field, oneof arm) has its
  own dedicated, TESTABLE function that delegates to the child message's
  base; ancestry-named specializations go one layer deeper ONLY where a
  specific path has real site-specific behavior, still through the base.
- PRIMITIVES get no wrappers.
- The producer side is SYMMETRIC: build functions with the same validation
  at construction, per-site builders on top.
- NO class-per-message mandate: the requirement is dedicated testable
  functions and separated concerns, NOT a shape — organization into
  modules/files/classes is the implementing agents' discretion, settled at
  the orchestration-planning stage; the anti-goal is a million
  unnamespaced Handle<A><B><C> functions.

**Validation invariant (restated from the previous entry, part of this
package):** illegal requests error to the producer immediately; illegal
stream pushes raise loudly at the consumer; integration testing is
expected to CATCH both, and the orchestrator remediates.

**Logging invariant (restated):** debug on every logical branch, correct
levels, >=WARNING enabled and PERUSED by orchestrators even on green
runs, warnings remediated to zero (fixed or deliberately downgraded),
debug enabled during remediation traces.

**The six-orchestrator protocol.** Five per-system orchestrators + one
LEAD.
- Every orchestrator and implementer reads the main design record and
  their system's implementation doc into context BEFORE any planning or
  work.
- Implementers anticipate API edge cases; every anticipated case gets a
  unit test on the corresponding per-site function; when the API is
  unclear for a test, implementers ASK their orchestrator, never guess.
- Implementers NEVER change protobufs. A needed change is a REQUEST to
  their orchestrator, who triages: true systemic oversight → surfaced to
  the lead AND the user; small remediation → to the lead, who vetoes or
  approves.
- On approval: lead broadcasts PAUSE (finish-or-abandon the current edit,
  never mid-file) → lead lands the proto change and rebuilds bindings →
  lead broadcasts RESUME carrying the NEW FOUNDATION COMMIT SHA so every
  system re-points at one identical contract version → orchestrators
  resume their implementers.
- Every proto-change request and its ruling gets a line in the design
  record: mid-flight contract drift stays auditable.

### REPL joins the EXEMPT SET — the built-ins walk is COMPLETE

The user's ruling, superseding the built-ins slate's "modeled, UI to be
designed" disposition for REPL: the sandboxed code-execution tool is not
useful enough to support now. REPL joins the EXEMPT SET — a known vendor
built-in the contract deliberately does not carry: its calls are DROPPED at
the shim, never emitted as AgentUnmodeled, never tripping the topbar's
unmodeled warning. No proto change lands; the earlier ride-the-grey-card
proposal is withdrawn unagreed. If REPL support is ever wanted, it re-enters
as its own increment with a drawing.

All six ruled built-in families are now dispositioned: plan mode,
ReportFindings, the worktree pair, cron, and push notification landed;
REPL exempt.

### The FOOTER STATUS FAMILY restructures — legality by construction; waiting gains permission/question

Findings 3 and 6 of the footer coverage review resolve together in the
user's structural design: the strip's three sibling cells (status,
sub-status, activity) become ONE TREE — each status arm declares exactly
the sub-status steps and activity kinds legal while it stands, so an
illegal pairing (a wakeup countdown under merging) is unrepresentable
rather than forbidden by comment.

- FooterStrip retires tags 2-3; FooterStatus's arms stop being empty and
  each owns its substatus oneof and a per-status activity wrapper whose
  oneof confines the legal kinds; the leaf payload messages (notification,
  wakeup, retrying, …) are SHARED across the per-status oneofs — one drawn
  cell, one payload vocabulary; the oneof types carry the legality.
- The user's sketch used enums for status/substatus; landed as oneofs per
  the standing state-enum prohibition, and the sibling coarse-status enum
  was dropped as a second spelling of the set arm.
- Notification is an ACTIVITY duplicated into every arm's oneof, never a
  status — a status arm would knock the real status off the strip.
- ACTIVITY OPTIONALITY IS EVIDENCE-GATED, the user's rule: required where a
  producer always has a line (waiting, loading), optional elsewhere with
  the WHY stated at each field; a bare optional with no stated absence
  path is a review defect.
- Finding 3 lands inside it: waiting gains permission and question steps
  with typed activities (gated call, question lead); the wakeup fallback
  rule is unchanged.
- The `at` instant rides each per-status activity wrapper (duplication per
  the figma→idl default), keeping the activity-began semantics.
- The rendering rule is stated once on FooterStatus: a status arm with no
  substatus merges that cell into the status cell; activity absorbs free
  width.

Consequences: the daemon's footer resolver builds one tree per push
instead of three cells; the webapp switches per status arm; every consumer
of the old FooterSubStatus/FooterStatusActivity top-level types recompiles
against the per-status wrappers.

### Two footer gaps close (blocked·query_died, waiting·cold_gate); the strip's rendering conventions land in the schema

From the user-requested conversation.v1→footer gap analysis (fast mode,
permission-mode changes, identity rotation and mid-session MCP faults ruled
LEFT UNMAPPED as deliberate non-surfaces):

- blocked gains the query_died step + composed activity line: a dead vendor
  query with a healthy shim previously fell back to bare idle; distinct
  from disconnected, which is the daemon<->shim link.
- waiting gains the cold_gate step + composed cost line: a session parked
  on the cold-context gate previously read plain idle; the feed's gate row
  stays the answering surface — the footer only says the session is parked.
- RENDERING CONVENTIONS stated once on the family banner: status and
  substatus render lowercase ASCII with spaces, never underscores; activity
  is the rich cell — colorized, formatted, ideally one line — and a
  statically typed datum in an activity message signals that datum deserves
  color in the rendered line.

### The three status-independent activities land in EVERY status arm, with a stated precedence ladder

The user's ruling on the orphan-activity finding: notification,
rate_limited and context_budget are status-independent standing facts, so
each appears in EVERY status arm's activity oneof (duplicate-don't-share;
legality confinement now constrains only the truly status-bound kinds).
The daemon selects ONE standing activity per push by precedence, stated
once on the family banner: notification OUTRANKS everything; status-bound
kinds rank next by the daemon's judgment; rate_limited is SECOND-LOWEST;
context_budget is LOWEST (shown only when nothing else stands). The
earlier partial placements (context_budget under idle+thinking only) are
superseded — a warning suppressed by unrepresentability would be silently
dropped, not outranked.

### The PUSH-CADENCE convention is stated once in the schema

The user asked for the update-cadence picture and approved it as the
standing rule, now stated on FooterView (and named as the convention for
every frontend.v1 view): EVENT-DRIVEN, WHOLE-VIEW, NO TICKS — push whole
on any resolved change, push nothing on no change (the keepalive
retraction restated), client-side ticking from shipped instants, bursts
coalescible because the wire carries states, not events.

### DESIGN FREEZES at 2d79f7501; bindings regenerate; the Makefile's proto list goes dynamic

The frozen-contract SHA is 2d79f7501 (the push-cadence landing — the last
design commit). Step 7 opens: the Makefile's hand-maintained PROTOS list —
stale by dozens of deleted files — is replaced with discovery from src/,
so a deleted file leaves the build the moment it leaves disk; Go and TS
bindings are regenerated once from clean for all six packages. Per-
subsystem reconciliation agents dispatch next; the reconciled foundation
SHA is recorded when they merge.

### AMENDMENT to the replacement-coverage prescription: INTEGRATION and E2E specs only — unit gaps are the mapping convention's job

The user's refinement, superseding the earlier prescription's routing of
unit specs: the orchestrator prescribes replacement coverage for
INTEGRATION tests (into each subsystem's implementation planning doc) and
E2E tests (into the main implementation doc) ONLY. Unit-test coverage is
NOT prescribed — it falls out naturally from the landed proto→code mapping
convention, under which every non-primitive use site gets a dedicated
testable function and implementers write unit tests per anticipated edge
case on those functions. Prescribing unit gaps would duplicate that
machinery's output and anchor implementers to a list instead of the
mapping.

### The per-subsystem IMPLEMENTATION PLANNING DOCS open at docs/overhaul/ — dead code is NAMED WORK

The user's ruling: dead code the redesign stranded (reconciliation
deliberately leaves it standing where deleting it would be design work) is
ROUTED TO THE ORCHESTRATORS as named work in each subsystem's
implementation planning document — never left for discovery. docs/
implementation/<subsystem>.md carries each system's dead-code inventory,
its integration replacement specs (unit specs deliberately absent per the
mapping-convention amendment), and reconciliation gotchas; MAIN.md will
carry the e2e specs. Shim and elisp are seeded from their reconciliation
reports; store/sidecar/webapp/daemon follow as their reports land.

### RECONCILIATION MERGES (5 of 6): shim, elisp, webapp, store, sidecar green; Connect Go stubs join the codegen; the daemon is a fanout subject

Five subsystems reconciled green in isolated worktrees and merged (shim
277 tests; elisp 5614; webapp 4901; store all packages race-checked;
sidecar all packages) — each report's dead-code inventory, blockers, and
integration replacement specs seeded into docs/overhaul/. The
DAEMON's agent correctly refused: it was never repointed off protocol.v1/
data.v1/state.v1 (9,724 dangling reference sites, 452/830 files), so
"minimum adaptation" would hollow it into an empty shell — its
re-targeting is fanout implementation, per the record's own routing;
ruling owed. Findings absorbed at the orchestrator: the binding regen had
dropped proto/gen/go's hand-written go.mod/go.sum (restored);
protoc-gen-connect-go joins the Makefile's go target, because
protoc-gen-go emits message types only and NOTHING could serve the three
Connect services — agentreplv1connect/shimv1connect/storev1connect
handler interfaces now generate.

### SubmitPrompt gains its FIRST refusal arm: merge-in-flight is an ERROR, never a hold

Settled during the daemon architecture planning (a sanctioned post-freeze
increment): a prompt arriving AFTER a merge began is REFUSED outright —
once the workspace merges it closes, so post-merge-start work would be
orphaned; holding it would promise a delivery that loses work. Prompts
already held when the merge began stay held (the dequeue offer resolves
their fate). This upholds the old daemon's mergepromptgate refusal at
contract level and lands SubmitPromptError's first derived reason arm
(merging, empty — the footer and merge bubble already show which merge).
Consequence for the daemon architecture: the occupancy-lease projection
gains PER-HOLDER REFUSAL POLICY — the merge lease projects to
error-on-new-submission; restart-pending and shutdown-drain project to
holds.

### THE SESSIONWATCHER settles — the pulls FOLD INTO WatchSession; the five resolvers named, internals free

Settled 2026-08-28, superseding BY NAME the same-walk session-manager
shape, the just-landed GetSessionContextUsage verb, AND the original
"diagnostics are PULLED, not pushed" ruling:

- shim.v1 GetSessionDiagnostics and GetSessionContextUsage are DELETED;
  SessionUpdate gains `diagnostics` (25) and `context_usage` (26) —
  the shim PUSHES both at its own cadence (context usage also at every
  turn end) on the one session stream. Simpler: the daemon has no
  session pulls at all.
- THE SESSIONWATCHER (one per workspace/shim) has exactly three jobs:
  watch the session's streams (turn, detached items, session — opened
  eagerly, the open set IS the live-work set); be the SOLE source of
  truth on session connectivity (the only thing watching the shim);
  route stream responses to the resolvers, fanning out per type. It
  never pulls and never writes — prompt submission is the prompt
  queue's alone, simple synchronous reads may use the shim client
  directly, and ALL async streaming data enters through the
  sessionwatcher.
- THE FIVE RESOLVERS, existence + purpose only: feed, footer, topbar,
  sidebar, hold tray — each converts conversation.v1 items and daemon
  facts into its component's frontend.v1 view; internals deliberately
  unprescribed. RESOLVER STATE IS FINE, unpersisted: resolvers
  accumulate piecemeal frames in memory and ship COMPLETE snapshots —
  the webapp never assembles partial state, because non-optional
  fields are semantically non-optional and a partial push would
  violate them.

### THE SESSION MANAGER settles — one component per live session; eager shim watches, lazy client subscriptions

Settled 2026-08-28 (architecture, no wire change), superseding the
same-day response-handler naming: the daemon-side face of a workspace's
live session is ONE component, THE SESSION MANAGER — the only consumer
of shim output, above the dumb-wire shim client. STREAM HALF: it owns
every shim watch (WatchSession, the turn's WatchAgent, one per live
detached item — opened EAGERLY on announcement, because liveness is
structural and the chips/freeness checks count the open set) and routes
every frame by type through one table to the feed resolver (output
address honored), footer resolver, accounting, and the turn-lifecycle
announcement the prompt queue drains on. QUERY HALF: the shim pulls
(context usage, diagnostics) are SYNCHRONOUS MEMBER FUNCTIONS answering
their caller directly, never entering the routing table. TWO-LEG
DETACHED FLOW: daemon↔shim eager, webapp↔daemon lazy — an expand's
OpenFeed/WatchFeed only SUBSCRIBES to rows already being produced; a
collapse cancels only the client leg; the expand never creates a
shim-side route. Pushing out is the resolvers'/publishers' alone.

### `GetSessionContextUsage` lands — the context fact is PULLED, never derived; the daemon owns account switching

Settled 2026-08-28 (a sanctioned post-freeze increment, completing the
topbar landing):

- NEW conversation/v1 `SessionContextUsage { total_tokens; max_tokens;
  repeated SessionContextCategory { label; tokens } }` — the vendor's own
  get_context_usage answer as a conversation fact; the topbar chip and
  the /context panel BOTH resolve from this one fact.
- NEW shim.v1 `GetSessionContextUsage` (SESSION section), pulled at the
  daemon's cadence plus every turn end (the GetSessionDiagnostics
  pattern). The user's ruling: CORRECT, not "derived" — the
  usage-frame derivation is forbidden as the chip's source. This also
  un-strands the /context panel, whose specced fill previously had no
  route.
- THE ACCOUNT IS DETERMINED, NEVER SELECTED (invariant): the
  repo-under-root rule is the account's ONLY source — CreateWorkspace
  deliberately carries NO account field, the old create-time explicit
  selection dies, and a differently-accounted workspace is structurally
  unrepresentable. This also discharges the create-time-selection gap.
- ACCOUNT SWITCHING, constraints not mechanics: the DAEMON determines
  the config dir (repo under $MULTI_REPO_ROOT → multi-repo dir, else
  default) and the DAEMON ports the vendor transcript between roots on a
  switch (a file move before the ordinary resume) — a shim-owned port
  was sketched and REVERSED as needless complication; the shim never
  participates.

### The TOPBAR gains its ACCOUNT element and the CONTEXT CHIP; the strip's layout and reveal conventions land

Settled 2026-08-28 (a sanctioned post-freeze increment), from the old-vs-new
topbar comparison:

- LAYOUT: one THIN strip — left, tight: account label + connectivity dot;
  center: the title with ALL free width flexing around it (the flank
  groups never spread); right, tight, in order: model selector, context
  chip, warning chip at the far edge. REVEAL CONVENTION: every element's
  detail renders BELOW the strip and CLAMPS within the viewport — never
  off-screen (the old implementation's stated failing).
- ACCOUNT (TopbarView.account = 8): which account the session SPENDS AS,
  daemon-resolved from the session's config root's login — the old
  webapp's account.ts feature (it rode an HTTP side-channel) restored as
  contract. THE ARM IS THE STATE: logged_in{email} | logged_out{}, the
  logged-out label drawn AS the warning (a logged-out root cannot run a
  turn; blank reads as loading).
- THE CONTEXT CHIP (TopbarView.context = 7; tag 6 token_breakdown
  RETIRED): the chip is a NUMBER, not an icon — the CURRENT CONTEXT SIZE,
  rendered YELLOW, shrinking on compaction/clear and never exceeding the
  model's window (a producer fact). Hover reveals TokenBreakdownView,
  now hardened SESSION-SCOPED ONLY: no turn section may ever appear —
  turn figures are the FOOTER's domain exclusively.
- ARCH TODO (daemon): the CONTEXT-SIZE PRODUCER — the figure is not
  natively on the conversation.v1 stream; candidates are the vendor's
  get_context_usage control verb (verified stable/typed at vetting) and
  the separation rows' after-tokens; the mechanism is implementation-wave
  work, the requirement stands now.
- STILL OPEN: the create-time ACCOUNT SELECTION half (an account option
  on CreateWorkspace + served options) and the account-switch-without-
  losing-transcripts mechanics.

### TWO MERGE METHODS — Emacs-repo vs everything else; the SEVEN-TAB vocabulary; the landing is a NO-FF MERGE COMMIT; MULTI_REPO_ROOT is account-only

Settled 2026-08-28 (a sanctioned post-freeze increment), superseding BY
NAME the six-tab family of the sub-feed landing and the merge-variants
entry's "landing split computed from MULTI_REPO_ROOT":

- MULTI_REPO_ROOT HAS NOTHING TO DO WITH MERGE STRATEGY — it is ACCOUNT
  SELECTION only, carried forward exactly as the current system supports
  it (config-dir routing by path, transcript-root probing, create-time
  resolution with the no-inheritance asymmetry, same-uuid-two-accounts
  disambiguation, the two-root roster).
- TWO METHODS, keyed self-repo-or-not (the git common-dir identity the
  self-reload check already computes). EMACS REPO: pre-prompt (lease) →
  NO-FF MERGE COMMIT onto the default branch (one commit to apply, one
  to revert — not cherry-pick, not rebase) with conflicts handled via
  the parked-lease spec → tests on the merge commit + fixes (lease) →
  the rollout bounce → post-prompt (lease). EVERYTHING ELSE: pre-prompt
  → post-prompt, nothing more — PR creation, landing, tests are the
  prompts' job there, for now.
- THE SEVEN TABS (FeedMergeTab.kind revised; landing/action/rebase/
  remediation kinds die): queue | pre_prompt | merge | conflicts |
  tests | fixes | post_prompt. RESOLVED: queue, merge (the commit's
  thin git narration lines), tests. AGENTIC: pre_prompt, conflicts,
  fixes, post_prompt. ALL CONDITIONAL structurally: no conflicts → no
  conflicts tab, no failures → no fixes, unconfigured prompt → no tab.
  PARKED exists ONLY on conflicts and fixes (the two give-up-to-human
  loops); the prompt tabs cannot park (pre-prompt failure fails the
  run; post-prompt failure rides the terminal). The footer merging
  family realigns to the same words (pre_prompt, merge, conflicts,
  testing, fixes, post_prompt, parked, failed, merged).
- CONSEQUENCE for the self-reload trigger: the landed range is the
  merge commit's second-parent history (default..branch), read off the
  commit — the cherry-pick-annotation walk dies.
- The per-repo queue and the terminal path (bubble settles, teardown,
  daemon-owned worktree removal) are IDENTICAL for both methods; a
  sessionless workspace with a configured prompt gets a session started
  under the lease.

### The MERGE BUBBLE becomes a SUB-FEED — six per-kind-state tabs; the PARKED lease policy; merge routing goes address-driven

Settled during the merge-flow gap remediation (a sanctioned post-freeze
increment), superseding BY NAME the per-phase tab strip and
presentation-nesting spec of the FeedMerge landing:

- THE BUBBLE IS A SUB-FEED: FeedMerge shrinks to the collapsed HEAD (tag 5
  `phases` retired); the bubble's own FeedId is its feed address (OpenFeed
  → WatchFeed, the subagent mechanics — ONE shared plumbing path, a
  merge-specific loader is a defect); content is LAZY (collapsed = head
  only; settled merges page like any settled bubble).
- SIX TABS as sub-feed rows (NEW FeedRow arm `merge_tab`, sub-feed-only):
  queue | rebase | tests | remediation | action | landing. TWO SHAPES:
  RESOLVED tabs (queue, tests, landing) carry content in the row, replaced
  whole; AGENTIC tabs (rebase, remediation, action) are containers whose
  content is sub-feed rows parented to them. Conflicts are the REBASE's
  hard parts, never a separate tab; rounds are new tabs ("tests (2)"),
  append-only as before. EACH KIND OWNS ITS STATE oneof (agentic: live |
  parked | settled; resolved: live | settled) with SHARED leaf payloads —
  the footer-restructure pattern, so "parked queue" is unrepresentable.
- TESTS SHIP COLOR: the daemon parses the terminal's ANSI into
  paint-class SPANS (this component's own wrapper, the code-span
  precedent); the client paints classes, never escapes.
- THE QUEUE TAB keeps ahead/current/behind, replaced whole; the front
  entry's status carries the front's ACTIVE TAB LABEL (the same message
  its own bubble draws, imported never respelled).
- ADDRESS-DRIVEN ROUTING: the feed resolver is MERGE-AGNOSTIC — a lease
  holder supplies a generic OUTPUT ADDRESS {target feed, parent row}, set
  on acquisition, updated per tab, cleared on release; while it stands,
  every session-produced record resolves to the merge sub-feed under the
  active tab and the root feed gains nothing but head upserts. Only the
  MERGE ORCHESTRATOR (semantics, facts, address) and the FOOTER RESOLVER
  (the merging status family) know "merge". Feed and footer pushes are
  dispatched IN PARALLEL at every merge-state change (prescription, not a
  wire invariant).
- THE PARKED POLICY, state-based recognition: when the agent exhausts its
  attempt, the lease's refusal policy flips to PARKED — prompts are then
  NOT refused and NOT queued as the session's own turn; the queue's one
  path delivers them through the merge orchestrator as guidance to the
  resolution agent, landing in the parked tab. NO classifier, no content
  inspection: the LEASE STATE is the recognition. The host composer oneof
  gains `merge_parked` (open-with-context) beside `merging` (closed); the
  footer merging family realigns (cherry_picking → rebasing, conflict →
  parked{composed line}, + remediating).
- HAND-RESOLUTION IS UNSUPPORTED by ruling ("not something that will
  happen"): no resolved-continue verb ever exists on any ingress; the
  conversational parked flow is the ONLY resume — which also resolves the
  old conflict-resume triage question.
- DEFERRED, next in the walk: the non-Emacs-repo merge (queue+rebase only
  — no well-defined tests, hence no remediation; the append-only tab rule
  already makes absent tabs structural) and MULTI_REPO_ROOT handling.

### The producer SPILL is REMOVED — WriteBatch failure holds in a bounded in-memory buffer; exhausted retries are LOUD

Settled during the rollout-controller planning (a sanctioned post-freeze
increment, reopening the WriteBatch entry's spill language). The user's
ruling: a failure to relay information from the shim to the store — the one
and only situation the spill served — is a LOUD FAILURE (loud logs, never a
shim crash), addressed if it ever occurs in production; durable
producer-side persistence is a workaround, not a robust fix, because a
persistent inability to reach the store indicates a LIFETIME-SEQUENCING
defect whose solution lies in the sequencing. What replaces it: a bounded
IN-MEMORY retry buffer absorbing transient blips silently; exhausted
retries log what was lost, loudly. Sequencing rule that falls out: the
shim's graceful stand-down WAITS FOR ALL ACKS before exiting — an exit
with unacknowledged writes IS the loud failure. Blast radius accepted:
records still close via GetLiveWork reconciliation and transcript-backed
content still arrives via the sidecar; only stream-only residue of the
crash window is lost, in a compound case (shim death during a store
outage) the operational rulings make doubly rare. The force-kill
spill-adoption question dies with the spill.

### The GRACEFUL-ROLLOUT HANDOVER lands — WatchDaemon, the adopt rendezvous pair, and the WEB LINK section

Settled during the daemon architecture planning (a sanctioned post-freeze
increment, the rollout controller's wire): a daemon self-rollout is a
blue-green handover — the old daemon spawns the rebuilt one (joining mode:
fresh socket, WSM read-only, owns nothing), announces, and transfers
workspaces one by one at freeness (no in-flight turn, no live detached
work), with no daemon↔daemon channel: coordination is client relay + WSM
facts + per-workspace kernel locks.

- HOST section gains `WatchDaemon {}` — THE daemon-level host stream the
  record reserved ("until a daemon-level pushed fact needs one"; that fact
  arrived). First push arm: `shutdown_announced { address }`. Emacs — the
  singular client multiplexer — dual-attaches on it.
- `WatchHostWorkspaceResponse` gains two push arms: `transferred` (the old
  daemon released this workspace at freeness and does no further work for
  it; Emacs adopts on the new connection then CANCELS the stream — a PUSH,
  never a terminal frame, honoring the standing-stream convention) and
  `reload_webapp` (webapp-only rollout: Emacs reloads the xwidget against
  the SAME daemon; empty by design — no address, and a combined rollout
  never sends it because the handover re-attach pulls fresh assets).
- NEW WEB LINK SECTION — the webview's connection surface, neither
  host-natured nor a drawn component: `WatchWebWorkspace { WorkspaceRef }`
  (the webview analog of WatchHostWorkspace — "Web" qualifies the VIEW as
  "Host" qualifies Emacs's; push `transferred { address }`, the address
  riding here because a webview has no daemon-level stream) and
  `AdoptWebWorkspace`.
- THE ADOPT RENDEZVOUS, two sibling verbs (`AdoptHostWorkspace` /
  `AdoptWebWorkspace`) so THE VERB identifies the participant — a shared
  rpc would need a self-declared kind field a confused client could get
  wrong. Expected participants = holders of the two per-workspace streams
  at announcement time; the new daemon completes adoption (claim the
  kernel lock, adopt the running shim, drain held intake) only when every
  expected participant has called, and all succeed together. Headless
  workspaces (no streams) transfer with zero rendezvous via WSM facts and
  the lock alone.
- ORDERING BY REFUSAL, not convention: the new daemon refuses per-workspace
  rpcs for an unowned workspace; OWED TO THE WAVE as derived error arms:
  `transferring_away { address }` on the old daemon's per-workspace verbs
  (a lagging client self-heals from the refusal) and `not_yet_adopted {}`
  on the new daemon's — two arms, not one, because wrong-daemon and
  right-daemon-too-early are different facts.
- INTAKE DURING THE WINDOW: prompts are HELD (never errored) and replay in
  order on the new daemon; the merge-in-flight refusal is unrelated and
  stands.
- ADOPTION TIMEOUT (the user's ruling): the OLD daemon times the window
  and surfaces expiry as that workspace's own error — remediated as it
  comes up, EXPLICITLY NOT an invariant to harden: absent a systemic
  cause it is not treated as a guarantee the architecture provides, and no
  abort/retry machinery exists.
- NEVER-FREE WORKSPACE: the rollout waits forever in the two-daemon steady
  state, logging a periodic warning (~10 min) naming the holdout; a newer
  rollout supersedes a joining daemon that never finished.

### The footer gains waiting·interrupting — the stop acknowledged the moment it registers

Settled during the daemon feature-loss triage (a sanctioned post-freeze
increment): when an interrupt registers and teardown begins, the footer
flips to waiting·interrupting IMMEDIATELY — before the turn's real end —
with a composed activity line; the interrupting prompt is placed at the
queue's semantic head so it, not the pre-interrupt head, is the next
delivery. New arms: FooterSubStatusWaitingInterrupting (6) and
FooterStatusActivityInterrupting (10) on the waiting activity oneof.

## 2026-08-29 — the creation-facts increment (triage ruling; landed 5c2dee711)

`endpoint_create_workspace.proto` reshaped: a `form` oneof (`standard` |
`one_shot`) confines form-specific fields per the adjacent-exclusivity
test — a one-shot never carries name/base_ref/merge_actions/fork, so the
arm is the kind. Shared facts beside the form: `parent` (UNSET = top-level,
merge target the repo mainline; PRESENT = child, merge target the parent's
worktree/branch — recorded into the merge layout facts; `fork` lives
INSIDE parent so a parentless fork is unrepresentable), `model`,
`WorkspacePriority` (new shared file, oneof of four empty level arms —
also the future SetWorkspacePriority vocabulary), and
`CreateWorkspaceUngatedConsent` (presence IS consent; an ungated-mode
creation without it is refused). `CreateWorkspaceOneShot` = required
prompt + required `finish` oneof (`self_merge` | `open_pr {self_certified,
add_to_merge_queue}`); the daemon owns the one-shot's whole sequence.

## 2026-08-29 — SetWorkspacePriority + SetPermissionMode (landed cb76e1873)

Two verbs: `SetWorkspacePriority {workspace, optional priority}` (SIDEBAR;
UNSET clears; roster push carries the state) and `SetPermissionMode
{workspace, mode string}` (TOPBAR, SetModel's sibling; mode spelled as the
session facts spell it, validated against the switchable set; new mode
arrives on the pushed surfaces). Both empty-success, empty derived-error.

## 2026-08-29 — the shim trio (landed with Hibernate)

`shim.v1 Hibernate` (SESSION): the daemon's pre-hibernation directive —
shim compacts, acks, and only then is stood down (revival never pays a
cold context). `SessionUpdate.compacting = 27` (empty arm): vendor
auto-compaction began; the ContextCut record is the end. Build identity
needed NO new field — `SessionRuntime.shim_build_sha` already rides
SessionStarted; its comment now names the staleness-bounce use.

## 2026-08-29 — the rollout/admin pair (typed, landed)

`drain_reason.proto`: `DrainReason { deploy | maintenance | operator{note
required non-blank} }` — a TYPED vocabulary; no bare reason strings.
`DaemonShutdownAnnounced` enriched: optional address (UNSET = plain
bounce), `DaemonShutdownCause { self_merge_rollout | scheduled_drain
{reason} | immediate {reason} }`, expected_outage_ms, minted_at_ms (a late
receiver shortens its window). `WatchDaemon` gains `drain_scheduled
{at_ms, reason}` / `drain_cancelled` pushes; `UpdateShutdownSchedule`'s
schedule and now arms both REQUIRE a DrainReason.

## 2026-08-29 — Interrupt: fan-wide target + confirm challenge (landed)

Kept the absorption ruling: no CancelDetachedAgents verb returns; instead
InterruptRequest.target gains `all_agents {}` (the fan-wide stop, success
via the existing interrupted_detached count arm), plus `confirm_agents`
(bool, answers the challenge) and the landed refusal arm
`InterruptError.confirm_required { live_agent_count }` — a turn stop with
live agents is refused once, naming the count.

## 2026-08-29 — the login port (landed; 42 rpcs)

Three WEB LINK rpcs carry the verified pty mechanism: `OpenLogin
{workspace}` → success{config_dir} (per-account idempotent open-or-join);
`WatchLoginTerminal` — BIDI stream, first input frame is `attach
{workspace}`, then keystrokes/resize in, raw pty bytes out (scrollback
replay first), concluding with the `closed` terminal frame when the login
child exits; `CloseLogin` (absent login = success). Daemon owns default
geometry (no OAuth URL wrap); nothing parses the TUI.

## 2026-08-29 — small verbs + typed notification (landed)

`OpenExternal {workspace, url}` (WEB LINK; the pinned-profile launch).
`HostWorkspaceNotification` gains a REQUIRED typed `HostNotificationKind
{ agent_addressed | permission_requested{tool_name} }` — semantics ride
the arm, never the text; a permission ask now fires the push and sets the
attention marker. Task plane: `task.proto` (`TaskRef`, daemon-minted echo
token) + `CreateTask{title}`, `UpdateTask{task, set_title|set_done|
set_open}`, `AssignWorkspaceTask{workspace, optional task}` (UNSET =
unassign) on the SIDEBAR section; WSM stores tasks + assignments, the
roster's task view renders them.

## 2026-08-29 — durable prompt origin + roster badge (landed)

`PromptOrigin` MOVED shim/v1 → conversation/v1 (layering: the durable
record may not import shim.v1; shim.v1 imports it back for StartTurn) and
`AgentPrompt` gains `origin = 4` — persisted with every delivered prompt,
so replay routes merge-born rows and labels restart re-drives.
`RosterRow.priority = 33` (optional `RosterRowPriorityBadge{label}`,
resolver-composed): the drawn badge; ordering stays the resolver's.

## 2026-08-29 — the /context rich schema (landed; increments complete)

`SessionContextUsage` reshaped to the vendor's FULL get_context_usage
answer, typed field-for-field against the SDK's declared response type
(sdk.d.ts SDKControlGetContextUsageResponse): totals + raw window +
vendor percentage + model, categories{label,tokens,color,is_deferred?},
memory_files, mcp_tools, deferred_builtin_tools, system_tools,
system_prompt_sections, agents, slash_commands/skills roll-ups (with
per-skill frontmatter), auto_compact_threshold?/is_auto_compact_enabled,
message_breakdown (plane figures + the ENCAPSULATED tool_calls_by_type +
attachments_by_type), api_usage? (vendor-nullable). gridRows omitted —
pure presentation. `ContextPanelView` rewritten as resolver-composed
typed sections (composed header/figures — the client never does
arithmetic), with message_breakdown.tool_calls the auto-folded foldable
render per the ruling. THE TRIAGE'S CONTRACT INCREMENTS ARE ALL LANDED.

## 2026-09-23 — KillTurn: an unforced kill ends only the synchronous turn (doc comments; landed)

Owner rule: an interrupt ends ONLY the synchronous turn and never stops
detached work, which ends only through its own per-task stop or an explicit
forced kill. `KillTurnRequest.force = false` now interrupts the turn and
leaves every live spawned item running with its streams open; an unforced
kill of an already-closed turn answers `no_turn_open`. `force = true` is
unchanged. `TurnKilled.agent_only` now also covers an unforced kill that
spared live work. `KillTurnFailure.live` / `TurnLive` are NO LONGER
PRODUCED; the arm is retained, documented as such, pending an owner ruling
on removal. No field, arm or number changed.
