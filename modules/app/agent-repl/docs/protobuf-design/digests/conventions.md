# CROSS-SYSTEM CONVENTIONS — digest

DERIVED from figma-to-idl-redesign.md at 86fd2b543 — the canonical record WINS on any conflict.
Do not edit; regenerate.

Scope: the core design principles, identity vocabulary, schema and wire conventions, the
fidelity principle and exempt set, standing policies, the implementation conventions, and
terminal status. Per-shape landings are out of scope except where they establish a standing
rule.

---

## 1. Core design principles

Project-wide facts stated as authorities over every later sketch. A shape that contradicts one
is re-partitioned before it is offered, or the contradiction is put to the user as a deliberate
question.

### 1.1 A subagent is handled EXACTLY as the turn is — by the daemon, and on the daemon↔shim API

DECISION. Subagents are internally handled exactly the same as a turn from the daemon's
perspective and on the daemon↔shim API. Clicking a subagent renders THAT subagent as the
first-class view — its feed, composer, held prompts — and submitting a prompt routes to it,
queued prompts presented as for the main agent.

WHY. One vocabulary, one handler, one queue; requests differ only in the address.

CONSEQUENCES.
- The write surface to any live agent is ONE type; the turn's update rpc and the subagent's
  carry it identically.
- Prompt QUEUING is daemon-side and kind-agnostic: a prompt to a busy subagent is held,
  classified and delivered (interrupting if that is what delivery takes) by the same machinery
  as for the turn.
- The read surface is likewise one type: the agent frame is the frame of the turn's stream and
  of a subagent's stream.
- Workflow and bash follow in principle, moot in practice (a workflow is never carried by a
  turn; a process has no input but stop).
- Does NOT claim bash or workflow become agent-shaped.

### 1.2 Sessions and turns OUTLIVE the daemon — SPAWN, ATTACH and END are decoupled

DECISION. Sessions and turns exist outside the daemon's lifetime, so spawning and attaching are
decoupled; the daemon records the identifiers durably and on restart simply runs the
corresponding Watch.

CONSEQUENCES.
- SPAWN is unary and returns (or adopts) an identity the daemon persists. A Watch never creates
  anything.
- ATTACH creates nothing and ENDS NOTHING: a consumer closing the stream — gracefully or
  abruptly — leaves work running and information accumulating. Attaching late or after a restart
  misses nothing.
- END is a Kill that refuses while work is live unless forced, and NAMES what it killed. Narrow
  stops remain single-target interrupts.
- No `CloseXConnection` verbs: cancelling the stream IS the graceful close in Connect; a verb
  would duplicate the transport's own act.
- The bounded-stream rule "a client-side close is a transport failure" now binds the PRODUCER
  side only: a stream the producer ends without a terminal frame is the failure.

### 1.3 The shim, store and sidecar hold NO VARIABLE-SIZE STATE

DECISION. No implicit or explicit variable-size state (stacks, queues, trees) in
shim/store/sidecar. A single parent-id lookup is static and fine; a VARIABLE number of lookups
(e.g. to determine lineage) is not. The DAEMON is not bound — it is the one component allowed to
hold state.

FOR THE STORE. The store writes conversation.v1 to the database. Shim and sidecar resolve each
frame at write time with a constant number of single lookups (tool return → its call; skill
document → its call; spawned agent → its spawn; a re-announced start → its original instant).
Rows are conversation.v1 messages; no record-granular vendor vocabulary is persisted.

INDEXING (architecture guidance, not schema). Every stored message carries its PARENT id and its
TOP-LEVEL id, both indexed. Top-level is set AT INSERTION to the parent's stored top-level — one
lookup — so by induction every descendant carries the true top level and no insert walks more
than one step. Prompts are stored as messages in their own right.

PAGINATION HAS TWO KEYS. A bare `agent_id = X limit N` is a trap: one vendor API response
arrives as SEVERAL block units, all rows under X. So every row carries its AGENT (lineage) and
its PARENT ITEM (the response it is a block of, keyed by the response's ANCHOR unit — the same
first-block unit the usage rule singles out). Prompts and anchors are PARENT-LESS. A page of
agent X is the N most recent parent-less rows of X plus every row whose parent item is one of
those — two indexed queries, constant per row, no walk. Containers (subagent, workflow run) are
doors: their contents are rows under the CHILD's own agent id.

THE THREE LINEAGE KEYS. (1) `top_level` — the nearest NON-SYNC ancestor, i.e. which live stream
carried the work; denormalized at insert by the one-lookup induction; used for kill scope and
session scope, NEVER for paging. (2) The PARENT AGENT — the immediate agent, sync subagents
included, read from the frame and never restated on the envelope; it is THE pagination key, so a
sync subagent is exactly ONE item in its parent's page. (3) `parent_item` — the within-response
anchor grouping.

A PAGE IS "IMMEDIATE CHILDREN OF X", newest first. A unit's later frames are not children of
anything — they are upserts of the same unit — so the tree is agent → units. The one non-agent
container is a WORKFLOW RUN; workflows are not loaded by page, the SUBAGENTS WITHIN are.

RULING 1 — lineage is a DENORMALIZED ROOT, never a walk. Verified: the vendor names a task's
owning AGENT but never its owning TURN, so the stamp is required.

RULING 2 — detachment is read from the EDGES; the LEVEL is relayed, never diffed. The vendor's
edge messages are bookends; the level is the full set with REPLACE semantics and must not be
correlated with the edges. A diff against the previous level would be a retained set. The
no-edge-pairing rule binds the daemon's INDICATOR only.

RULING 3 — `update` frames carry DELTAS, never cumulative text. The shim returns only what
CHANGED; coalescing or forwarding is the daemon's choice, where state is allowed. A
gap-detecting offset was refused for prose (a lost fragment is evident in the response and the
terminal frame carries the whole text); bash keeps its offset because its spool has no settled
whole to recover from.

---

## 2. Identity rulings and vocabulary

### 2.1 The four identifier spaces, NOT interchangeable

Recorded because two were conflated; joining on the wrong one gives plausible, silently wrong
attribution.

- `agent_id` (vendor) — WHICH AGENT INSTANCE, its own space. The vendor's parent-agent field is
  the proof: depth beyond one cannot be resolved from call ids at all.
- `tool_use_id` (vendor) — WHICH TOOL CALL. For a spawn it identifies THE SPAWN, not the agent
  spawned; the vendor keeps the two adjacent and separate.
- `activity_id` (ours) — WHICH UNIT OF WORK: the stable identity of one unit from first frame to
  last, what every frame upserts and what a nested unit is scoped under. SHIM-minted, one per
  unit for its whole life, sourced from `tool_use_id` where the vendor has one and from message
  id plus block index otherwise. IT NAMES WORK, NEVER AN AGENT.
- `TurnId` (ours) — WHICH TURN: the window during which the main thread cannot accept a prompt.
  Daemon-minted, returned at submission.

An agent is not its spawning call; a unit of work is not the agent doing it; a turn is neither.
ROOT CAUSE of the conflation: reasoning from where an id happens to come from to what it MEANS.
Provenance is not semantics.

### 2.2 Vendor identity never crosses the contract

Vendor uuids, message ids and prompt ids stay shim-side; the shim translates where a unit
exists. Permanently uncarried, logged in the deferred document: which messages a compaction
preserved, ancestry across a compaction, vendor request/message ids on failure evidence.
Corollary: opaque vendor handles naming a JOB rather than a transcript record (cron ids,
background task ids) may be carried opaque.

### 2.3 Typed identities are imported, never respelled

A typed identity is a join key, not a prop: every importer embeds the owner's type rather than
restating a string. Identity join keys are explicitly carved out of the figma→idl respelling
license.

### 2.4 The typed echo token

Where a value is dynamically determined by the backend and reused by the frontend in a later
request, the provider mints a WRAPPER MESSAGE, embeds it in what it serves, and the request
field is the SAME type echoed back unchanged. The round trip becomes a schema fact, and a client
that invents a value is typed as wrong rather than merely told not to. Applied to the model
selection, workspace and repository refs, the detached-work handle, the turn id in the hold
tray, the feed watch token, the store session token, the history continuation.

REFINEMENT. The convention is about the value being ONE TYPE across the round trip, not about
being opaque. Where the producer's own key IS the text (a question's text, an option's label),
echoing the typed VALUE is correct and requires the shim to remember NOTHING. A minted token was
rejected there because it would need shim-side state; position was rejected because nothing
about the producer's data is ordered. Validating an echo costs nothing where the producer
already holds the pending ask by construction — no NEW state, not merely relocated state.

### 2.5 Daemon-minted identity and opacity

- A path is never an identity (directories can be spelled many ways). The client PROVIDES the
  path; the daemon MINTS the identifier and returns it.
- The FeedId is one opaque daemon-minted string: the daemon ENCODES the identity of what the row
  DRAWS and DECODES it on echo — no id table, stable across pushes and restarts. Typed identity
  spaces are FULLY HIDDEN from the frontend.
- UPSERTS ARE UNIVERSAL: every row replaces whole by id, under "one row per drawn subject".
- The detached-work handle STAYS as the uniform CONNECTION token: it makes the daemon→shim
  connection identical regardless of kind. The PRODUCER may map to it differently per underlying
  message; the CONSUMER never does that mapping.

---

## 3. Cross-cutting schema and wire conventions

### 3.1 The response-outcome convention

- Every response is `oneof result { <Method>Success | <Method>Error }`; every rpc returns
  `<RpcName>Response` and never a foreign type directly (a stream of a view wraps the view as
  its single field).
- ERRORS ARE DERIVED, NEVER INVENTED: arms come from the implementation's real refusal sites,
  added at the wave; an empty error arm set until then is correct.
- DOMAIN OUTCOMES ARE ANSWERS, NOT ERRORS: unhealthy is a success arm; a denial is an answer; a
  stop that found nothing running is a success arm; a search that matched nothing is a success
  with an empty answer; an unanswered ask is a success carrying the unanswered arm.
- Failure is scoped to the ACT it describes: an ask's failure arm covers failing to ASK; failing
  to APPLY an answer belongs to the answer's own call, which this convention already gives a
  failure arm.
- Success is EMPTY on purpose wherever the new state arrives as a push; restating it in the
  unary response would be a second authority.

### 3.2 The bounded-stream convention

Every frame of a stream that CONCLUDES carries a one-level `oneof result { update | success |
failure }`. A conclusion is a MESSAGE the producer sends, never the stream merely stopping — so
a stream ending WITHOUT a terminal frame is a transport failure and is read as one. One level,
not two, for consistency; a shared terminal fact is duplicated across arms rather than nested.
SCOPE: bounded streams only — STANDING streams get no terminal arm, which would invent an ending
they do not have. Cancelling an item is a CALL, never a client-side close, which would be
misread as a transport failure.

### 3.3 No keepalives anywhere (a retraction)

RETRACTED: the in-band keepalive frame. It bleeds implementation detail and puts the detector in
every client. The replacing layering, each layer already modeled: source-of-truth silence is
observed and reported by the DAEMON as a real fact (the party that can see the silence reports
it); pipe death is seen by the client library; a wedged publisher behind a healthy connection is
a daemon-internal fault its own watchdog surfaces. Stream messages therefore carry views only. A
terminal frame does not reopen this — it is a real fact the producer knows and states, the
opposite of a ping standing in for a fact nobody observed.

### 3.4 Push cadence

Stated once in the schema and named as the convention for EVERY frontend.v1 view: EVENT-DRIVEN,
WHOLE-VIEW, NO TICKS. Push the whole view on any resolved change, nothing on no change;
client-side ticking from shipped instants; bursts are coalescible because the wire carries
STATES, not events. A partial-replace or delta oneof is a delta creeping back in and is
rejected; if a ticker's rate ever matters the fix is daemon-side coalescing.

### 3.5 The clock convention

Clocks carry only an INSTANT and the client ticks. An elapsed figure on the wire was rejected
twice over: it is a second authority for a value derivable from one instant, and it arrives at
the producer's cadence, so a drawn clock would jump to network timing. Countdowns invert the
same rule: the daemon ships the DEADLINE instant and the client re-derives remaining time at a
one-second tick. A frozen clock on a settled item is a daemon-composed sentence from the start
and settle instants, UNSET when either is missing — never a ticking or invented figure.

### 3.6 Completeness factoring

Two adjacent fields where one's value depends on the other's screams oneof factor — a special
case of adjacent exclusivity, since a total means nothing when the answer is complete. Factored
to `oneof extent { all | partial }`, and the partial arm carries what was OMITTED rather than a
total: that is the figure a reader is shown, it needs no arithmetic, and a total is recoverable
by addition.

NESTED distinction: where the count can be a FLOOR, the omitted figure carries `oneof { exact |
at_least }` — "42 more" and "at least 42 more" are different claims and only one is safe to
draw.

At the backend→frontend boundary the completeness arms COLLAPSE to daemon prose (a composed
omitted sentence): the client draws, never compares. Omitted counts serve DISPLAY ONLY and imply
no retrieval. A progress fraction (both counts always drawn) is explicitly NOT the completeness
case.

### 3.7 Adjacent exclusivity, mode-selecting bools, enums

- A boolean that selects the INTERPRETATION of adjacent data is a TWO-ARM ONEOF of dedicated arm
  messages, never an adjacent bool. A plain bool is fine where no adjacent data changes meaning
  under it (e.g. a force flag).
- If any value of an enum corresponds to adjacent information exclusive to that value, that is a
  strong indicator the enum should be a oneof with the exclusive information confined to the
  arm.
- STATE ENUMS ARE PROHIBITED. Closed scalar SETS that are not state (a command name, a
  compaction scope, a hook event, an LSP severity, an effort level) legitimately stay enums.
- Data meaningless in a state lives INSIDE that state's arm (a recovery count in the closed arm;
  an active-form phrase in running; a cache TTL in the lapsed arm; a composer state in the live
  arm).

### 3.8 Presence, never sentinels

- Nilable MESSAGE fields carry the explicit `optional` keyword so maybe-absent reads off the
  schema at a glance.
- Absence is a legal answer, never an empty string or zero; `-1` and empty-means-all sentinels
  become presence.
- Always-set fields stay bare (recorded borderline judgments: panels always resolved on every
  push; rows whose bool carries the state; facts stated as unconditional).
- An unset element of a VIEW is a malformed frame, not a state: every element is always set and
  "nothing to show" is expressed INSIDE the element.

### 3.9 One canonical form; no respelling; extraction

- Import the encompassing message; re-spelling in its four forms is a defect; depth is not
  synthesis. What is repeated across sibling shapes is EXTRACTED and declared once per kind, and
  every carrier wraps it whole.
- Where a terminal fact appears twice, the second EMBEDS the first's message.
- Two places describing one fact could disagree; one cannot.
- NO COLLECTIVE NOUN for the protocol's units: the stream frame's oneof names the concrete kinds
  directly. Ruled out with reasons: "element" (owned by figma→idl), "entry" (owned by the
  datalayer), "event" (these units have state and SETTLE), "node".

### 3.10 figma→idl takes precedence over no-respell at the backend→frontend boundary

DECISION. `frontend.v1` is composed ONLY of element messages shaped for the drawn element. Where
props derive from an internal type, the DAEMON RESOLVES a frontend-shaped message — a deliberate
re-spelling into UI vocabulary.

WHY SAFE THERE AND NOWHERE ELSE. The daemon is the single resolver, so the frontend copy is a
resolved VALUE re-published on every change and self-corrects like any duplicated value; the
internal type keeps its canonical form; the frontend never becomes a second AUTHOR of the fact.

NOT LICENSED: respelling within conversation.v1 / shim.v1 / store.v1; frontend.v1 redeclaring
another surface's type for anything but drawing; restating typed identities, which are still
imported.

### 3.11 UI element-message conventions

- A view message contains NO dangling primitives: every field, addressing included, is wrapped
  in a dedicated named message, so subcomponents are implicit in the schema.
- When the SAME fact appears in two component views, each wraps it in ITS OWN message —
  duplicate information in dedicated messages implies separate UI subcomponents.
- THE MESSAGE TREE IS THE UI TREE: every drawn box is one message, nesting equal; the ASCII
  drawing is agreed BEFORE shapes are sketched and checked against them.
- The daemon resolves precedence, wording, formatting, figures and highlighting; the client
  draws. Client-side derivation is the defect being removed. The one recorded, component-bounded
  departure is the cold-context gate, whose facts are counts and instants rather than resolved
  presentation.
- GENERICIZE: presentation forms rather than per-tool arms, so the client holds no per-tool
  knowledge and a tool-specific affordance is a schema change by design.
- Legality by construction where a family allows it: a parent arm declaring exactly the child
  steps legal while it stands makes an illegal pairing unrepresentable rather than forbidden by
  comment; leaf payloads are SHARED across the per-parent oneofs and the oneof types carry the
  legality. Where a fact is parent-independent it is duplicated into EVERY arm (suppression by
  unrepresentability would silently drop it) and a precedence ladder is stated once on the
  family banner. Optionality is EVIDENCE-GATED: required where a producer always has a value,
  optional elsewhere with the WHY stated at the field; a bare optional with no stated absence
  path is a review defect.

### 3.12 The node model, and the `start` / `update` rule

- THE PROTOCOL MODEL IS NODES, NOT A LOG: identity per THING, not per arrival. A unit holds one
  identity from first fragment to last; growth is a re-send; a frame is an upsert of the WHOLE
  unit. The SHIM performs the fold; the flat faithful log stays on the datalayer side.
- `start` means "THIS STREAM now carries this unit" — not that the work began. `update` reports
  GROWTH, and a kind has an update arm IFF something actually produces growth for it. PROGRESS
  is a THIRD arm kind — a beat, not growth — so `update` stays earned by growth alone.
- EVERY STREAM THAT CARRIES A UNIT OPENS WITH THAT UNIT'S `start`, and a second announcement
  repeats the ORIGINAL instant so a drawn clock does not reset when work moves between streams.
  Upsert makes re-announcement the mechanism, and the daemon must survive its own restart, so a
  stream whose first frame presumes an earlier one is unrecoverable.
- Because a frame upserts the whole unit, a terminal frame omitting the unit's identifying
  element would LOSE it for a consumer that missed the announcement — the duplication is
  required, not tolerated.
- On a nested arm the outer id is the containment path; the unit upserted is the INNERMOST one.

### 3.13 Announcement rides the spawning stream

The stream that spawns work ANNOUNCES it as it happens, never batched. One rule applied
recursively, so the stream tree mirrors the work tree and provenance is implicit in which stream
announced an item. A session-scoped roster stream was REJECTED as a second authority for a fact
the streams already state structurally. An announcement with no actor and no unit of anyone's
work (a script-created agent) gets its own arm rather than being wrapped in the activity
envelope, which would require inventing both.

### 3.14 Open/watch bifurcation and pagination

- OPEN is a bounded unary answer carrying the first page and minting the watch token; WATCH is a
  pure tail addressed by that token, pinned to begin exactly after the page. The token exists so
  a client MUST open before it can watch.
- The caller's own high-water mark decides repaint vs catch-up; the producer deliberately tracks
  NOTHING about what it previously served. Every streamed item carries its pointer so the caller
  always holds a current mark.
- Page size rides the REQUEST so it can vary across a walk; a continuation carries ONLY the
  pointer the previous response served.
- Order is by the unit's FIRST INSERT, never its last write, so a unit settling mid-walk cannot
  teleport across a continuation.
- PAGINATION IS FOR AGENTS AND NOTHING ELSE. The test is IDENTITY: an item has one, can be
  upserted, and can contain other things. Bash lines, read lines, grep matches and patch hunks
  are positional, not identified.

### 3.15 NO `seq` on the daemon-facing wire

DECISION. History is first/next with an OPAQUE continuation token; the turn stream carries no
position at all.

WHY. The decisive argument is COUPLING: a turn frame carrying positions makes the turn handler a
participant in history, the wrong seam. The one real need — catch-up after the daemon's own
downtime — is ordered PAGINATION, not seq.

CONSEQUENCE. The daemon persists an OPAQUE TOKEN instead of a number, so advancing a cursor past
a position the store never assigned is UNREPRESENTABLE rather than guarded.

DELIBERATE divergence, stated so it does not read as inconsistency: at the frontend boundary the
DAEMON holds each container's walk position and the client says only first/next, because a
webview's walk dies with the webview; at the shim boundary the token is persisted by the DAEMON,
because it must survive its own restart. Same no-leaky-cursor discipline, different holder.

### 3.16 The datalayer and the protocol are TWO MODELS

DECISION. `store.v1` owns the DATALAYER model, shaped for storage; `conversation.v1` owns the
PROTOCOL model, shaped for consumers; the SHIM owns the mapping — one mapping, one place, one
author.

ACCEPTED COSTS, stated because no-respell otherwise holds throughout: a hand-maintained mapping
has NO COMPILER to detect divergence, and the mitigation owed to the wave is tests that FAIL
when a field is added on one side and not mapped. The guarantee given up was that the daemon's
view was a field access no mapping function could forget.

COLUMNS VS BLOB: entities the store filters and joins on are UNPACKED to columns; content the
store never queries stays a SERIALIZED frame it never opens — unpacking content would put every
conversation.v1 churn into DDL for nothing.

The PRODUCER decides pageability (a page line NAMES its book); the store holds one row per
opaque producer-minted upsert key and a write supersedes it whole; the store has a SINGLE place
it looks for a given property.

### 3.17 Documentation standard for landed protobufs

Every landed message, oneof and field carries documentation written FOR A FUTURE INTEGRATOR:
what it is in domain terms, why the shape exists and what it rules out, when precisely a
producer sets it and a consumer sees it, what the consumer does with it, producer obligations,
and integration gotchas. Design knowledge established in conversation is CARRIED INTO the
comment, because knowledge living only in the design record is unavailable at the call site.

The systematic omission the audit exposed was `oneof` ARM LINES: the arm line is where a
consumer decides whether to handle the case at all, so "expect this when…" belongs there and the
payload's meaning on the message body.

HARD PROHIBITION, permanent: landed comments NEVER reference the development process — no
earlier drafts, objections, amendments or "used to be". State the result and its reasoning as a
standing fact; history belongs in the record. Also prohibited: serialized-shape comments at line
ends.

Codified into `/create-or-update-protobufs`: a landing with an undocumented declaration is
incomplete, not merely untidy.

### 3.18 Untyped fields are an ACCEPTED COST, never a fallback

The one qualifying reason is that the producer holds no schema for the thing and nothing routes
on or renders from it: arguments of a tool the producer did not define, an arbitrary
user-authored script payload, a client log's per-call-site diagnostic context, a genuinely
unknowable content block. Each is stated at the field.

An unmodeled/unsupported arm is NOT A FALLBACK: it is for shapes genuinely unknowable in schema,
never for modelling that was inconvenient or deferred, and a recognizable built-in arriving
there is a PRODUCER DEFECT. Written down because that arm rots quietly otherwise. There is no
free-text escape where the producer CAN classify — an activity the daemon can name but has no
arm for is a modelling gap and the fix is the arm.

### 3.19 File and package model

- Package boundary is surface boundary; package names encode ownership, not routing; the
  drawn/called test separates frontend from the API surface.
- Files are grouped BY CONCERN: one concern, one file, one review, one import.
- Within a stage, files are walked TOP-DOWN BY CONTAINMENT, read mechanically off the
  intra-package import graph, importer before imported, so a leaf is never designed before the
  message embedding it has said what it needs.
- An RPC service package is `service.proto`, one `endpoint_<rpc>.proto` per rpc, and
  `<shared>.proto` for what MORE THAN ONE endpoint needs. Small single-caller packages may fold
  their reads into one file.
- Workspace-specific shared dependencies live in a LEAF PACKAGE so a drawn package never imports
  a called one.
- LAND WHETHER OR NOT IT BREAKS: a package left dark until its own stage is intended, not
  patched around.
- Sketches show the ENCOMPASSING message with a path comment; fields may be elided within it;
  never floating fields.
- Every concept or distinction put to the user carries exemplifying protobuf whenever a schema
  can illustrate it — explaining a schema distinction in prose is the recorded root cause of a
  wasted iteration.

---

## 4. The fidelity principle and the exempt set

### 4.1 CORE PRINCIPLE: conversation.v1 carries the vendor's fields even when NO UI maps them

DECISION. Always include fields in the conversation protos even when unsupported in the UI,
commented EXPECTED UNMAPPED at the field. conversation.v1 is a FIDELITY LAYER; UI-relevance
gates frontend.v1 only.

CONSEQUENCES. "Nothing draws it" is no longer a reason to drop from conversation.v1 (it remains
one for frontend.v1). Recorded drops justified solely by that reason were reopened by name and
re-judged as a sweep.

DOES NOT CLAIM: no relay of vendor identity spaces, and no license for JSON-in-a-string —
unmapped fields are still fully typed.

### 4.2 THE EXEMPT SET

A THIRD category beside modeled and unmodeled: a KNOWN vendor built-in the contract deliberately
does not carry. An exempt tool's calls are DROPPED at the shim — never emitted as the unmodeled
arm (which keeps meaning "genuinely unknowable", producer-defect stance intact) and never
tripping the topbar's unmodeled warning.

The fidelity principle governs FIELDS OF MODELED tools; WHOLE TOOLS can be exempt.

MEMBERS as ruled: TaskStop, TaskOutput, TaskGet, TaskList, ToolSearch, NotebookEdit, the
background-shell peek, skip-transcript-marked ambient tasks, the undocumented `mode` disk line
(kept only as unparsed store residue), REPL, the MCP-resource family (ListMcpResources /
ReadMcpResource / RefreshMcpTools), SendFeedback, ClaudeDesign, Projects,
ShowOnboardingRolePicker, ProposeSkills.

Sketched arms for an exempt member are WITHDRAWN unlanded — the conversation protos are not
updated with dead stuff. A dropped item that later gains a real vendor route re-enters as a new
arm with its own increment and drawing.

Consequence accepted for ambient work: it is invisible on our surfaces; the vendor's level set
still governs liveness shim-side, so no indicator wedges.

### 4.3 Evidence standards

- CORPUS ABSENCE IS NOT DELETION EVIDENCE. Absence from the personal corpus proves NON-USE,
  never NON-SUPPORT. Deletion and no-producer verdicts require DOCUMENTATION-GRADE evidence —
  the SDK's doc comments, official docs, release notes, research; corpus absence is supporting
  color only.
- Recorded trap: several no-producer claims failed by inspecting the IMPOVERISHED COPY of a
  structure rather than its live source.
- DERIVED, NOT INVENTED: an arm waits for a real producer. A shape resting on a doc comment
  rather than an observation is debt for the vetting register.
- Where the vendor's vocabulary is not in evidence, a verbatim vendor string is carried and
  typed arms wait until it is.
- VOCABULARY: bare "CLI" is retired. The SDK (npm wrapper) SPAWNS the agent binary (the engine,
  owning the prompt queue, permissions, task lifecycle, compaction, plugins, OAuth). Agreed
  terms: "the agent binary", "the SDK", "the vendor".

---

## 5. Standing policies

### 5.1 THE STORE IS NUKED, NEVER MIGRATED

The store's contents will be entirely nuked, so backfilling, hydrating and migrating are
explicitly avoided; during development the store is nuked as needed and never persisted.

LICENSES. Every renaming, reshaping and re-homing may break the durable schema freely. No
migration path, no dual-read, no compatibility arm for old rows, no field retained because
persisted data names it. A durable-compatibility argument is NOT a reason to keep a shape.

FORBIDS. An implementation agent must NOT write backfill, hydration or migration code, and must
NOT preserve a message, field or arm on durable-compatibility grounds. Where existing contents
are in the way, the store is DROPPED and recreated.

RECORDED BECAUSE a fresh implementer would otherwise treat the durable store as the one artifact
that must be migrated.

CLARIFICATION kept visible: a frozen-replay premise is NOT void under this policy — it is an
OPERATIONAL prescription, merely irrelevant during development. The constraints return once the
schema ships.

### 5.2 Subagents never design protobufs

Subagents collect, classify, enumerate and research; every SHAPE is the orchestrator's sketch,
the user's agreement, the orchestrator's landing. Applied to remediation waves too: a
subagent-landed wave stands only pending a full ORCHESTRATOR REVIEW of its text and judgment
calls, brought to the user as review items and amended by hand. Ruling from that review:
anything that is NEW feature/support goes to the DEFERRED metadocument; only additions servicing
ALREADY-LANDED surfaces stay. Validity defects found on deferred shapes travel with them and
must be fixed if the shape returns.

### 5.3 The deferred metadocument

Surfaces learned about but deliberately not implemented now are parked in a third sibling
document beside the record and the vetting register, so a later PR starts from evidence rather
than re-surveying.

### 5.4 Process errors recorded as standing lessons

A COMPOUND agreement request cannot be answered separately, so it cannot be refused separately
either. A spec stated in prose is not the agreement the sketch still owes. Unasserted textual
replaces on a drifting file silently no-op — assert every replace or rebuild the block wholesale.
An RPC is presented with BOTH request and response, one at a time. Taking the on-disk
decomposition as the specification is an inversion; derive the architecture from a holistic
reading.


---

## 6. Implementation conventions

Settled at the design gate; inherited VERBATIM by the fanout's planning docs, for all five
systems, both directions (producers and consumers).

### 6.1 Proto→code mapping

- Every MESSAGE has one core implementation function ("base") per language; VALIDATION LIVES
  THERE ONCE.
- Every NON-PRIMITIVE use site (message-typed field, oneof arm) has its own dedicated, TESTABLE
  function delegating to the child message's base. Ancestry-named specializations go one layer
  deeper ONLY where a specific path has real site-specific behavior, still through the base.
- PRIMITIVES get no wrappers.
- The producer side is SYMMETRIC: build functions with the same validation at construction,
  per-site builders on top.
- NO class-per-message mandate. The requirement is dedicated testable functions and separated
  concerns, NOT a shape; module/file/class organization is the implementing agents' discretion,
  settled at orchestration planning. The anti-goal is a million unnamespaced Handle<A><B><C>
  functions.

### 6.2 The validation invariant — UNSET NON-OPTIONAL FIELDS ARE ILLEGAL, EVERYWHERE, IMMEDIATELY

- REQUESTS: a request carrying an unset non-optional field is answered with an ERROR to the
  producer at once — never "handled", never defaulted. Integration tests thereby DETECT producer
  gaps: the consumer checks the producer, and the orchestrator remediates.
- RESPONSES AND STREAM PUSHES: a non-optional response field MUST be set; a consumer receiving
  one unset RAISES A LOUD ERROR itself (on a stream there is no producer to answer), sized to be
  caught during integration remediation.
- Empty strings with required semantics are ERRORS.
- An UNSET ONEOF is an ERROR BY DEFAULT — a documented fallback only where the schema comment
  explicitly sanctions absence.

### 6.3 Logging standards for remediation

- Every logical branch carries a DEBUG statement; warnings log at WARNING, errors at ERROR —
  level discipline is part of review.
- Integration/e2e orchestration (expected to run at TWO LEVELS given the project's size) turns
  on >=WARNING logging BEFORE tests run and PERUSES the logs EVEN WHEN TESTS PASS. Any warning
  found is remediated to zero — fixed, or deliberately downgraded — never left standing.
- When issues are detected, subsequent remediation runs enable DEBUG logging to trace the path.

### 6.4 The six-orchestrator protocol

Five per-system orchestrators plus one LEAD.
- Every orchestrator and implementer reads the main design record AND their system's
  implementation doc into context BEFORE any planning or work.
- Implementers anticipate API edge cases; every anticipated case gets a unit test on the
  corresponding per-site function; when the API is unclear for a test, implementers ASK their
  orchestrator, never guess.
- Implementers NEVER change protobufs. A needed change is a REQUEST to their orchestrator, who
  triages: a true systemic oversight is surfaced to the lead AND the user; a small remediation
  goes to the lead, who vetoes or approves.
- On approval the lead broadcasts PAUSE (finish-or-abandon the current edit, never mid-file) →
  lands the proto change and REBUILDS BINDINGS → broadcasts RESUME carrying the NEW FOUNDATION
  COMMIT SHA so every system re-points at one identical contract version → orchestrators resume
  their implementers.
- Every proto-change request and its ruling gets a line in the design record: mid-flight
  contract drift stays auditable.

### 6.5 Code-level consistency requirements carried to the fanout

- ONE shared webapp link component and ONE shared Emacs subroutine ("open path[:line] in a doom
  popup, right side, half width") serve every jump/edit affordance.
- ONE renderer subroutine draws EVERY separation-divider arm; an arm selects only accent color
  and label/payload text. A per-arm divider renderer is a defect.
- The canonical attention blink cadence is specified ONCE in the schema (two blinks, 500 ms
  on/off, then steady); the webapp sidebar and the Emacs tab-bar both implement exactly that
  spec and cite the message. Divergence is a defect.
- These are owed to the fanout's structural-invariants pass.

### 6.6 The reconciliation prescription

- Reconciliation runs PER SUBSYSTEM: bindings regenerated once, then one adaptation agent per
  system (elisp, daemon, shim, store, sidecar, webapp) in isolated worktrees, merged as each
  passes.
- THE TEST RULE: any test referencing a DELETED symbol, or a RESPELLED one (pointing at a
  genuinely different structure, not a mere rename), is DELETED, never adapted. Pure renames
  adapt mechanically. An adaptation that would require deciding what behavior should now be is a
  SURFACED GAP, not an adaptation.
- REPLACEMENT COVERAGE IS ARCHITECTURE WORK, the main agent's only: for every deleted test the
  orchestrator determines the REPLACEMENT TESTS TO BE ADDED and records ONLY those forward specs
  — the deletion itself never appears in any document.
- AMENDED ROUTING: replacement coverage is prescribed for INTEGRATION tests (into each
  subsystem's implementation planning doc) and E2E tests (into the main implementation doc)
  ONLY. UNIT coverage is NOT prescribed — it falls out of the proto→code mapping convention, and
  prescribing it would duplicate that machinery's output and anchor implementers to a list
  instead of the mapping.
- DEAD CODE IS NAMED WORK: dead code the redesign stranded is ROUTED TO THE ORCHESTRATORS in
  `docs/overhaul/<subsystem>.md` — never left for discovery. Those docs carry each
  system's dead-code inventory, integration replacement specs and reconciliation gotchas;
  `MAIN.md` carries the e2e specs.

---

## 7. Terminal status

### 7.1 DESIGN-COMPLETE GATE PASSED (2026-08-27)

The consequences recap was approved whole: six surfaces settled, breaking consequences accepted
under the land-whether-or-not-it-breaks rule, the exempt set / fidelity principle / identity
ruling standing, debts routed to the vetting register and the deferred metadocument. Design
iteration is CLOSED; changes from here are vetting verdicts landing as increments, then
reconciliation, then `/cross-system-fanout`.

### 7.2 Vetting verdicts and the built-ins slate

The eleven vetting runs concluded with the contract overwhelmingly upheld; the verdicts landed
as increments — unsourced fields dropped, two shapes deleted for having no possible producer,
one vendor-declared enum value added, two slash commands falling through to the vendor as not
worth the complication, footer coverage gaps closed, producer notes corrected.

The built-ins slate resolved: six families modeled with drawn homes assigned (plan mode,
ReportFindings, the worktree pair, cron, push notification — each walking the ordinary increment
protocol of drawing agreed, then encompassing sketch agreed, then landed end to end), and the
remainder exempt.

### 7.3 DESIGN FREEZE

The frozen-contract SHA is `2d79f7501` (the push-cadence landing — the last design commit). The
Makefile's hand-maintained proto list was replaced with discovery from the source tree, so a
deleted file leaves the build the moment it leaves disk; Go and TS bindings were regenerated
once from clean for all six packages.

### 7.4 Reconciliation status

FIVE OF SIX subsystems reconciled green in isolated worktrees and merged (shim, elisp, webapp,
store, sidecar), each report's dead-code inventory, blockers and integration replacement specs
seeded into `docs/overhaul/`.

THE DAEMON's agent correctly REFUSED: the daemon was never repointed off the old packages, so
"minimum adaptation" would hollow it into an empty shell. Its re-targeting is FANOUT
IMPLEMENTATION, per the record's own routing.

Absorbed at the orchestrator: the binding regen had dropped the generated Go module's
hand-written go.mod/go.sum (restored); `protoc-gen-connect-go` joins the Makefile's Go target,
because the message-type generator alone left NOTHING able to serve the three Connect services.

### 7.5 Sanctioned post-freeze increments

Increments settled after the freeze during daemon architecture planning and feature-loss triage
are sanctioned and recorded as ordinary increments: a first derived refusal arm on prompt
submission (a prompt arriving after a merge began is REFUSED outright, never held —
post-merge-start work would be orphaned, and holding it would promise a delivery that loses
work; prompts already held stay held), and an immediate footer acknowledgement the moment an
interrupt registers. Consequence for the daemon architecture: the occupancy-lease projection
gains PER-HOLDER REFUSAL POLICY — the merge lease projects to error-on-new-submission, while
restart-pending and shutdown-drain project to holds.
