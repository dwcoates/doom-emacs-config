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

1. **`conversation.v1`** — `message` → `tool_call` → `agent` → `user` →
   `content_blocks` → `context_cut` → `api` → `detached_work` →
   `tokens` → `session_command` (the per-concern file set after the folds
   and split recorded under Landed changes; originally `message` →
   `payloads` → `content` → `tokens` → `session_command`).
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
`tokens` before `session_command` is arbitrary between two leaves. This
convention is being added to the `/create-or-update-protobufs` skill itself by
a one-shot workspace dispatched at the same moment.

**Retracted by this amendment.** The `content.proto` increment sketched
before the amendment (ThinkingBlock as a two-arm oneof, ImageBlock's location
split into path/url arms, UnsupportedBlock.raw's exception stated at the
field, ToolCallBlock.arguments deferred to its own increment) is WITHDRAWN
unagreed and returns when `content.proto` comes up in the top-down order — by
then `payloads.proto` will have said what it needs from it.

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

## Landed changes

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
  record's prose and in git history (`4b0d6aa4c^`), not on disk. When an
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
