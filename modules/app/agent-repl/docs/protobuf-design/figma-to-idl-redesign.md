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

## Landed changes

### RECURSION IS RETRACTED: subagent activity is FLAT, attributed by `agent_id`; and the workflow body lands

**What changed ("okay go for it").** `AgentActivity.subagent_activity` is
DELETED. `AgentSubagentStart` gains `created_agent_id`. The `AgentWorkflow` body
lands with no recursive arm. `message.proto` is deleted (see its own entry).

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

**THE WORKFLOW BODY, and what the evidence allowed.** `AgentWorkflow` =
{ start | success | failure }. Start carries the run's name and identity, a
`source` oneof (inline script, named, path, resumed) and a `placement` oneof
(local, remote), plus an optional non-blocking warning. Success carries an
optional summary. Failure carries an OPTIONAL run identity — a script rejected
before launch never had one — over a cause oneof separating a rejected script from
a run that started and ended.

**No update arm and no enumeration, both on evidence.** A run's journal holds only
`{type: started|result, key, agentId, result?}` — one pair per agent call — which
is exactly what those subagents' own units already state. And the run's
constituents are not listed anywhere on this unit: a consumer learns them by
asking for the items of an agent it saw created, like any other agent's work.

**A LANDED CLAIM CORRECTED.** `detached_work.proto` asserts that "a journal file
states a step's label, its detail and its status separately". FALSE against the
real file, which has no label, no detail and no status. Nor does the sidecar
produce that shape: it flattens each record to a prose line, and its own comment
concedes "THE RENDERING IS LOSSY AND THAT IS A KNOWN COST… a journal record's
structure does not survive into the store."

**PHASE GROUPING HAS NO PRODUCER, and that is an evidence gap rather than a
choice.** A script declares its phases in `meta` and each agent call may name
one, but NEITHER reaches any readable artifact — the journal has no phase and the
per-agent meta holds only `{agentType, spawnDepth, model}`. Recovering it would
mean interpreting the script's control flow. So the fan-out-by-phase view a
workflow's data seems to invite cannot be drawn, and making it possible is a
PRODUCER change, not a schema one.

**A workflow is ALWAYS detached** — the producer's status values are only
`async_launched` and `remote_launched`, with no synchronous form. Worth
contrasting with the subagent, where the same suspicion was raised and the
producer turned out to offer a synchronous path after all.

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
