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
