# COLLECTED GAPS — implementation wave two

Six agents rewrote six subsystems against the frozen five-surface schema, in
isolation, under a standing rule that no agent could touch a `.proto` file for
any reason. This is what they hit, deduplicated.

**Nothing here is fixed.** Per the gap protocol in
`DESIGN-protobuf-surfaces.md`, a gap found mid-implementation is evidence, not a
verdict — it may be a real omission, bad modeling, an abstraction leak, or a dead
feature, and that judgment needs the whole set in view.

## How to read the convergence column

The agents could not see each other. When several independently reported the same
hole from different sides of a wire, that is the strongest evidence available
here that the hole is real rather than one agent misreading the contract. When
exactly one reported something, it is worth checking whether that agent simply
owned the only code that cared.

| # | gap | found by |
|---|---|---|
| 1 | the store write has no receipt | wire, shim, store, sidecar |
| 2 | no per-record correlation identity | daemon, shim |
| 3 | inference has no plane to record under | store, sidecar |
| 4 | a turn cannot say it failed | daemon, shim |
| 5 | ephemerality has no write-side expression | store, sidecar, shim |
| 6 | the feed lost permission and context-cut | daemon, webapp |
| 7 | open-task recovery is underivable | sidecar, store |
| 8 | the ungated-session check fails OPEN | webapp |
| 9 | five messages have no carrier | daemon, shim, sidecar |
| 10 | a bounded replay serves records with no position | daemon, store |
| 11 | the feed cannot carry a `MessageEntry` | daemon, webapp |

---

# A. Blocking — a decision becomes unmakeable, not merely undisplayable

## 1. The store write has no receipt, and the ack was load-bearing twice

`StoreWriteAck` was deleted with `StoreWrite`. `StoreEntryWrite` has no reply.

Four agents hit it, and between them it is two distinct losses:

**As a receipt.** A producer cannot learn that its write landed — which is
precisely what `write_id`'s replay-idempotency contract depends on, making that
contract unfalsifiable from the producer's side. Nothing can report `accepted`,
`last_seq`, "absorbed as a replay", "stored but unconvertible", or "rejected, and
why". The sidecar now commits its cursor on "reached the socket" rather than
"durable", so a store that takes the bytes and then fails to persist loses
records with nothing able to detect it.

**As the connection's alternation invariant — the finding nobody anticipated.**
`storeclient.Write` holds the mutex across `WriteAny` AND the following blocking
`ReadAny`; `Heartbeat` and the health probe do the same on the SAME connection.
The producer connection is therefore strictly alternating, and that alternation
is the ONLY reason correlation works, because the frame envelope carries no
per-frame correlation id. Remove the read leg and a heartbeat echo can silently
consume a frame belonging to something else.

So restoring a receipt and designing frame correlation are the same question, and
`wire` cannot paper over it — it is deliberately stateless per frame.

**Also an error channel.** `StoreWriteAck.error` turned a rejected batch into a
returned error. Its removal is schema-level removal of failure-surfacing, which
no implementing agent was permitted to compensate for.

**Current state.** The store drops the producer connection on a rejected batch,
because silence and success are otherwise indistinguishable. That is observable
but carries none of an ack's information and destroys the connection to deliver
it. The shim's entire hold/spill/flush/backpressure state machine is unbuilt.

**Read: real omission.** Highest severity in the set.

## 2. No per-record correlation identity — `query_instance_id` and `request_id`

`ExternalEntry` carries neither. Found from both ends of the wire independently.

- `query_instance_id` survives only on `ShimHello` and `QueryLifecycle`, and
  `QueryLifecycle` itself has no carrier (gap 9). The daemon's single
  historical-vs-live classifier, `eventIsHistorical`, classifies a `TurnEnded` —
  which now carries no query stamp. Losing it reverts the fix for a logged
  incident that left a workspace unopenable.
- `request_id` was the independent second witness that a payload belongs to the
  turn it names — the accounting ledger's anti-misattribution mechanism. Only
  `TurnBegan`/`TurnEnded.turn_id` survives, so no non-boundary record can be
  joined to its turn at all.

**Read: real omission.**

## 3. An inferring producer has no plane to record under

`agentshim.v1.Plane` has two arms. Its comment justifies cutting the third:

> An earlier draft had a `synthetic` arm for the daemon stating something it
> inferred. The daemon has no store write path at all […] so that arm named a
> producer that cannot exist here.

**That rationale is factually wrong, and the error is mine.** The daemon indeed
has no store write path — but the arm's actual users were never the daemon. Both
live in the sidecar: the stale-task sweep (`internal/stale/stale.go:282`) and
`link.go:313`. Both are inferences by a producer that DOES write to the store. I
verified the daemon could not write, concluded the arm was unreachable, and never
checked who was setting it.

**Consequence.** The store rejects a batch whose plane is unset, so a
sidecar-inferred record must claim `file` — asserting it was read from disk when
it was not. A LOST sweep and an outage report are now indistinguishable from
transcript reads.

**Read: real omission resting on a wrong premise.** Found independently by the
store (enforcement side) and the sidecar (producing side).

## 4. A turn cannot say it failed

`TurnEnded` lost `is_error`, `stop_reason` and `duration_ms`. No arm anywhere can
state that a turn ended in a vendor error.

`unexplained` exists but is for a daemon-SYNTHESIZED close and must not stand in
for an observed vendor failure — using it would make the two indistinguishable.
This breaks `errclass.TurnEnd`, the progress footer, and
`ssm.VendorBlockingTurnEnd` (the purple workspace). Turn duration is underivable:
`ResponseTiming.total_ms` is per-message, not per-turn.

`internal/errclass` is the keystone here — it blocks nine daemon packages, and
compiling it would mean deriving "this turn failed" and "this error is still
retrying" from evidence the schema does not carry.

**Read: real omission.**

## 5. Ephemerality has no write-side expression

The old `EventClass.EPHEMERAL` let a producer say "fan this out, never store
it" — it carried the typing preview and tool progress.

`EntryDelivery` states the distinction BY SHAPE, and the design treats that as
sufficient. It is sufficient only on the READ side: `LiveEntryDelivery` is
producible only by something handing a record straight to the daemon. **The write
surface has no field a producer can use to request it.**

So `conversation.v1.MessageEntry.content_arriving` — documented as "a fragment of
content still arriving, handed straight to the daemon by the stream plane" —
becomes a durable row if written through the store. The sidecar's degraded
reports, deliberately ephemeral because "an operational notice about the pipe
belongs in the live stream, never in the durable conversation history the pipe
carries", are now durable.

**Read: either a real omission, or a deliberate routing change (the shim delivers
live records directly, bypassing the store) that is stated nowhere.** Undecidable
from the protos alone, which is itself the finding.

## 6. The feed lost permission and context-cut, and one half of each survives

Reserved on `frontend.v1.Message`: `permission` (30), `context_cleared` (32),
`context_compacted` (33).

- `PermissionAnswerCmd` survives. **A client can answer a permission it can no
  longer be shown.**
- `conversation.v1.PermissionAsked`/`PermissionAnswered` exist as `MessageEntry`
  arms, and nothing forwards them.
- `CompactionSummaryItem` covers the summary block but not the boundary rule, the
  tokens-before/after figures, or the clear divider. `CompactDivider` and
  `ClearDivider` remain reachable with no producer.
- `slash-menu.proto` still documents itself as "the invocation, not the outcome",
  and the outcome half no longer exists anywhere.

A permission request and a context cut are both MESSAGES by the feed's own test,
so these are holes in the feed rather than tidy-ups.

**Read: real omission.**

## 7. Open-task recovery is underivable

`OpenTaskState.started` carried the task id, kind, session and output path.
`DetachedWorkStarted` has no `output_path`.

Four behaviors lost:

- A task open across a restart is untracked, so never LOST-swept — it sits as
  running forever.
- `bootSweep` has nothing to sweep; tasks killed by a reboot stay running.
- The spool-owner index cannot be seeded, so a live task's spool is unattributed
  and therefore unread until its transcript is re-read.
- `ownerByOutput` and `ownerPathConflicts` are seeded by nothing, making the
  path-conflict detection code unreachable — that was the authoritative
  resolution surviving two sessions reusing one task id.

`CursorList.open_tasks` still exists and is still validated as authoritative, so
**the schema asks for a set it gives no way to construct.** The store now always
returns empty with `open_tasks_authoritative=false`.

**Read: real omission.**

## 8. The ungated-session check now fails OPEN — a safety regression

`SessionBegan` has no permission-mode field. Verified against the whole tree:
every `permission_mode` spelling is either the registry's REQUESTED mode
(`frontend.v1`) or a per-prompt override (`protocol.v1`).

The ungated-session warning ORed the daemon's registry view against the effective
mode the CLI itself reported at init. **Only the second operand catches a
settings-borne escalation** — the shim loads `settingSources: ["user", "project",
"local"]`, so a user's own `permissions.defaultMode` in settings.json escalates a
session the daemon believes is `default`. The init record is gone, so the OR has
one operand, and such a session now renders no ungated banner and no `.ungated`
body class.

`FROZEN-implementation-wave.md` item 7 records this as a SETTLED decision —
"permission mode is modeled neutrally on `SessionBegan`… it closes a fail-OPEN
gap". **It was settled and never landed in the schema.** The client-side
compensation has now been removed, so the gap is live rather than latent.

Secondary: `SessionInitView.rows` is label/value strings, so a panel could
display the mode while nothing can branch on it.

**Read: real omission, and the only one in this set that is a safety regression
rather than a fidelity loss.**

## 9. Five messages have no carrier

`QueryLifecycle`, `TurnClaimBridge`, `SessionRewound`, `KeepAliveDiscard` and
`DegradedState` are declared on `protocol.v1`, referenced on master, and
reachable only through the deleted `Event.payload`. No envelope on the new model
can carry one, so a producer has no way to send one.

`advanceDurableCursor` loses two of its three gates entirely and the third by
half — the crash-recovery guarantee that a resume lands on a complete turn or
termination pair is gone.

`DegradedState` is the sharpest case, and it points two ways at once:
`shimclient/events.go:388` mutates the DURABLE cursor from it, while the sidecar
used it to open and close an ingestion-outage WINDOW. Rerouted to
`ProducerDiagnostic`, the reason text survives and the window does not, so
workspace health no longer learns the file plane was down.

**Read: real omission** — though `DegradedState`'s durable-cursor read may also
be an abstraction leak worth separating.

---

# B. Modeling questions the wave surfaced

## 10. A bounded replay serves records with no position

`ReplayEntry` wraps a bare `ExternalEntry`, which deliberately has no seq.
`Subscribe` delivers `EntryDelivery`, which carries one.

So the BOUNDED replay path loses position entirely while the standing one keeps
it — for records that are by definition all durable and all positioned. Every
floor-advancing caller needs a position.

**Read: bad modeling in the NEW schema.** `ReplayEntry` probably wants
`StoredEntryDelivery`. Found by the daemon and the store independently.

Related: `StoredMessage.records` is `repeated ExternalEntry`, so a consumer
holding a page cannot address or resume from an individual record — only
re-anchor the whole page. Probably intentional, stated nowhere.

## 11. The feed cannot carry a `MessageEntry`, so passthrough is not literal

`frontend.v1.Message`'s payload oneof has no arm for a
`conversation.v1.MessageEntry`. Its arms are `AgentEmission`, `UserContent`,
`FailureCardView`, `DaemonInterceptedCommandItem`, `DetachedWork` and
`CompactionSummaryItem`.

The daemon must therefore still re-encode `MessageEntry` payloads into
`AgentEmission`. **Translation did not disappear; it stopped being VENDOR
translation.** `translate.go` cannot simply be deleted, and the webapp's
instruction to switch on the `MessageParent` oneof is unimplementable client-side
because no `MessageEntry` reaches a frontend.

The design document recorded both consequences as accepted and has been corrected.
The open question is whether the feed should GAIN a `MessageEntry` arm — making
passthrough literal — or keep re-encoding by design.

## 12. `Plane` was arbitrating, not decorating

Three daemon sites hard-refuse records to de-duplicate the twin observation
planes: `turnlifecycle.go:192`, `ssm/turnboundary.go:146`, `ssm/turnclaims.go:201`.

The log-only reads of `plane` ARE the abstraction leak the split was built to
remove, and should go. These three are different: they have no replacement
discriminator, and deleting them re-admits double-counted turn boundaries.

**This is the leak and the load-bearing use sharing one field** — which is why
removing the field wholesale was too blunt.

## 13. An `Entry` cannot be partially convertible

`InternalEntry.unconverted` is documented as "set when there is nothing to hand
the daemon", and `Entry.external` is what may leave. A record with BOTH a
renderable projection AND unmodeled structure has nowhere to put the second half.

Concretely: a workflow journal record renders to a `DetachedWorkProgressed` line,
and its verbatim JSON is then lost. This directly weakens the total-ingestion
mandate — "no data is lost as a Struct" was the old escape hatch.

**Read: real omission.**

## 14. The `message_delta` usage correction has no home — quantified

The vendor reports a response's FINAL `output_tokens` on `message_delta` "and
nowhere else the daemon can see". One measured turn summed **563 against the
result's 4407**.

`TokenUsage` rides `AgentSaid`, which is file-plane. `BookkeepingEntry` has no
usage arm, since `UsageObserved` was deliberately removed in favor of
`AccountUsageObservation`. So the correction has no carrier from the stream
plane.

**Read: real omission, and the only one in this set with a measured magnitude.**

## 15. Routing: two surfaces still misfiled by this design's own test

- **Store↔sidecar health traffic.** `ConnectionHeartbeat`, `HealthCheck` and
  `HealthStatus` are in `protocol.v1` and never cross the daemon boundary. Same
  class as the cursor messages, which moved to `agentshim.v1` for exactly this
  reason.
- **`frontend.v1` imports `protocol.v1`** for `PromptOrigin`, `InterruptOutcome`,
  `QueryRuntimeIdentity` and four query-failure arms. The dependency rule
  sanctions `conversation` as the shared leaf and says nothing about this edge.
  Each is a shim-wire type reaching a client directly.

---

# C. Fidelity losses — real, lower severity

- **`ContentArriving` has no `signature` arm**, and `ThinkingBlock` carries no
  signature either, so the thinking-block attestation is lost from the durable
  model too.
- **`ContentArriving` has no `estimated_tokens`** — drove the live thinking-token
  counter.
- **`SessionBegan` has no `source`.** `SessionSource{FRESH, RESUME,
  COMPACT_CONTINUE}` survives with nothing producing it, so a resumed session is
  indistinguishable from a fresh one.
- **`SessionIdentityChanged` cannot state what the identity changed TO** — it has
  `previous_session_id` only, so rotation lineage is not walkable forward.
- **`Heartbeat` lost tool name, parent tool call and elapsed seconds**; only
  `live_work_ids` survives. Producer-side face of the `HeartbeatView.progress`
  gap.
- **`ResponseTiming` cannot be produced whole by one plane** — `first_token_ms`
  is stream-plane, `total_ms` only from the turn's terminal result.
- **`ProducerDiagnostic` loses six queryable fields** from `FilePlaneDiagnostic`:
  `source_runtime`, `level`, `verbosity`, `context`, `source_pid`, `source_path`.
  Flattened into prose, so a consumer that filtered on level now parses text.
- **A record with no external half carries no session identity** —
  `session_id` lives only on `ExternalEntry`, so "which conversation did we fail
  to parse a line for" is unanswerable. Same root cause: an unconverted record
  also has no producer clock.
- **`ToolReturned` has no way to name an unresolved caller** — after a restart
  mid-file, results whose calls sit behind the cursor have no legal parent and
  become `UnknownEntry`.
- **`ImageBlock` carries a reference; the vendor carries bytes.** Transcript
  images are inline base64 with no path or URL, so every pasted or screenshot
  image is unrenderable unless some component materializes bytes to disk first.
- **`system/turn_duration` has no home** — carries pending background-agent and
  workflow counts that drive footer state.
- **`TypingDelta.delta`** — the content DOES exist on `conversation.v1`'s
  `ContentArriving`; only the frontend spelling is missing. Cheapest in the set
  to close.

---

# D. Confirmed non-issues — worth recording because they were suspected

- **The daemon has no store write path.** Verified exhaustively for a second
  time. `StoreWriteAck`'s victims are the shim and sidecar, not the daemon.
- **`dedup_key`, `write_id` and `extras` are read by zero daemon files.**
- **`TokenUtilization` and `TurnAccounting` moved to `state.v1` field-for-field
  identical**, so durable replay is byte-safe.
- **`protocol.v1.Heartbeat` and `ConnectionHeartbeat` cannot collide in the
  demux.** The bookkeeping `Heartbeat` is only ever a payload arm inside
  `BookkeepingEntry`, never a top-level frame, so no receiver's type switch sees
  both. Two distinct `type_url`s. Safe — but adjacent in one Go package, which is
  a live footgun for a reader.
- **`dedup_key`'s per-producer keys are covered by `write_id`.** The one
  exception is the `wf:` workflow key, whose `run_id` lives in the journal file
  PATH rather than any payload — the reservation's stated justification does not
  cover that case.
- **`TaskKind`/`TerminalStatus` are fully replaced** by `DetachedWorkKind` and
  the `DetachedWorkEnded` outcome oneof, which is strictly richer.
- **The shim producing no durable conversation content is correct, not a
  defect.** `MessageEntry.parent` requires the producer to state root-or-inside
  "or fail", and the SDK stream carries no parent pointer. The file plane
  resolves lineage from the JSONL parent chain and produces the record model.
  The two planes divide cleanly.

---

# E. Schema-internal contradictions found in passing

None of these break a build; all are the schema describing itself wrongly.

- `state/v1/durable.proto` declares `TelemetryRecordMissingQueryLifecycle`, a
  durable problem code naming a record nothing can produce.
- `tool-call.proto`'s `TaskEntry.kind` says it "mirrors `DetachedWork`'s kind
  arms" while having 4 to `DetachedWorkKind`'s 6 — no `skill`, no `merge`.
- `feed.proto:460` places `SessionBegan` in `conversation.v1`; it is in
  `protocol.v1`.
- `permission-card.proto`'s header describes the request arm as
  `protocol.v1.PermissionItem` "carried on `Message`" — the field it now reserves.
- `agent-emission.proto` and `tool-call.proto` cite `agentshim.data.v1.
  ToolUseResult` and `internal/frontend/translate.go`, both deleted.
- `core.proto:724` still asserts the shim "copies this value onto the persistent
  `TurnStarted` event" — a sentence about a message that no longer exists. It
  also makes `PROMPT_ORIGIN_CACHE_KEEP_ALIVE`'s documented "every consumer
  excludes them unconditionally" unimplementable, since `TurnBegan` lost
  `prompt_origin`.
- `CursorList.open_tasks_authoritative` justifies itself as distinguishing "an
  older store that does not understand field 2" — backward-compat framing that no
  longer applies, though the field is now load-bearing for a different reason.
- `agent-shim/AGENTS.md` still documents `agentshim.core.v1`/`agentshim.data.v1`
  as the packages that tree owns, and its worked example turns on a
  `core.v1`-vs-`frontend.v1` test that no longer exists.

---

# F. Build state at the close of the wave

| scope | state |
|---|---|
| wire + logging | green — needed no change, schema-agnostic by construction |
| store | green — builds, vets, `-race -count=2` across 6 packages |
| sidecar | green — builds, vets, `-race -count=2` across 8 packages |
| webapp | green — 4965 tests, typecheck clean |
| shim | 510 tests pass, 19/30 files green; 11 blocked on gaps 1 and 6 |
| daemon | 33 packages pass, 11 fail to build; blocked on `errclass` (gaps 2, 4) |

Every blocked agent stopped at the gap and recorded it rather than improvising a
signature, which is what the wave was for.

Two agents also found real bugs in their own new code by testing it: the sidecar
was filing every unmodeled line type as `vendor_specific` (claiming an
understanding nobody has) and emitting an empty `UserSaid` alongside pure
tool-result carriers.

The webapp additionally removed **2,848 lines of client-side derivation** —
turn accounting reconciled client-side and the per-model breakdown rebuilt from
the persistence aggregate, both second authorities that could disagree with the
daemon with nothing comparing them. It also read `AccountUsageObservation` and
`UsageWindow` directly, which are `BookkeepingEntry` payloads that by this
design's own rule never reach a client.

**One coverage note that is not a gap.** `frontend.v1`'s message-arm key sets are
hand-written `new Set([...])` rather than the generated field-set helper, so the
compile-time manifest that catches every other schema drift does not cover them —
gaps 6 and the `TypingDelta` loss are invisible to `npm run typecheck`.
