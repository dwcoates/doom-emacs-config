# FROZEN: the implementation wave for `agentshim.conversation.v1`

Foundation commit: **`0ceb094d4`** on `proto/message-pagination`. Every agent
branches from it. Bindings are generated and committed; `make validate` is green.

---

## THE RULE THAT OVERRIDES EVERY INSTINCT: NO BACKWARD COMPATIBILITY

**This is a ONE-SHOT BREAKING CHANGE and we want it that way.**

The store holds messages written under the old schema, which this wave deletes.
Those rows will not decode. Every e2e test that reads the store will fail until
the store is emptied. That is EXPECTED and it is FINE.

Therefore, for every agent:

- **Do NOT write compatibility shims, version probes, or dual-decode paths.**
- **Do NOT preserve any handling of the old messages.** If code exists only to
  read a `data.v1` shape, delete it — do not adapt it.
- **Do NOT treat an old-schema row as a case to handle.** There is no migration
  and no backfill. The store gets emptied, once, after this lands.
- **A failing test that fails BECAUSE it depends on old-schema store contents is
  not yours to fix by making the code lenient.** Say so and move on.

A compatibility path added here would outlive the wave and become the permanent
answer to a problem that exists for one afternoon. The whole point of the
overhaul is that the daemon and the webapp never see a vendor shape again; a
decoder that still accepts one keeps the door open.

---

## The architecture, per subsystem

### A. `agent-shim/claude/shim` — the STREAM plane producer (TypeScript)

**Produces.** `Entry` records with `internal.plane = PlaneStream`, plus the two
ephemeral records that bypass the store.

- Durable, written to the store: `BookkeepingEntry` — `SessionBegan`,
  `SessionEnded`, `TurnBegan`, `TurnEnded`, `SessionIdentityChanged`,
  `ProducerDiagnostic`, `ResponseTiming`, `AccountUsageObservation`.
- Durable message records the stream plane owns: `PermissionAsked`,
  `PermissionAnswered`. The shim sees the SDK's permission exchange; the file
  plane does not carry the answer.
- Ephemeral, NEVER written: `ContentArriving` and `Heartbeat`, handed to the
  daemon as `LiveEntryDelivery` — which has **no seq field**, so nothing can
  advance a resume cursor on them.

**Deletes.** Every `data.v1` construction site. `ClaudeStreamMessage` and its
arms become the shim's own internal parse types in TypeScript — not a schema,
not on a wire.

**The lifecycle/content split.** The shim owns TURN AND SESSION LIFECYCLE. It
does NOT write conversation content: `UserSaid`, `AgentSaid` and `ToolReturned`
come from the sidecar, which reads what the vendor actually recorded.

**`SessionBegan` is now much richer** and the shim fills it: `agent_version`,
`SessionAuth`, `output_style`, `FastMode`, `skills`, `subagents`,
`mcp_servers` with health, `plugins`, `memory_paths`. These are the facts the
`/status` panel is resolved from. The shim already has all of them — it is what
`SystemInit` carried.

**`AccountUsageObservation` moved** from `core.v1` into
`conversation.v1` `BookkeepingEntry`. Same shape, same emission point
(`uds-session.ts`, alongside `TurnStarted`), new package.

### B. `agent-shim/claude/shim-sidecar` — the FILE plane producer (Go)

**Produces.** `Entry` records with `internal.plane = PlaneFile`, carrying the
conversation itself.

- `UserSaid`, `AgentSaid`, `ToolReturned`, `ContextCut`, `FailureRaised`,
  `DetachedWorkStarted` / `Progressed` / `Ended`, `SkillBodyResolved`.
- Anything it cannot convert goes to `InternalEntry.unconverted` —
  `VendorSpecificEntry`, `UnknownEntry`, `UnparsedEntry` — which has NO external
  half and therefore no path to the daemon.

**Owns lineage, which is the biggest change.** The daemon used to reconstruct
this; now the producer states it.

- `message_id` — the vendor's per-message uuid. `LineEnvelope.uuid` on disk. The
  shim's `ContentDelta.uuid` is THE SAME VALUE, which is what lets a client
  retire a live preview by matching ids. Do not invent a new identity.
- `parent_message_id` — `LineEnvelope.parent_uuid`, already on every line.
  Empty on the first. ONE HOP, never the root.
- `top_level_message_id` — the root of the parent chain. Propagate it: a
  record's root is its parent's root, and reading in file order guarantees the
  parent was seen first. One map, not a correlation pass.
- **At a compaction the physical chain is CUT.** The boundary line carries no
  parent, and a separate logical pointer is the only link back. Resolve
  `parent_message_id` from that pointer there; walking only the physical chain
  makes pre-compaction history unreachable.

**Deletes, and does not port.** `LineEnvelope` was a census artifact that
reflectively matched disk keys and spilled the rest to `extras`. It has no
successor. `TranscriptLine`'s fifteen line kinds do not cross a wire again.

**Detached work.** A skill invocation OWNS ITS WINDOW — it is a feed row, so
every record it produces names the skill's message as `top_level_message_id`.
`DetachedWorkKind` has six arms: `agent`, `shell`, `workflow`, `unclassified`,
`skill`, `merge`. `DetachedMerge` is the one detachment with an EMPTY
`origin_tool_call_id`, because no tool spawns it.

### C. `agent-shim/shim-store` — persistence (Go)

**Persists whole `Entry` records** — both halves. Assigns `seq` at write.
Dedups on `internal.dedup_key`, deriving one from the record's identity when
empty.

**Forwards ONLY the external half.** `entry.external`, wrapped in
`StoredEntryDelivery{seq, entry}`. This is a field access, not a conversion —
there is no projection function to keep in sync.

**An entry with no external half is persisted and never forwarded.** That is
what makes an unconvertible record unable to reach a page: not a filter, an
absent field.

**The page query** selects the `message` arm, groups by `top_level_message_id`,
and counts GROUPS — ten messages, not ten records. Bookkeeping is retrieved by
SEQ RANGE, which is a different query, and is never counted toward a page.

### D. `daemon` — the consumer (Go) — `opus-medium`

**Consumes `core.v1.EntryDelivery`.** Advances its resume cursor ONLY from the
`stored` arm; the `live` arm has no seq field to advance from.

**MUST NOT import `agentshim.conversation.internal.v1`.** `make
conversation-isolation` fails the build if it does. Everything the daemon is
entitled to see is reachable from `conversation/v1/external.proto`.

**Deletes outright — these are reconstructions the contract makes unnecessary:**

- The four `plane` checks (`ssm/turnboundary.go:146`, `ssm/turnclaims.go:201`,
  `sessioncontroller/turnlifecycle.go:192`, `shimclient/events.go:420`). The
  daemon cannot read `plane` any more, and does not need to: authority is in
  the arm.
- `frontend/detachedsplit.go`'s `detachedWorkStore` and its
  `if key == "" { key = "agent:" + AgentID }` fallback. Grouping is now
  `top_level_message_id`.
- `sessioncontroller/skillbody.go`'s `skillCorrelator`. The skill body carries
  the skill message's own `message_id`.

**Resolves, per figma→idl — the daemon decides, the client renders verbatim:**

- `SessionInitView.rows` — `repeated SessionInitRow{label, value}` in render
  order, resolved from `SessionBegan`. Omit a row rather than pushing a blank.
  The old client-side derivation is retired.
- `FailureCardView` — daemon-synthesized failure cards, INCLUDING recovery
  classification (`internal/errclass`). `conversation.v1.FailureRaised` is only
  the vendor's own recorded error and carries no recovery judgement.
- `DaemonInterceptedCommandItem` — unchanged, still daemon-minted and ephemeral.
- `TurnAccounting` — the turn's token total is still the daemon's aggregation
  over many `AgentSaid.usage` values. `usage_at_start` / `usage_at_end` now name
  `conversation.v1.AccountUsageObservation`.

**Order of work, because the packages are a chain:** `internal/frontend` (378
refs) → `internal/progress` (150) → `internal/sessioncontroller` (705) → the
rest (~228). Anything else compiles only once the layer below it does.

### E. `webapp` — the renderer (TypeScript)

**Renders the neutral content model.** `UserContent`, `AgentContent`,
`ToolResultContent` — narrow per site, so a user message carrying reasoning is
unrepresentable rather than merely wrong.

**Retires the `/status` derivation entirely.** `statusSnapshotFromInit`,
`apiKeySourceWord`, `authLabel`, `fastModeLabel`, `pluginLabels`, `str`, `arr`
and the `StatusSnapshot` view-model all go. The panel prints
`SessionInitView.rows` and splices its own rows (account, model, permission
mode) ahead of them.

**Routes by `top_level_message_id`.** A skill's window, a subagent's fold and a
shell's spool are all "records naming this message as their root". No identity
ladder, no nearest-preceding heuristics.

**MUST NOT import `agentshim.conversation.internal.v1`.** Same gate as the
daemon.

### F. e2e authoring

Writes tests against the frozen contract. Runs nothing. See its prescription.

---

## Cross-cutting rules every agent receives

1. **Your changes are ISOLATED to your own system.** Do not touch another
   system's code, even to fix a compile error you can see there.
2. **EXPECT every other system to be broken for the whole wave.** That is the
   steady state, not a signal. A broken sibling is never evidence that your own
   understanding is wrong.
3. **Write and run UNIT tests for your change only.**
4. **Do NOT write or run integration or e2e tests.** Authoring them belongs to
   one dedicated agent; RUNNING them belongs to the orchestrator alone.
5. **No backward compatibility.** See the top of this file.
6. **ESCAPE HATCH: when you hit something this prescription does not cover,
   STOP and surface it. Do not improvise.** An uncovered case is the same kind
   of event as a contract deviation, and guessing is how a local decision
   becomes a cross-system disagreement.
