# Vetting register → docs/overhaul coverage audit

Scope: every item and every "Owed to later stages" entry in
`docs/protobuf-design/figma-to-idl-redesign.vetting.md`, checked against the six
system documents under `docs/overhaul/`, the two prompt documents, and
ORCHESTRATION-META.md. An item counts as ADDRESSED when an overhaul document
states it, implies it necessarily, or carries the ruling that superseded it.
Contract files under `proto/src/` were consulted only to settle whether a
superseding ruling had in fact landed.

Findings are ordered most significant first.

## 1. Item 1's RULINGS-OWED slate was never ruled, and no overhaul document
   tells an implementer what to do with the affected arms

- WHAT THE ITEM REQUIRES. Item 1's run (2026-08-27) closed with an explicit
  "RULINGS OWED (see slate)" list: `AgentPermissionAbandoned` (the SDK says
  permission prompts "have no park deadline", so the arm may be unproducible),
  `AgentPermissionDeniedForWantOfDecider` (no declared discriminator),
  `SessionIdentityRotated.reason` (no source), `ContextCompacted.
  cumulative_dropped_tokens` + `tools_before_cut` and `ContextCleared.tokens`
  (no declared source), and `AgentEffortLevel` missing the vendor-declared
  `max`. Each is a landed declaration whose producer the run could not find.
- WHY IT IS NOT COVERED. The arms are still in the frozen contract
  (`conversation/v1/permission.proto:311,316` declares
  `AgentPermissionDeniedForWantOfDecider`; the abandoned/rotation/context-cut
  fields likewise stand), and no overhaul document mentions any of them. The
  words "abandoned", "decider", "rotation reason", and the two context-cut
  token fields appear nowhere in `docs/overhaul/`.
- CLOSEST NEAR-MISS. `shim.md`'s permission section enumerates the shim's emit
  points exhaustively — "`AgentPermission.start` from the gate callback;
  `success` from the shim's own resolve; `denied.by_policy` from the vendor's
  system permission-denied message" — and stops there. That list is precisely
  where the two unruled permission arms would have to appear, and they do not,
  so an implementer reading it will simply never fill them and will not know
  that is a known-open question rather than an oversight.
- CONSEQUENCE. The validation invariant ("unset non-optional fields are illegal
  everywhere") makes this worse, not better: an arm nobody can produce is
  indistinguishable, at the wave, from an arm the implementer missed.

## 2. Deferred item 5's capture harness has no owner, and the project lead's
   mocked-vendor mandate appears to forbid the very captures two systems' test
   specs depend on

- WHAT THE ITEM REQUIRES. Item 5 (the only DEFERRED item, ruled at the
  design-complete gate) requires capturing REAL transcripts from the actual
  agent binary, diffing them against `fake-query.ts`, and rebuilding the fake's
  scripts FROM captures so it can no longer agree with us by construction.
- WHY IT IS NOT COVERED. Two subsystem documents consume the harness as a
  dependency but neither creates it: `shim.md`'s replacement spec says
  "SDK→conversation.v1 conversion: golden transcripts (real captures per
  deferred vetting item 5)", and `sidecar.md`'s says "golden real captures …
  (rides item 5's capture harness)". No document assigns building it, and item
  5 is named nowhere else in `docs/overhaul/`.
- CLOSEST NEAR-MISS, AND THE TENSION IN IT. `prompts/PROJECTLEAD.md` §"The
  mocked vendor — a hard prerequisite" mandates a mock shim SDK before any e2e
  test or playtest, with "no test or playtest may ever make a real Claude call".
  That is the nearest thing to an owner, but it prescribes the OPPOSITE
  artifact: a hand-built mock is exactly the structurally-unverifiable fake item
  5 exists to retire, and a blanket no-real-calls rule reads as forbidding the
  capture run. Nothing reconciles the two, so the likely wave outcome is a
  second hand-written fake and two integration suites resting on golden files
  that were never captured.

## 3. Item 11's whole follow-through — pin the SDK version, add a loudly
   failing test — is absent, and the panels it named have dissolved without
   the ruling being recorded

- WHAT THE ITEM REQUIRES. Item 11's stated verification method is an
  implementation-wave action, not a design question: "pin the SDK version, call
  the method against the pinned version, and add a shim test that fails loudly
  when the method disappears on an upgrade." Its run also raised a CONSEQUENCE
  with a "user ruling owed": `get_usage` is literally named
  `usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET`, and
  `get_session_cost` has no declared response type at all.
- WHY IT IS NOT COVERED. No overhaul document mentions pinning the SDK version,
  and neither `shim.md`'s dead-code list nor its four replacement integration
  specs contains an upgrade-canary test. The owed user ruling
  (rely-and-absorb vs degrade to composed text) is recorded nowhere.
- CLOSEST NEAR-MISS. Two partial absorptions exist and should be read as
  evidence the question was half-answered rather than answered. `daemon.md`
  states "Money/cost is deliberately absent from the entire API" and
  `frontend/v1/` carries no cost or usage panel file, which retires
  CostPanelView; and `conversation/v1/session.proto:331` gives
  `SessionAccountUsage` an `unavailable` arm, which absorbs a MISSING method at
  runtime. Neither addresses a method that still answers but RESHAPES — the
  failure mode the pinned-version canary exists to catch — and neither records
  that the /cost half was resolved by deletion.

## 4. Item 6 was ruled only halfway: the note was retired in the contract, but
   `free_text` survives with no producer and the webapp is told to always draw it

- WHAT THE ITEM REQUIRES. Item 6's run REFUTED the two-facts assumption at
  documentation grade over 206 real answer payloads: the wire carries only
  {questions, answers, annotations}; `answers` is ONE string per question, a
  selection-plus-typed-text arrives as a single comma-joined string
  "structurally indistinguishable from a multi-select label join", and
  `annotations` only ever echoes model-authored option previews. The item
  explicitly states that if the substitute-for-a-choice field is something else,
  "`free_text` has no producer and the drawn card's free-text escape has nowhere
  to land."
- WHY IT IS ONLY HALF COVERED. The NOTE half of the ruling landed in the
  contract — `conversation/v1/question.proto:191` retires tag 4 with the
  finding's own reasoning. The FREE_TEXT half did not:
  `question.proto:189` still declares `optional AgentQuestionFreeText
  free_text = 3`, and `webapp.md` instructs "FeedQuestion (free-text escape
  always drawn)". No overhaul document names a producer for it.
- CLOSEST NEAR-MISS. `shim.md` says the shim "undoes both [the text-keying and
  the comma-join] at the boundary using echoed values (it validates echoes
  against the pending callback it already holds)". Validating against the
  pending ask does make LABEL de-joining tractable, so this covers the answer
  serialization — but it is silent on the residue the item actually flagged:
  the typed text riding inside that same joined string, which is what
  `free_text` claims to carry.

## 5. Item 8's field-set validation experiment is unassigned, and no
   compaction test spec exists anywhere

- WHAT THE ITEM REQUIRES. Item 8's run confirmed the PREMISE (resume consumes
  only the on-disk JSONL; deep equality, not byte equality; uuid chains are what
  resume walks) but recorded that "the exact required-field union is explicitly
  CLI-internal and undocumented — a shim-written transcript must mimic observed
  lines, and validating that is an implementation-wave experiment".
- WHY IT IS NOT COVERED. `SessionColdCompact` is load-bearing in the overhaul —
  it is one of three cold-gate remediations on a user-facing surface — yet
  `shim.md`'s replacement integration specs (session lifecycle, turn, detached
  work, history, store client, conversion, reattach) contain no compaction
  subject at all, and no document schedules the transcript-writing experiment.
- CLOSEST NEAR-MISS. `daemon.md` and `shim.md` both assert the mechanism as
  settled — "Compaction is OURS: daemon-directed, shim-implemented via a
  throwaway summarizing session" — which states the design without carrying the
  register's warning that the hardest part (the field union the CLI silently
  requires) is undocumented and must be discovered empirically.

## 6. Item 4's superseded premise is restated in `daemon.md` in its
   pre-verification form

- WHAT THE ITEM SETTLED. The run found MIXED results: turn-fatal API errors ARE
  transcript-persisted (`isApiErrorMessage: true`), but retried-then-recovered
  errors, `result`-level classification, and control-channel errors are
  STREAM-ONLY and leave no transcript trace. The architecture absorbs this
  because "the daemon's own store persists conversation.v1 frames as the durable
  record and the vendor transcript is only a recovery source" — the register
  marks that store-is-the-record premise as "now load-bearing".
- WHY IT IS NOT COVERED. `daemon.md`'s failure-classification section still
  states the unqualified pre-run belief: "Every live vendor API failure must
  also land as a transcript record, or the feed silently misses it." As written
  that is false per the run, and it points an implementer at the wrong
  guarantee — the correct one is that the STORE, not the vendor transcript,
  must hold the frame.
- CLOSEST NEAR-MISS. `sidecar.md` and `store.md` both build the store-is-the-
  record architecture correctly, so the system will behave right by
  construction; the defect is the stated rationale, which invites a wave-time
  "recover it from the transcript" fix for exactly the classes the run proved
  are not there.

## 7. Item 9's travelling observation (do vendor background processes survive
   the CLI exiting?) is unassigned, and `daemon.md` asserts the unverified answer

- WHAT THE ITEM LEFT OPEN. The run confirmed the STREAM-level answer (the
  background-task level is per-process; nothing is re-announced on resume) but
  recorded explicitly: "Whether the underlying OS processes die or orphan at
  exit is NOT FOUND (no admissible evidence) … OS-process fate stays an
  implementation-wave observation."
- WHY IT IS NOT COVERED. No overhaul document schedules that observation. Worse,
  `daemon.md`'s SHIM RELAUNCH spec states the unverified half as fact — "WAIT
  FOR FREENESS (no in-flight turn, no live detached work — a shim bounce kills
  the vendor process and everything under it)" — turning a recorded unknown into
  a load-bearing premise of the rollout's wait condition.
- CLOSEST NEAR-MISS. The same entry's FAILURE DISPOSITIONS ("GetLiveWork
  reconciliation closes every open obligation") is the right compensating
  machinery, but it closes RECORDS, not orphaned OS processes; if detached work
  in fact orphans, the reconciliation writes a terminal for a process still
  running and writing its spool.

## 8. Item 10's travelling question has no home, and the one producer note that
   would answer it is contradicted by a `shim.md` gotcha

- WHAT THE ITEM LEFT OPEN. Whether a FRESH `task_started` edge fires when a
  foreground agent is backgrounded (Ctrl-B) is "NOT FOUND — no observed sequence
  exists anywhere"; the run named `task_updated{patch.is_backgrounded}` as the
  candidate producer for `DetachedCauseByUser` and moved the live observation to
  the implementation wave.
- WHY IT IS NOT COVERED. No overhaul document assigns the observation, and none
  names a producer for a Ctrl-B'd AGENT's detachment cause.
- CLOSEST NEAR-MISS, WHICH POINTS THE WRONG WAY. `shim.md`'s gotcha says
  "Backgrounding causes are harvested from the BASH TOOL RESULT
  (`timedOutAfterMs`, `backgroundedByUser`), never from the task stream." That
  is correct for a backgrounded SHELL and is exactly wrong for the agent case
  the item is about, where the register's candidate producer IS the task stream.
  `DetachForeground {AgentActivityId}` is prescribed as an rpc in the same
  document, so the case is in scope and an implementer has one rule that does
  not apply to it.

## 9. Owed C is covered only as far as "a warning exists" — the legibility
   requirement, which is the whole point, is absent

- WHAT THE ENTRY REQUIRES. `TopbarWarningStrip` needs a warning kind for
  "unmodeled work occurred" WITH "a dropdown treatment that renders an
  ABBREVIATED, LEGIBLE ACCOUNT OF THE CALL — never a dump of its untyped
  arguments", because the purpose of surfacing it is remediation: someone must
  be able to decide whether the tool deserves a modelled arm.
- WHY IT IS ONLY PARTLY COVERED. The warning's HOME landed —`daemon.md`
  ("Unmodeled tools are NOT failures and never feed rows — their home is the
  topbar's warning dropdown, one warning per distinct name") and `webapp.md`
  ("The warning dropdown is the home of unmodeled-tool and session-fault
  surfacing"). But "one warning per distinct name" is a NAME, not an account of
  the call, and no overhaul document carries any treatment of the call's
  arguments. The recorded (explicitly non-binding) idea of classifying the
  argument structure and summarizing novel shapes with a small fast model
  appears nowhere, and neither does any alternative means of meeting the
  requirement.
- CLOSEST NEAR-MISS. `webapp.md`'s server-driven-UI rule ("Where the daemon
  composed a sentence, the client draws the sentence") establishes WHO would
  compose such a summary, which is why this is a gap in the daemon's
  prescription rather than the webapp's.

## 10. Owed G's vetting half — is the keep-alive rewind reliable against the
    vendor's actual transcript? — is neither scheduled nor tested

- WHAT THE ENTRY REQUIRES. Owed G has two halves. The STAGE-5 half (index
  keep-alive turns as never-served) is covered. The VETTING half is stated
  plainly: "`SessionRewound` and `KeepAliveDiscard.dropped_turn_ids` already
  claim the rollback and the exclusion. Whether the rewind is RELIABLE against
  the vendor's actual transcript is unverified."
- WHY IT IS NOT COVERED. `shim.md` states the YIELD OBLIGATION as settled
  behavior ("a real prompt rolls context back to just after the last real
  prompt, discarding trailing keep-alive turns before delivery") without noting
  that the mechanism's reliability was never established, and no test spec in
  any document exercises the rollback.
- CLOSEST NEAR-MISS. `store.md`'s integration spec "Keep-alive exclusion (Owed
  G) … no page ever returns a keep-alive row" covers the STORE-side exclusion
  only. A correct store index with an unreliable rewind still yields the failure
  the entry is about: real turns silently building on keep-alive context, with
  nothing on any surface to reveal it.

## 11. Item 12 — the user-requested footer status/sub-status coverage review —
    has no verdict anywhere, and its disposition made it a gate before
    implementation

- WHAT THE ITEM REQUIRES. Added at the design-complete gate on the user's own
  request: present ONE table over `FooterStatus` × `FooterSubStatus` × the
  activity axis, naming which pairs are legal, which producer fact drives each,
  and which situations have no pair and draw as bare `idle`. Its disposition is
  explicit: "Run at the vetting stage as a presented overview; the user's review
  verdicts (gaps to fill, pairs to forbid) become ordinary landing increments
  before the contract freezes for fanout."
- WHY IT IS NOT COVERED. The contract is frozen (ORCHESTRATION-META STATUS:
  "contract frozen at 2d79f7501") and no overhaul document records that the
  review was presented or what it concluded. This is the one register item whose
  disposition was a PRE-FREEZE gate, so its absence is a process gap as well as
  a content one.
- CLOSEST NEAR-MISS. `webapp.md` describes the family's shape ("Status →
  SubStatus → StatusActivity … legality-by-construction: each status arm
  declares which substeps/activities are legal under it"), which restates the
  file's own construction. The item's premise is that a per-arm-correct family
  can still have SEQUENCE holes, so a description of the construction is exactly
  the evidence the review was called to go beyond.

## 12. Owed H's clause (2) — "every record's parent is an `AgentId`, never nil"
    — is contradicted by the landed store design, and the divergence is
    nowhere acknowledged

- WHAT THE ENTRY REQUIRES. Clause (2): "Every record's parent is an `AgentId`,
  never nil: the main agent's children carry the main agent's id."
- WHY IT IS ADDRESSED-BUT-DIVERGENT. `store.md` lands the opposite explicitly
  and with reasons: `StoreAgentUpdate.top_level` is "Optional: UNSET only when
  unresolvable (an unparsed record may name no agent)", and `book_agent_id` is
  "indexed and NULL for unserveable". The superseding design IS carried, so this
  is not an uncovered requirement — it is listed here because the register's
  never-nil clause is stated absolutely and a reader reconciling the two
  documents will find a flat contradiction with no note that one supersedes the
  other. Clauses (1), (3) and (4) of Owed H are fully covered (see below).

---

## Checked and covered

Each of the following was traced to a specific overhaul statement, so the
coverage above can be trusted as exhaustive over the register.

- **Owed A** (index a unit's OWN identity) — `store.md`: `entry` is a spine with
  `upsert_key` as PRIMARY KEY, plus the explicit standing policy "The
  re-announced `start` instant is recovered FROM the store by unit id (a
  shim-restart path): rows must be reachable by their upsert identity."
  Reinforced in `sidecar.md` (restart-recovery integration spec, gotchas) and
  `shim.md` (streams-owe-consumers).
- **Owed B** (`forwardSubagentText`) — `shim.md` gotcha, first line: "must be
  set or a subagent's prose and reasoning never reach the shim at all."
- **Owed D** (thinking-token representation) — discharged by the contract:
  `conversation/v1/api.proto:271` carries `output_thinking_tokens` (the billed
  figure the PARTLY SETTLED status validated) and `frontend/v1/footer.proto`
  draws thinking as an indented subclass of output. The residual — a type for
  the live ESTIMATE channel — is conditional on a surface drawing it, and none
  does. `daemon.md`'s gotcha "Thinking-token estimates are unbilled — never add
  them to a bill" carries the reason the two must not merge.
- **Owed E** (workflow per-agent transcripts) — `sidecar.md` twice: the blocker
  list ("discovery must glob `workflows/wf_*/agent-*.jsonl` + meta.json") and
  the discovery-scope section, which additionally lands the meta.json dependency
  ("a per-agent transcript is not ingestible without its meta file").
  Cross-stated in `shim.md`'s gotchas.
- **Owed F** (four files still naming `conversation.v1.MessageId`) — discharged
  in the contract: `grep -rn MessageId proto/src/` returns nothing. The
  successor vocabulary is carried throughout (`FeedId` encode/decode in
  `daemon.md`, `AgentId`/`AgentActivityId` in `shim.md`).
- **Owed G, implementation half** — all three requirements stated: store
  indexing (`store.md`, `sidecar.md` gotchas), the rollback (`shim.md`'s YIELD
  OBLIGATION), and shim-owned prompt text ("The shim determines the keep-alive
  prompt text"). Only the reliability verification is missing (finding 10).
- **Owed H, clauses (1), (3), (4)** — `store.md`: logical-session scoping with
  `vendor_session_id` as a mutable attribute and "an identity rotation must
  never split an agent's book or a page"; the indexed `book_agent_id` walk
  ("Pagination is two-keyed in practice: filter by book, walk by pointer")
  replacing the ingest-time owner walk; keep-alive exclusion as a named
  integration subject.
- **Item 2** (shell vs subagent detachment) — assumption held; `shim.md` carries
  the consequence: the shim "consumes the vendor's LEVEL signal
  (`background_tasks_changed`, replace semantics) and its EDGE bookends without
  ever diffing the level or pairing edges", and `daemon.md` forbids the daemon
  pairing edges. The declared ordering non-guarantee is absorbed by replace
  semantics, exactly as the run said.
- **Item 3** (`stop_reason` finality) — assumption held at turn granularity;
  `daemon.md` gotcha carries it: "Finality of a response is derived per render
  from the turn's own conclusion, never a positional or wire fact", and
  `webapp.md` adds "Multiple responses per turn is the NORMAL case."
- **Item 7** (mid-turn prompt to the MAIN agent) — assumption held; the
  undeclared fold-vs-queue policy is absorbed by `shim.md`'s ruling that "How a
  prompt lands (steer-at-next-tool-round vs resume) is the shim's business; the
  consumer never learns which", with the daemon as sole queue.
- **Item 1, the sub-findings other than the rulings slate**:
  - SHIM-DERIVABLE trio — `shim.md` gotcha: "Grep/glob omitted figures are
    shim-subtracted (the vendor reports totals); hook duration and spawn depth
    are shim-derived."
  - STALE ARTIFACT COMMENT — corrected in both `shim.md` ("artifact (the output
    is TYPED — read fields, never parse prose)") and `webapp.md`.
  - The ~20 unmodeled/unexempt built-ins — resolved in both directions:
    `shim.md`/`sidecar.md` publish the EXEMPT SET explicitly (TaskStop,
    TaskOutput, TaskGet, TaskList, ToolSearch, NotebookEdit, REPL, the
    MCP-resource family, SendFeedback, ClaudeDesign, Projects,
    ShowOnboardingRolePicker, ProposeSkills, the background-shell peek), and the
    activity vocabulary now models plan mode, report_findings, worktree, cron,
    push_notification and the task acts. `AgentUnmodeled`'s meaning is restated
    as "a tool whose schema genuinely cannot be known", with a recognizable
    built-in arriving there called a producer defect.
  - SIDECAR-TIER routes — `sidecar.md` covers each: skill document by
    `sourceToolUseID` (never name-matching), diagnostics by adjacency, the
    detached-spool `EXIT=<code>` terminator; `shim.md`'s WatchSession arm list
    carries `context_budget_warning`.
  - SendMessage delivery arms — `shim.md`: "the `resumedAgentId` field is the
    only structured discriminator in the vendor's prose result", matching the
    register's "stands as recorded".
- **Item 6, the note half** — ruled and landed: `question.proto:191` retires tag
  4 with the finding's own reasoning. Only `free_text` remains open (finding 4).
- **Item 11, the sound half** — `get_context_usage` is named as the /context
  panel's source and prescribed as pushed-never-derived in `daemon.md` (topbar
  resolver) and `shim.md` (WatchSession arm 26).
