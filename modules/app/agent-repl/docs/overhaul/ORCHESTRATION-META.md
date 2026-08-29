# ORCHESTRATION META — FOR THE PLANNING ORCHESTRATOR ONLY

Downstream agents do NOT consume this document. It records the PROCEDURES
established for the daemon (and general fanout) planning process so they
survive compaction. It joins the post-compaction mandatory-reload set.

## STATUS (the settled/open ledger — updated at every landing)

- PIPELINE POSITION: contract frozen at 2d79f7501; five systems
  reconciled green and merged (shim, elisp, webapp, store, sidecar);
  the DAEMON is ruled a FROM-SCRATCH REBUILD (its old tree is untouched
  reference material, build knowingly red at the foundation).
- DAEMON ARCHITECTURE WALK: shim+state-db section settled and triaged
  through batch 4 plus the merge-variants rulings; daemon.md's OPEN
  section lists the unruled audit backlog; the response side (ingest
  core, view resolvers), the client-facing half (Connect server,
  publishers), and the by-endpoint sequence diagrams (meta rule 5) are
  UNSTARTED.
- ORCHESTRATOR PROMPT DOCS opened (2026-08-28) at
  docs/overhaul/prompts/ — TEAMLEAD.md (shared by all five) and
  PROJECTLEAD.md; running tallies per rule 22. The client-facing walk LANDED
  (2026-08-28): Connect server + publishers DISCRETIONARY (daemon.md
  12) and the never-miss-never-end-stale subscription invariant (13).
- CONVENTIONS WALK COMPLETE (2026-08-28): the conventions digest was
  triaged item by item into the prompt docs (teamlead gains: no-
  backwards-compat, proto→code mapping, validation invariant,
  production-code logging, four identifier spaces, bounded streams,
  push cadence, clocks, presence, exempt set, evidence standards, the
  proto-docs and weird-tests declaratives; projectlead gains:
  no-backwards-compat, validation invariant, proto-docs standard);
  system docs gained their specific items (daemon: parity, lifecycle
  decoupling, softened state-placement preference, store-nuke; elisp:
  popup subroutine + blink cadence; webapp: one separation renderer +
  link component + blink cadence); everything unchecked stays
  digest-only.
- FINAL-AUDIT TRIAGE COMPLETE (2026-08-29): all 144 findings from the
  seven coverage audits were walked and ruled with the user
  (docs/protobuf-design/final-audit/TRIAGE-RULINGS.md is the ledger);
  every DOC ruling is LANDED in the six system docs and both prompt
  docs; the proto comments were swept of planning-doc/skill pointers
  (build green). OWED: the ~17 sanctioned contract increments listed in
  daemon.md's "Contract increments owed" section — sketched with the
  user and landed before kickoff (the /context rich schema additionally
  needs verification against the vendor's real get_context_usage
  answer).
- OWED BEFORE HANDOFF: the contract increments above, then the
  foundation SHA stamped at dispatch.
  THE ARCHITECTURE WALK IS COMPLETE (2026-08-28): the internal
  components closed — failure classification falls out, accounting
  dissolved into per-resolver accumulation (sessionwatcher routes to
  resolvers only), git operations landed as THE GIT CLIENT (the second
  leaf, requirements-only). MAIN.md is DEAD BY RULING (2026-08-28): the
  project lead's prompt carries the e2e-suite duty — the lead derives
  the suite from the record and the contract; tests-are-not-truth is
  stated in both prompt docs. The by-endpoint diagrams are DROPPED by
  ruling.
- ROLLOUT CONTROLLER: daemon.md entry 10 is settled through the wire —
  the handover protobuf increment LANDED (WatchDaemon, adopt rendezvous
  pair, WEB LINK section, transferred/reload_webapp arms; record +
  digests updated); ruled: adoption timeout (not an invariant),
  never-free (wait forever + periodic warn), webapp-only reload via
  Emacs. The SHIM RELAUNCH is now SETTLED at product-spec depth
  (prelaunch-inert, freeness, hold, stand-down-with-ack-wait, reap
  gate, greedy resume, dispositions); SIDECAR joined STORE as
  rollout-UNHANDLED (user-initiated full restart); the durable producer
  SPILL is REMOVED (bounded in-memory retry, loud exhausted-retry
  drop) — record amended, WriteBatch comment updated, digests appended.
  The two derived refusal arms land at the wave.
  MERGE-FLOW REMEDIATION (2026-08-28): the merge bubble is a SUB-FEED
  with six per-kind-state tabs (protos landed, record + digests
  updated); address-driven merge-agnostic feed routing; the PARKED
  lease policy with state-based recognition; merge_parked composer arm;
  footer merging family realigned; batch-1 triage RULED (refuse
  pre-state, queued merge blocks close, daemon-owned worktree removal,
  conflict resume = conversational-only, hand-resolution unsupported).
  TRIAGE SWEEP (2026-08-28): every merge-variants finding RULED (boot
  repair dead; Emacs merge memory + merged-tab hiding killed;
  doom-multi-repo-mode killed; skill leftovers wont-do); INTAKE SIDE
  EFFECTS dropped for this project; DRAIN group fully ruled (three WSM
  invariants + rate-limited refusal logging; interrupt-before-
  disconnect rejected); SHIM-CONNECTION group fully ruled
  (2026-08-28): redial-forever-by-evidence, readiness-is-the-health-
  answer, crash-boot adoption, THE SESSIONWATCHER prescribed (10b —
  final shape: one per workspace/shim; watch + connectivity-truth +
  route only; the diagnostics and context-usage pulls FOLDED into
  WatchSession as pushed arms, both shim verbs deleted; detached work
  eager shim-side / lazy client-side); connectivity-per-hop invariant
  (11); THE FIVE RESOLVERS landed purpose-only (10c — feed, footer,
  topbar, sidebar, hold tray; in-memory accumulation fine, complete
  snapshots only). THE TRIAGE
  BACKLOG IS EMPTY; TWO MERGE
  METHODS RULED (2026-08-28): Emacs repo = pre-prompt → no-FF merge
  commit → tests+fixes → bounce → post-prompt; everything else =
  pre-prompt → post-prompt only; seven conditional tabs landed (protos
  + record + digests); MULTI_REPO_ROOT ruled ACCOUNT-ONLY and its
  support requirement landed on WSM; self-reload landed-range =
  merge-commit second-parent history.
- TOPBAR (2026-08-28): account element + context chip landed; the
  TOPBAR RESOLVER is PRESCRIBED (daemon.md 10a — the response side's
  first component); shim.v1 GetSessionContextUsage + conversation
  SessionContextUsage landed (pulled, never derived; /context panel
  un-stranded); account-switch CONSTRAINTS landed (daemon determines
  config dir + ports transcripts itself). The create-time selection gap DISSOLVED by
  ruling: the account is DETERMINED (repo-under-root), never selected —
  landed as an invariant. Two webapp audit reports parked in
  docs/protobuf-design/ for webapp planning.
- STANDING INSTRUCTIONS: STOP just before invoking /cross-system-fanout
  — the user gates that invocation personally; digests regenerate when
  the record grows; nothing lands without explicit approval.

## Procedures

1. THE MARKER VOCABULARY. Every component entry in a planning doc opens
   with one of three markers:
   - PRESCRIBED — needs a dedicated module; the entry follows the fixed
     template: RESPONSIBILITIES / INTERFACE / USAGE PATTERNS /
     PREREQUISITES.
   - INVARIANT — a cross-component constraint, binding but prescribing no
     internals.
   - DISCRETIONARY — named only for the dependency graph: one line +
     prerequisites + "internal design is the implementing orchestrator's."
   Anything unmarked is DISCRETIONARY BY DEFAULT — silence never implies
   prescription.

2. PRESCRIPTION DEPTH. Only non-trivial components are prescribed;
   orchestrators fill trivial gaps dynamically. Over-prescription is a
   defect.

3. PREREQUISITES ARE THE SEQUENCING GRAPH. Every prescribed or named
   component carries its prereqs; the annotations double as the
   implementer-orchestration plan (parallelizable = disjoint prereq
   subtrees).

4. DECISIONS LAND IMMEDIATELY. Every architecture decision settled in
   conversation lands in the owning subsystem's planning doc the moment it
   settles, marked per (1).

5. THE DAEMON ARCHITECTURE METHOD: by agentrepl.v1 ENDPOINT — one sequence
   diagram per rpc (31) plus exactly two non-endpoint diagrams (BOOT
   RECOVERY, IDLE SWEEP); shim.v1 calls are implementation details inside
   diagrams, never the set's basis; other systems appear as interface
   targets only, never their internals. Shared machinery surfaces
   organically through diagram overlap; the audit for machinery in NO
   diagram is the gap check.

6. REPLACEMENT COVERAGE prescribes INTEGRATION and E2E specs only — unit
   coverage falls out of the proto→code mapping convention.

7. THE PURPOSE OF THIS PLANNING PROCESS (the user's framing, binding on
   every entry): the goal is a GOOD HIGH-LEVEL ARCHITECTURE — landing
   invariants, architectural points, gotchas, notes, and constraints,
   especially insofar as they further ORCHESTRATION READINESS — never
   detailed prescription. Over-prescription LIMITS THE ORCHESTRATORS'
   FLEXIBILITY TO REMEDIATE and is a defect, not thoroughness.

8. META-PROCEDURE: process principles the user states during planning
   land in THIS document AUTOMATICALLY, the moment stated — the
   orchestrator never waits to be told to record one (the design record's
   record-on-settlement rule, applied to procedures).

9. ALL FEEDBACK IS PROCESS FEEDBACK. Every piece of feedback the user
   gives during this planning — including feedback phrased as iteration on
   one in-flight draft — applies to EVERYTHING covered by the process, and
   lands in this document the moment it is given. There is no
   "draft-local" revision: a correction to one entry IS a rule for every
   entry, and rule 8's automatic-recording obligation covers it. (The
   classification error this rule closes: treating a general principle as
   sketch iteration because it arrived while a sketch was open.)

10. PRESCRIPTION BREVITY. Prescriptions are written MUCH more simply than
    a full specification: a few short lines per template field. Detail
    beyond what orchestration-readiness needs is a defect (rule 7's
    purpose applied to the writing itself).

11. UNAMBIGUOUS COMPONENT REFERENCES, PLAINLY SPELLED. When an entry
    mentions another component, use exactly the component's own name —
    "the shim client", "the WSM occupancy lease" — enough to be
    unambiguous, and NOTHING more: no entry-number anchors ("(entry 7)"),
    and no sub-feature descriptors ("the shim client's occupancy face"),
    which read as naming a different thing than the component itself.
    Ambiguous shorthand ("lease", "publishers") and over-qualified
    references are the same defect from opposite directions.

12. SITREP = THE ARCHITECTURE OVERVIEW, AND IT PRECEDES EVERY LANDING
    SUGGESTION — NO EXCEPTIONS. "What's the sitrep" means the settled
    component overview: fully nested bullets, one brief sentence per
    entry, covering ALL system components settled so far. Before
    SUGGESTING that any change be landed, that overview is presented in
    exactly that format, and every sitrep includes a sync check of the
    planning docs against the settled state.

13. POST-LANDING FEATURE-LOSS AUDIT. After landing a set of components,
    dispatch one Opus subagent PER COMPONENT to check the EXISTING
    implementation for responsibilities/features the settled design does
    not account for (the near-loss of the prompt handler's
    mirror-prompt-to-webapp responsibility is the motivating case). Each
    agent returns a simple, concise overview of findings; findings are
    surfaced to the user IFF non-nil.

14. AUDIT-FINDING TRIAGE PROCESS. Feature-loss audit findings are
    remediated through batched rulings: the orchestrator groups findings
    into batches at its discretion (by context), and each finding is put
    to the user as ONE single-select AskUserQuestion with at least two
    options — one or more REMEDIATE variants (the orchestrator supplies
    the concrete variants as needed) and DO NOT REMEDIATE. Each ruling
    lands in the owning subsystem's planning doc per the standing
    approval rules.

15. AGREED TECHNICAL DETAILS SURVIVE INTO THE DOC. Any technical detail
    the user specifies and the orchestrator agrees with MUST be carried
    into the planning-doc update when the ruling lands: the doc is
    DEFINITIVE, downstream agents have none of this conversation's
    context, and every critical agreed fact must survive there. This is
    a completeness mandate for critical facts, never a verbosity
    mandate.

16. DIGESTS. The canonical design record stays intact and authoritative;
    per-system DIGEST documents (docs/protobuf-design/digests/) are
    DERIVED extracts, each headed with its generation SHA and "the
    record wins on any conflict"; they are regenerated (or appended)
    whenever the record gains entries, and the bootstrap points at
    digests, with the full record loaded only for contract-change work.

17. CAPTURE AT LANDING. Every landing updates the STATUS ledger above in
    the SAME commit — newly settled items move off the open lists, newly
    surfaced items join them; a landing that changes neither is exempt.
    The sitrep sync pass checks the ledger's freshness.

18. PRODUCT-SPEC DEPTH FOR COMPLEX ADDITIONS. Some additions are COMPLEX
    PRODUCT REQUIREMENTS internal to a system, not mere architecture —
    the graceful-rollout daemon handover is the exemplar. For those (and
    only those — they are rare), the entry captures the FULL AGREED FLOW
    at narrative depth: numbered phases, each phase's actors and actions,
    and the settled revisions — the level of the conversation's own final
    summary. Rule 10's brevity governs ordinary prescriptions; it never
    licenses compressing a product spec into a paragraph that loses the
    flow. The distinction to apply: describing WHAT the product must do
    step by step is spec, not over-prescription; prescribing HOW modules
    implement it internally still is.

19. NO SALIENT DETAIL LEFT TO INTERPRETATION. Every detail salient to
    the implementation that WE TOGETHER determined — anything decided to
    BE a certain way or to NOT be a certain way — MUST appear in the
    arch-doc update when it lands. Period. No decided constraint is ever
    left implicit, "obvious", or recoverable only from conversation;
    rule 15's completeness mandate is absolute, and the pre-landing
    check is: re-scan for BOTH decided details AND surfaced-undecided
    items, against the baseline "everything not yet in a doc" (the OPEN
    sections are the checklist) — never merely "since the last landing",
    so a missed item keeps failing the scan until captured instead of
    aging out of the window. (Scope widened by rule 21.)

20. ARCH AND PROTO LANDINGS CROSS-CHECK EACH OTHER. Every arch-doc
    landing includes a check for protobuf changes the change implies —
    surfaced with the landing, never discovered later. Every protobuf
    landing updates the canonical design record with the new entries AND
    regenerates (or appends to) every per-system digest the change
    touches, in the same landing.

21. THE OPEN PIPELINE — CAPTURE IS NOT ENDORSEMENT. The decided-things
    pipeline (rules 4, 15, 17, 19) gets its undecided mirror:
    - FINDINGS PERSIST AT SURFACING, AS OPEN. The moment an audit or
      investigation finding is surfaced, it lands in the owning
      subsystem doc's OPEN section marked UNRULED. This lands nothing
      unresolved into the prescriptions — the OPEN section is
      definitionally not-the-design; rulings later move items out.
    - SUBAGENT REPORTS GET A DURABLE HOME. A report whose findings are
      not fully ruled in the same conversation is saved verbatim under
      docs/overhaul/reports/, and the OPEN items cite it — the
      evidence outlives the transcript even when the summary is lossy.
    - PARKING IS A WRITE, NOT A NOTE. Suspending any walk or triage
      REQUIRES flushing its complete remaining state — every unruled
      finding, not just those already phrased as questions — into the
      OPEN section before moving on; the ledger points at it.

22. THE ORCHESTRATOR PROMPT DOCS ARE A RUNNING TALLY. docs/
    implementation/prompts/TEAMLEAD.md (one shared document for all
    five system teamleads — system-specific content lives in each
    system's digest and architecture documents, never here) and
    prompts/PROJECTLEAD.md hold the prompt information the fanout's
    orchestrators are launched with. Prompt-relevant rulings made
    during planning land there the moment they are made, per the
    rule-8 automatic-recording obligation.
