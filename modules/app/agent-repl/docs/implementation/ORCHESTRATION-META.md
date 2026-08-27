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
- OWED BEFORE HANDOFF: MAIN.md (the e2e replacement specs), the
  reconciled-foundation SHA record, and the remaining architecture
  sections.
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
