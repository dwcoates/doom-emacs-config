# ORCHESTRATION META — FOR THE PLANNING ORCHESTRATOR ONLY

Downstream agents do NOT consume this document. It records the PROCEDURES
established for the daemon (and general fanout) planning process so they
survive compaction. It joins the post-compaction mandatory-reload set.

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

11. UNAMBIGUOUS COMPONENT REFERENCES. When an entry mentions another
    component, the mention is short but NEVER ambiguous — name the
    component and anchor it ("the WSM occupancy lease (entry 3)", "the
    shim client's occupancy face (entry 2)"), never a bare word like
    "lease" or "publishers" that could name several things.
