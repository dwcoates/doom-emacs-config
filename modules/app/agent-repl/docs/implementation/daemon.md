# Daemon implementation planning

DISPOSITION (settled with the user): the daemon is REBUILT FROM SCRATCH
against the frozen contract. Reconciliation deliberately left the old
daemon untouched (9,724 dangling references across 452/830 files — its
re-targeting is implementation, not adaptation), and the old tree stays in
git as REFERENCE MATERIAL for behavioral knowledge (merge orchestration
and its git edge cases, hold/classification policy, errclass taxonomy).
Its build is knowingly red at the foundation SHA.

PRESCRIPTION DEPTH (standing rule): this document prescribes only the
NON-TRIVIAL components and their seams. Simple/trivial internals are
deliberately NOT prescribed — implementing orchestrators fill those gaps
dynamically. Every prescribed decision carries its PREREQUISITES, because
the prerequisite annotations are the orchestrator's sequencing graph —
this planning process doubles as the implementer-orchestration plan.

## Settled architecture decisions

1. DEPENDENCY DIRECTION: the shim client is a LEAF — it knows nothing of
   WSM or any other module; WSM knows and drives the client. All
   workspace-pertaining shim interaction STARTS at WSM.

2. THE SHIM CLIENT (one per session) is a dumb connection module with an
   internal occupancy MUTEX (obfuscated from callers) and two faces:
   - OCCUPANCY face (StartSession, StartTurn, Kill*, SetSessionModel,
     stand-down): fully WSM-MEDIATED, lease-checked, mutex-guarded.
   - CONVERSATION face (WatchAgent frame streams, UpdateAgent
     answer/stop): WSM RESOLVES the workspace to the client handle and
     HANDS OFF — frames flow client→ingest directly; WSM never relays
     frame-by-frame.

3. WSM is the workspace-state coordinator and SOLE DATABASE OWNER (all
   seven tables). The in-client mutex is the in-memory guard; WSM's
   persisted lease is the source of truth behind it.

4. OCCUPANCY LEASE (non-trivial, prescribed): per-workspace exclusivity —
   who may drive this workspace's session now. Acquired by the merge
   orchestrator and the drain controller; respected by prompt delivery.
   THE TRAY'S HOLD REASONS ARE PROJECTIONS OF THE LEASE: a prompt
   arriving at a leased workspace is held under the holder's label
   (merge / restart-pending / shutdown-drain). Distinct from the per-REPO
   merge window (queue admission) — both exist.

5. PROMPT HANDLER is deliberately THIN: recognize the session command,
   forward to WSM. WSM consults lease + in-flight and either delivers
   through the occupancy face or persists the hold in held_prompt ("the
   daemon is the only queue", made literal).

6. WSM SUBCOMPONENTS and their prerequisites (parallelizable given 6a+6b):
   a. registry (workspace table) — no prereqs; TRIVIAL, not further
      prescribed.
   b. binding + occupancy lease (session_binding + lease) — prereqs:
      registry, shim client. The one arbitration point.
   c. prompt queue (held_prompt) — prereqs: a, b. Orthogonal to d/e by
      the lease projection: it never knows WHY a workspace is leased.
   d. MERGE ORCHESTRATOR — prereqs: a, b. NON-TRIVIAL, gets its own
      dedicated component and usage-pattern prescription (queue admission
      per repo, phases, git work, bubble/footer synthesis) — to be
      detailed in this document as its section is walked.
   e. drain/shutdown (shutdown_schedule + idle sweep) — prereqs: a, b.
   Subcomponents c/d/e never call each other laterally; they meet only at
   the lease and the registry.

## Not yet walked
- The EMACS+WEBAPP section (Connect server + resolvers/publishers) and
  the internal-only components (ingest core, failure classification,
  accounting, git ops) — architecture decisions land here as settled.
