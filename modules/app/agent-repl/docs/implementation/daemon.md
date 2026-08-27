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

## Marker vocabulary
Entries below are marked PRESCRIBED (dedicated module; responsibilities/
interface/usage/prereqs stated), INVARIANT (binding cross-component
constraint, no internals), or DISCRETIONARY (named for the dependency
graph only; internal design is the implementing orchestrator's). Anything
unmarked is DISCRETIONARY by default.

## Settled architecture decisions

1. INVARIANT — dependency direction: the shim client is a LEAF (knows
   nothing of WSM or any module); WSM knows and drives the client. All
   workspace-pertaining shim interaction STARTS at WSM.

2. PRESCRIBED — THE SHIM CLIENT (one per session).
   - RESPONSIBILITIES: the only module that dials shim.v1; a dumb
     connection with NO policy; internal occupancy MUTEX obfuscated from
     callers.
   - INTERFACE: two faces. OCCUPANCY (StartSession, StartTurn, Kill*,
     SetSessionModel, stand-down) — WSM-mediated only, lease-checked,
     mutex-guarded. CONVERSATION (WatchAgent frame streams, UpdateAgent
     answer/stop) — WSM resolves workspace→handle and HANDS OFF; frames
     flow client→ingest directly, never relayed through WSM.
   - USAGE PATTERNS: WSM drives occupancy; ingest and the answer verbs
     hold conversation-face handles obtained from WSM.
   - PREREQUISITES: none (leaf; generated shimv1connect stubs).

3. PRESCRIBED — THE OCCUPANCY LEASE (inside WSM).
   - RESPONSIBILITIES: per-workspace exclusivity — who may drive this
     workspace's session now. Persisted truth in WSM; the client's mutex
     is its in-memory guard.
   - INTERFACE: acquire/release by the merge orchestrator and the drain
     controller; consulted by prompt delivery.
   - USAGE PATTERNS: THE TRAY'S HOLD REASONS ARE PROJECTIONS OF THE
     LEASE — a prompt at a leased workspace holds under the holder's
     label (merge / restart-pending / shutdown-drain). Distinct from the
     per-REPO merge window (queue admission); both exist.
   - PREREQUISITES: registry, shim client.

4. PRESCRIBED — THE PROMPT QUEUE (inside WSM; persisted in held_prompt).
   - RESPONSIBILITIES: the ONE queue — classification, holds, delivery.
   - INTERFACE: fed by the thin prompt handler (recognize the session
     command, forward to WSM — nothing more); serves the tray's view.
   - USAGE PATTERNS: consult lease + in-flight → deliver through the
     occupancy face (interrupting when that is what delivery takes) or
     persist the hold; release/drop verbs act on held rows; it never
     knows WHY a workspace is leased, only the holder's label.
   - PREREQUISITES: registry, lease.

5. PRESCRIBED — THE MERGE ORCHESTRATOR (inside WSM).
   - RESPONSIBILITIES: queue admission per repo, phase execution, the git
     work, merge bubble/footer/roster fact synthesis.
   - INTERFACE and USAGE PATTERNS: to be detailed as its section is
     walked (deliberately still owed).
   - PREREQUISITES: registry, lease.

6. INVARIANT — orthogonality: the prompt queue, merge orchestrator, and
   drain controller never call each other laterally; they meet ONLY at
   the lease and the registry. Given registry+lease, they are pairwise
   independent and parallelizable.

7. DISCRETIONARY — registry (workspace table; prereqs: none), session
   binding (session_binding; prereqs: registry), drain/shutdown
   controller (shutdown_schedule + idle sweep; prereqs: registry, lease;
   its lease usage is fully covered by 3). Internal design is the
   implementing orchestrator's.

8. WSM is the workspace-state coordinator and SOLE DATABASE OWNER (all
   seven tables) — INVARIANT.
## Not yet walked
- The EMACS+WEBAPP section (Connect server + resolvers/publishers) and
  the internal-only components (ingest core, failure classification,
  accounting, git ops) — architecture decisions land here as settled.
