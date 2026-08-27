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

1. INVARIANT — dependency direction: the shim client is a leaf that knows
   no other module; WSM and the peer modules know and drive it, and all
   workspace-pertaining shim interaction starts at the module that owns
   the flow, never at the client.

2. PRESCRIBED — THE SHIM CLIENT (one per session).
   - RESPONSIBILITIES: the only module that dials shim.v1, as a dumb
     no-policy connection with an internal occupancy mutex hidden from
     callers (the in-memory guard backing WSM's lease metadata) — AND,
     by ruling, the shim PROCESS SUPERVISOR: spawn with process-group
     discipline, kill with stop attribution, reap with exit decoding,
     the stderr ring-buffer kept as failure evidence, and
     spawn-death-vs-connect correlation (a dead process ends bring-up
     immediately with exit and stderr, never a timeout).
   - INTERFACE: session/turn/kill/model/stand-down verbs (lease-checked,
     mutex-guarded), the WatchAgent frame streams, and answer/stop
     delivery.
   - USAGE: frame streams flow directly to their consumers; nothing
     relays them module-by-module.
   - PREREQUISITES: none (leaf; the generated shimv1connect stubs).

3. PRESCRIBED — WSM, THE STATE CLIENT.
   - RESPONSIBILITIES: a database client around the daemon's durable
     state — the sole owner of the seven tables, including the workspace
     registry, session bindings, and the persisted occupancy lease
     (per-workspace: who may drive the session now).
   - INTERFACE: state operations only (resolve refs, bindings, lease
     acquire/release/inspect, held prompts, queue positions, schedules);
     NO orchestration logic; peers call it, it calls only the database.
   - USAGE: the lease is HYBRID by ruling — the kernel file lock stays
     the ARBITRATION mechanism (cross-process, self-releasing on death,
     no stale-pid state), and WSM holds only the lease's POLICY metadata
     (holder label, refusal policy): the lock decides, the row
     describes. The lease projects PER-HOLDER REFUSAL POLICY onto new
     submissions — the merge lease ERRORS them (SubmitPrompt's merging
     refusal arm; post-merge-start work would be orphaned since a merged
     workspace closes), restart-pending and shutdown-drain leases HOLD
     them; items already held when a lease is acquired stay held.
   - DURABLE FACT INVENTORY (ruled; the facts are prescribed, the DDL is
     the orchestrator's):
     - MERGE GEOMETRY: per-workspace source branch, source dir, target
       dir, origin — recorded at workspace creation, REFUSED rather than
       guessed when absent.
     - CREATION JOBS: workspace lifecycle BEFORE any session exists
       (worktree path, branch, resolved base, materialization state),
       plus the configured before/after merge actions the merge
       orchestrator reads back.
     - SESSION FACTS beyond the binding: last-engagement (the idle
       sweep's input), death/terminality with cause (a deleted session
       REFUSES resurrection), and spawn identity (config dir,
       overrides).
     - FEED PAGE POSITION: the daemon persists, per workspace, where the
       webapp's page walk stands — because a fresh webapp asks for the
       first page (no token needed), but when the DAEMON restarts under
       a live workspace session (the doom self-merge reload), it must
       remember what page the webapp is on so a NextPage request works
       before a new prompt remints the walk.
     - DEAD BY DESIGN: the old compaction-gate instants — the shim's
       SessionCold refusal is the authoritative coldness fact now.
   - INVARIANT — one handle, one writer: WSM opens the database with a
     single connection and a single writer (two writers on one SQLite
     file was a real lost-update class); the sole-DB-owner rule made
     mechanical.
   - RULED ADOPTIONS from the feature-loss audit: exactly ONE durable
     held-prompt store exists (the old daemon's second store,
     session_record.queued_prompts beside the drain park rows, is an
     accident of history — never two); and workspace state is
     CURRENT-STATE ROWS ONLY — the old append-only multi-axis lifecycle
     log is DEAD BY DESIGN, because history and liveness now belong to
     the streams and the store (open watches ARE the live set), so
     derived facts like the live-task identity set and last-activity
     come from the new sources, never a WSM log.
   - PREREQUISITES: none. (Registry and binding internals are
     DISCRETIONARY.)

4. PRESCRIBED — THE PROMPT HANDLER (the modeled body of SubmitPrompt).
   - RESPONSIBILITIES: the request-side component every prompt crosses —
     modeled as a component so business logic never lives in an rpc name.
   - INTERFACE: acknowledge with the TurnId; MIRROR THE PROMPT TO THE
     USER IMMEDIATELY, split by state — a HELD prompt mirrors to the
     TRAY only, an accepted-for-delivery prompt gets its FEED row
     (ruled: the old daemon's retired optimistic echo died of a
     double-identity bug that daemon-minted TurnId/FeedId identity
     removes structurally); recognize session commands and answer the
     read-only panel class inline; forward everything session-bound to
     the prompt queue.
   - USAGE: thin and stateless, done at submission; it owns NO execution
     and NO response formatting.
   - PREREQUISITES: prompt queue, view resolvers (for the mirror push).

5. PRESCRIBED — THE PROMPT QUEUE (peer module).
   - RESPONSIBILITIES: the ONE path for ALL session-bound deliveries —
     prompts from every origin, session-acting commands (/clear,
     /compact), AND model changes (ruled: /model and the SetModel rpc
     both submit the same session-act here, because a model change
     resolves at the turn boundary and respects the lease like any
     delivery — one meeting point, so command and picker can never
     diverge) — as a CONSTRAINT, not a default.
   - INTERFACE: submit in; delivery through the shim client; holds
     persisted via WSM; serves the tray's facts.
   - USAGE: check the occupancy lease (delivering the lease holder's own
     submissions, refusing or holding others per the lease's policy);
     deliver if clear, else hold; on turn resolution, pop and deliver
     the next held item. RULED ADOPTIONS from the feature-loss audit:
     - THE CLASSIFIER: a queued prompt is judged (headless cheap-model
       run) into the five-verdict taxonomy the tray protos already carry
       (classifying / interject / hold_for_turn_end /
       uninterruptible_turn / classification_error), with the
       explicit-interrupt fast path ("stop", "abort", ...) bypassing the
       model round trip.
     - INTERJECT, re-specified: the interrupting prompt is placed at the
       queue's SEMANTIC HEAD before teardown begins (it, not the
       pre-interrupt head, is the next delivery); the footer's
       waiting·interrupting push fires the MOMENT the interrupt
       registers; the submit waits for the turn's REAL end, and a failed
       interrupt strips the jump and stamps the classification error.
     - PARKED-LEDGER SEMANTICS on held_prompt: boot-materialized,
       adopted on session wire-up, cancel legal with no session,
       durable-drop-FIRST on cancel (a failed drop refuses the cancel),
       tombstoned so a mid-flight restore cannot resurrect a forced or
       cancelled prompt.
     (Delivery-retry pacing and unknown-fate reconciliation were ruled
     NOT prescribed — the implementing orchestrator's.)
   - PREREQUISITES: WSM, shim client.

6. PRESCRIBED — THE MERGE ORCHESTRATOR (peer module).
   - RESPONSIBILITIES: the whole merge — per-repo queue, phases
     (including the git work and agent-driven conflict resolution), and
     the merge facts the frontend draws.
   - INTERFACE: enqueue (MergeWorkspace = enqueued, rest is push);
     pause/resume/evict; dequeue offer via the tray; emits merge facts
     for the view resolvers, never touching a view itself.
   - USAGE: holds the occupancy lease for the merge's duration; its
     remediation prompts route through the prompt queue like any origin;
     phases are append-only (a repeat pass is a new phase).
   - GOTCHAS: phase history is feed content, not WSM columns; an
     in-flight merge across a daemon restart is resumed or LOUDLY
     failed, never left with the lease stuck; the composer gate is the
     primary defense against post-merge-start prompts and the merging
     refusal arm is the race fallback; the old daemon's merge code is
     the edge-case reference.
   - PREREQUISITES: WSM, shim client, prompt queue.

7. INVARIANT — orthogonality: the prompt queue, merge orchestrator, and
   drain controller never call each other laterally (the merge's prompts
   use the queue's one public path like any origin); they meet only at
   WSM and its lease.

8. INVARIANT — response-side ownership: the ingest core and the view
   resolvers own ALL outcome formatting — including session-acting
   command outcomes such as /clear's separation row, which happen at
   EXECUTION time, not submission time — and no request-side component
   formats responses.

9. DISCRETIONARY — drain/shutdown controller (schedule + idle sweep;
   prereqs: WSM, shim client; acquires the lease like any peer).
## Not yet walked
- The EMACS+WEBAPP section (Connect server + resolvers/publishers) and
  the internal-only components (ingest core, failure classification,
  accounting, git ops) — architecture decisions land here as settled.
