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
   - INVARIANT — the battle-tested open settings are copied from the old
     daemon's statedb open path (each exists because a real bug happened
     without it, the worst being a silently lost concurrent write).
   - INVARIANT — the database file carries its layout version, an older
     daemon REFUSES to open a newer file (silent corruption class during
     deploy/rollback), and a read-only open mode exists for inspection
     that is guaranteed to change nothing.
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
   - RULED ADOPTIONS from the feature-loss audit:
     - CONFIGURED ACTIONS: before_ws_merge runs the workspace's session
       under the lease BEFORE the pick plan and its failure fails the
       run; postprocessing_prompt runs AFTER every commit lands, can
       never fail the run, and its error rides the terminal status;
       both are read from the creation-job facts.
     - THE TEST GATE: suites selected from changed paths by blast
       radius (unknown beats wrong), output archived and named in the
       failure, and the agent remediation loop exits ONLY on a pass or
       the agent's own escalation record. NO flake re-run — a failure
       is an error to remediate, period (a deliberate change from the
       old daemon).
     - GIVE-UP RULES: conflicts handed to the agent EXACTLY ONCE per
       conflict commit then parked for a human; evict/dequeue/abandon
       are three distinct ends with distinct causes; a DELETED session
       refuses the merge; a workspace with no session merges
       sessionless; the displaced user turn is captured durably and
       resubmitted exactly once at lease release, across a daemon
       bounce.
   - RULED (merge-variants investigation):
     - TWO INGRESSES, ONE ENGINE: Emacs merges arrive via the
       MergeWorkspace rpc, non-Emacs merges via the workspace
       command-file merge verb; the engine is identical behind both, a
       merge never round-trips through Emacs, and a workspace with no
       live session merges sessionless (skipping displaced-turn capture
       and post-merge teardown).
     - ACCOUNT/LANDING SPLIT IS COMPUTED, NEVER HARD-PINNED: a repo
       under MULTI_REPO_ROOT lands via PR + CI merge queue then close
       (cherry-picking would duplicate CI-owned commits); a repo outside
       it lands via the local merge engine. The old elisp hard-pins this
       by directory constants — the rebuild computes it from the env
       var.
     - SELF-RELOAD: a merged outcome whose TARGET matches the daemon's
       own checkout (git common-dir identity; sibling worktrees
       excluded) triggers the self-redeploy — fires exactly once, only
       after lease release and terminal publication, classifies the
       landed range by changed subsystem prefixes and restarts ONLY
       what changed; EXECUTION is delegated to the rollout controller
       (the graceful-rollout entry below), which owns the
       zero-perceived-downtime mechanics.
   - THE BUBBLE AND ROUTING (settled at the merge-flow remediation):
     the merge bubble is a SUB-FEED (FeedMerge = head only; six
     FeedMergeTab rows: queue | rebase | tests | remediation | action |
     landing; agentic tabs are parent containers, resolved tabs replace
     whole; rounds are new tabs). The orchestrator EMITS: the merge
     facts, the queue snapshot (front entry carrying its active tab
     label), and the generic OUTPUT ADDRESS {target feed, parent row} —
     set on lease acquisition, updated per tab, cleared on release —
     that the merge-agnostic feed resolver applies. Feed and footer
     pushes dispatch IN PARALLEL at every merge-state change
     (prescription, not a wire invariant). The tests tab ships
     daemon-parsed ANSI as paint-class spans.
   - THE PARKED POLICY (settled): when the resolution agent exhausts its
     attempt, the lease's refusal policy flips to PARKED — submitted
     prompts are neither refused nor queued as the session's own turn;
     the prompt queue's one path delivers them through this orchestrator
     as guidance to the resolution agent, landing in the parked tab. NO
     classifier and no content inspection: THE LEASE STATE IS THE
     RECOGNITION. The host composer's merge_parked arm opens the
     composer with context; the footer shows merging·parked with the
     composed line. HAND-RESOLUTION IS UNSUPPORTED by ruling — no
     resolved-continue verb exists on any ingress; the conversational
     parked flow is the only resume.
   - RULED (batch-1 triage, 2026-08-28): unrecorded/unmergeable
     workspaces are refused PRE-STATE on both ingresses (rpc: derived
     MergeWorkspace error arm; file route: quarantine — no
     enqueuing→failed stamp exists); a QUEUED merge JOINS THE CLOSE
     BLOCKERS (close refuses while a merge is queued, one more composed
     reason); post-merge WORKTREE REMOVAL IS THE DAEMON'S (this
     orchestrator's post-merge teardown removes the merged worktree —
     Emacs's finish removal dies).
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

10. PRESCRIBED — THE ROLLOUT CONTROLLER (graceful doom-change rollout).
   - RESPONSIBILITIES: turns the merge orchestrator's self-reload
     trigger into a zero-perceived-downtime rollout, per changed
     subsystem: daemon → blue-green handover; shim → per-workspace
     preemptive relaunch; elisp → hot-load; webapp → hot asset swap;
     SIDECAR and STORE → deliberately UNHANDLED (a user-initiated full
     restart; both are rare-change codebases by design — revisitable in
     a future project, not this one).
   - DAEMON HANDOVER (product spec — the full agreed flow):
     1. SPAWN. The old daemon detects the self-merge, rebuilds, and
        spawns the new daemon itself. The new daemon starts in JOINING
        mode: binds a fresh socket, opens WSM read-only, owns no
        workspaces.
     2. ANNOUNCE. The old daemon announces its shutdown on the
        EXISTING Emacs connection, carrying the new daemon's address.
     3. DUAL ATTACHMENT. Emacs opens a second connection to the new
        address while keeping the first. Both are live; each
        workspace's updates flow ONLY from the daemon that currently
        owns it, so Emacs never sees one workspace from two sources.
     4. PER-WORKSPACE TRANSFER. The OLD daemon detects freeness (no
        in-flight turn, no live detached work) — it owns the session,
        so only it can know. At freeness it QUIESCES: from the
        transfer notice on it does NO work for that workspace (queue,
        views, anything), holding all arrivals; it detaches from the
        workspace's shim (which keeps running), releases the
        workspace's kernel lock, and announces the transfer on the
        OLD connection. Emacs then treats the new connection as that
        workspace's home and tells the NEW daemon "workspace X is
        yours — resume pending operations." The new daemon adopts the
        running shim, claims the lock, drains the held intake in
        order, and pushes fresh views (the persisted feed page
        position keeps the webapp seamless).
     5. DRAIN AND EXIT. After the last transfer the old daemon exits
        gracefully; the new daemon's WSM handle becomes the sole
        writer.
   - THE WIRE (landed post-freeze increment): Emacs holds `WatchDaemon`
     (push `shutdown_announced { address }`); `WatchHostWorkspace` gains
     `transferred` and `reload_webapp` push arms; the webview holds
     `WatchWebWorkspace` (push `transferred { address }` — the address
     rides here since a webview has no daemon-level stream); adoption is
     TWO sibling verbs, `AdoptHostWorkspace` and `AdoptWebWorkspace`
     (WEB LINK section), so the VERB identifies the participant.
     `transferred` is a PUSH, never a terminal frame — the CLIENT cancels
     its streams after acting (the standing-stream convention).
   - THE ADOPT RENDEZVOUS: expected participants are the holders of the
     workspace's two per-workspace streams at announcement time; the new
     daemon completes adoption (claim the kernel lock, adopt the running
     shim, drain held intake) only when every expected participant has
     called, and all calls succeed together; headless workspaces have
     zero participants and transfer via WSM facts + the lock alone. The
     new daemon REFUSES per-workspace rpcs for an unowned workspace —
     ordering by refusal, not convention; derived error arms owed at the
     wave: `transferring_away { address }` on the old daemon's verbs,
     `not_yet_adopted {}` on the new daemon's (two arms — wrong daemon vs
     too early are different facts).
   - ADOPTION TIMEOUT (ruled): the OLD daemon times the window and
     surfaces expiry as that workspace's own error; remediated as it
     comes up — deliberately NOT an invariant to harden, and no
     abort/retry machinery exists.
   - NEVER-FREE WORKSPACE (ruled): wait forever in the two-daemon steady
     state, with a periodic warning log (~every 10 minutes) naming the
     holdout; a newer rollout supersedes a joining daemon that never
     finished.
   - WEBAPP-ONLY ROLLOUT (ruled): the `reload_webapp` push has EMACS
     reload the workspace's xwidget against the SAME daemon; the arm is
     empty (no address — the daemon is not changing, and a combined
     rollout never sends it: the handover's fresh attach pulls new assets
     as a side effect); the webview's default first-page-only load is the
     whole recovery.
   - SHIM RELAUNCH (product spec — the settled flow; the same engine
     serves the build-staleness bounce, one engine two triggers):
     1. REBUILD once; per live workspace, independently and in
        parallel, PRELAUNCH the new shim process — up and
        daemon-connected but INERT by construction (no session started:
        no vendor process, no store writes, no keep-alives), so it
        coexists with the old shim indefinitely.
     2. WAIT FOR FREENESS (no in-flight turn, no live detached work — a
        shim bounce kills the vendor process and everything under it);
        never-free gets the same wait-forever + periodic-warn ruling as
        the daemon handover.
     3. AT FREENESS: flip intake to the restart-pending HOLD
        (tray-visible, existing semantics); STAND DOWN the old shim —
        stop keep-alives, WAIT FOR ALL STORE ACKS (an exit with
        unacknowledged writes is a loud failure; there is no durable
        spill), terminate its vendor process, exit.
     4. THE REAP IS THE GATE: the shim client confirms the old process
        is GONE (kill attribution + exit decoding) before anything
        else — the guarantee that at most one vendor binary ever
        touches the session's transcript.
     5. GREEDY REATTACH: StartSession(resume) on the prelaunched shim
        at once; the context cache is SERVER-side so a fast swap stays
        warm (the cold gate fires only on a genuinely lapsed TTL, under
        the ordinary rules); model and mode recover from the transcript
        per the settled resume behavior.
     6. DRAIN the held intake; keep-alives resume in the new shim. No
        wire fact anywhere: the host stream's shim_attached flicker is
        acceptable and useful feedback; parked (shim-less) workspaces
        need nothing — the next implicit revival spawns the new binary.
     FAILURE DISPOSITIONS: old shim won't exit in the stand-down window
     → force-kill + reap, GetLiveWork reconciliation closes every open
     obligation (stream-only residue of the window is lost, loudly
     logged — accepted, not an invariant); resume finds a lapsed TTL →
     the ordinary cold gate; resume fails hard → the workspace's own
     error, remediate-as-it-comes-up.
   - INVARIANT — no durable producer spill: the shim's WriteBatch retry
     is a BOUNDED IN-MEMORY buffer; exhausted retries are a LOUD logged
     drop, never a crash and never disk persistence (persistent store
     unreachability is a lifetime-sequencing defect to fix at the
     source). The sidecar needs no buffer: its sources are durable
     files re-read from the cursor.
   - FACT (verified in source): the shim never talks to the sidecar —
     they meet only at the store and at the files the vendor binary
     writes; a shim-only bounce is sidecar-oblivious, and the sidecar
     is a SINGLETON launchd process covering all workspaces.
   - WEBAPP SIDE: the old daemon pushes a transfer notice per webview;
     from that notice on, the webview sends nothing more on the old
     connection for that workspace; it connects to the new daemon
     FIRST, then acks the old — the old connection outlives the new
     one's creation, so no gap is observable and no work races the
     switch; the persisted feed page position makes re-attach
     evidence-free.
   - INVARIANT — no daemon↔daemon channel: coordination is Emacs relay
     + WSM facts + kernel locks only (locks self-release on death, so
     a crash mid-window leaves every workspace claimable by the
     survivor).
   - INVARIANT — WSM contention scope: during the overlap every
     mutable WSM fact is workspace-scoped (arbitrated by the workspace
     kernel lock) or repo-scoped (the per-repo merge queue gets its
     own kernel lock); any future cross-cutting table must be
     lock-scoped or rollout-frozen.
   - GOTCHAS: headless workspaces transfer via WSM facts + lock claim
     alone and must never wait on an Emacs relay (Emacs may not be
     running); a never-free workspace leaves the rollout in a
     two-daemon steady state (policy open); a newer rollout supersedes
     a joining daemon that never finished.
   - OWED: the handover messages (shutdown announcement, transfer
     notice/ack, adopt command) are new emacs↔daemon and web↔daemon
     contract shapes — a protobuf increment to design before fanout.
   - PREREQUISITES: WSM, shim client (adoption), prompt queue
     (hold/drain), merge orchestrator (trigger).

## OPEN — unruled audit findings (the triage backlog)

These feature-loss audit findings are NOT yet ruled; each awaits a
remediate / do-not-remediate ruling per meta rule 14:

- SHIM-CONNECTION group: reconnect/backoff policy with terminal-vs-
  retryable classification; readiness gated on SILENCE rather than
  elapsed time; handshake facts (permission posture, resume position
  selection); the build-staleness bounce (shim reports build identity,
  daemon bounces exactly once on mismatch); surviving-shim arbitration
  (wait / adopt / evict, never a duplicate over one transcript); boot
  reconciliation with shims that outlived the previous daemon; the
  typed sink fan-out; model-catalog handling; connectivity truth edges
  (OnConnected/OnLinkLost as the only witnesses of wired).
- INTAKE SIDE EFFECTS group: a user prompt declines parked permission
  asks (a failed decline fails the submit); it cancels owed post-bounce
  re-drives; engagement is declared at the funnel and retracted on
  failure; the accepted edge publishes synchronously before the shim
  submit, with a retraction path restoring state when the submit fails.
- DRAIN group: the work gate is re-asked INSIDE the lease (an
  unprovable answer releases and refuses); teardown drains the
  interrupt BEFORE cancelling the connection; workspace-scoped orphan
  close with one-transaction bookkeeping (claim retirement +
  interruption rows + the idle edge together); standing refusals
  rate-limited with exact suppressed/total accounting; hold restore is
  all-or-nothing on a corrupt ledger.
- MERGE leftovers: dequeue-offer timing (the old flow raises it from
  the INTERRUPT, our entry says at completion); the agent-driven
  merge-skill detached window (a separate concept from the daemon
  merge); cross-repo multi-queue membership (one workspace queued on
  several repos; Standing reports the first, Dequeue takes all).
- MERGE-VARIANTS findings, UNRULED (evidence:
  docs/implementation/reports/merge-variants-2026-08-27.md; refusal
  semantics, close-vs-queue, worktree removal and conflict resume were
  RULED 2026-08-28 and moved to the merge orchestrator's entry): the
  boot geometry-backfill gate yields to a host connect briefly then
  runs anyway (merges must never depend on Emacs being up, and merge
  commands block on the gate); Emacs holds durable merged/merge-failed
  state across its own restart plus the merged-workspace visibility
  treatments (tab close on merged, re-raise on failure, teardown
  refusal mid-merge) that the new pushed views must feed;
  doom-multi-repo-mode membership is unevaluable by the daemon (an
  Emacs toggle with no on-disk representation widening "under the
  root"); --pr-was-merged exists only in the workspace skill — the
  merge engine has no PR-merged branch at all; and
  parent-notification-on-child-merge has no found implementation
  (unresolved in the report).

DECLINED (ruled, do not re-ask): delivery-retry pacing and unknown-fate
reconciliation are NOT prescribed (the implementing orchestrator's);
the merge test gate has NO flake re-run.

## Not yet walked
- The EMACS+WEBAPP section (Connect server + resolvers/publishers) and
  the internal-only components (ingest core, failure classification,
  accounting, git ops) — architecture decisions land here as settled.
- FIRST SETTLED FACT for the response side (from the merge-flow
  remediation): the FEED RESOLVER honors a generic OUTPUT ADDRESS
  {target feed, parent row} supplied by lease holders — it is
  merge-agnostic (any future lease holder gets bubble-routed output for
  free); the FOOTER RESOLVER is NOT agnostic (it projects merge facts
  into the merging status family); resolvers dispatch feed and footer
  pushes in parallel per state change.
