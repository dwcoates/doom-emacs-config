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
   - RULED (2026-08-28): REDIAL FOREVER with backoff when the
     connection to a still-running shim breaks — retry-vs-give-up is
     decided by EVIDENCE (a dead process stops the redial and
     surfaces), never by a count. READINESS IS THE HEALTH ANSWER: a
     starting shim counts as ready when GetSessionDiagnostics answers
     healthy — structural, never a fixed delay and never a
     quiet-on-the-wire heuristic (both die). First-connect facts
     (permission mode, resume position) are implicit in StartSession/
     WatchSession — when to call StartSession is the implementing
     orchestrator's, with the health endpoint available for gating.
     CRASH BOOT: shim processes that outlived a crashed daemon are
     RECONNECTED AND ADOPTED (the handover's adoption machinery), never
     killed-and-restarted — in-flight work survives the crash.
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
     - ACCOUNT SELECTION (MULTI_REPO_ROOT), carried forward as the
       current system supports it: config-dir routing by path
       (under-root → the multi-repo config dir, else default),
       create-time resolution with the no-inheritance asymmetry (a
       parent merely under the root has chosen nothing), transcript
       lookup probing the routed root first, same-vendor-uuid-under-
       two-accounts disambiguation, and the two-root account roster.
   - INVARIANT — one handle, one writer: WSM opens the database with a
     single connection and a single writer (two writers on one SQLite
     file was a real lost-update class); the sole-DB-owner rule made
     mechanical.
   - INVARIANT — the battle-tested open settings are copied from the old
     daemon's statedb open path (each exists because a real bug happened
     without it, the worst being a silently lost concurrent write).
   - INVARIANT — FREENESS IS ONLY EVER JUDGED WHILE HOLDING THE
     WORKSPACE'S LEASE: never checked before acquiring, never assumed
     across a release — the whole of the old recheck machinery, made
     structural (ruled 2026-08-28).
   - INVARIANT — ONE-TRANSACTION ORPHAN CLOSE: when a workspace shuts
     down, everything that never got a terminal is closed together in a
     single database transaction (turn claim retired, interrupted
     markers written, last-activity stamped) — a crash mid-teardown can
     never leave half the bookkeeping done (ruled 2026-08-28).
   - INVARIANT — ALL-OR-NOTHING HOLD RESTORE: at boot, held prompts
     restore from durable storage whole; a corrupt record restores
     NOTHING, loudly — never a partial set that silently loses what
     users typed (ruled 2026-08-28).
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
   - TWO MERGE METHODS (ruled 2026-08-28), keyed self-repo-or-not (the
     common-dir identity the self-reload check computes); MULTI_REPO_ROOT
     is ACCOUNT SELECTION ONLY, never merge strategy. EMACS REPO:
     pre-prompt (lease, if configured) → NO-FF MERGE COMMIT onto the
     default branch (one commit to apply, one to revert; conflicts via
     the parked-lease spec) → tests on the merge commit + fixes (lease)
     → the rollout bounce → post-prompt (lease, if configured).
     EVERYTHING ELSE: pre-prompt → post-prompt, nothing more — PR
     creation, landing, tests are the prompts' job there. The per-repo
     queue and the terminal/teardown path are IDENTICAL for both.
   - TABS (revised): queue | pre-prompt | merge | conflicts | tests |
     fixes | post-prompt — resolved: queue/merge/tests; agentic:
     pre-prompt/conflicts/fixes/post-prompt; all conditional
     structurally; PARKED only on conflicts and fixes.
   - RULED: the configured prompts run for EVERY merge on EVERY
     ingress, Emacs included — read from the WSM creation-job facts. A
     SESSIONLESS workspace with a configured prompt gets a session
     STARTED under the lease (revival-is-implicit); only a workspace
     with no configured prompts merges truly sessionless.
   - CONSEQUENCE for self-reload: the landed range is the merge
     commit's second-parent history (default..branch) read off the
     commit — the cherry-pick-annotation walk dies with cherry-picking.
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
   RULED (2026-08-28): teardown NEVER interrupts the vendor — graceful
   shutdown means waiting for freeness, so the old interrupt-before-
   disconnect step is REJECTED outright; repeated refusals log
   rate-limited with exact suppressed/total counts, never a flood.

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

10a. PRESCRIBED — THE TOPBAR RESOLVER (the response side's first
   prescribed component).
   - RESPONSIBILITIES: resolve and push the whole TopbarView on any
     change: title + session line (WSM naming and session facts), model
     selector (catalog + current selection), connectivity (link state),
     warnings (accounting + unmodeled + pulled diagnostics), the CONTEXT
     CHIP, the ACCOUNT element.
   - CONTEXT: arrives as WatchSession's pushed context_usage arm (the
     vendor's own answer, NEVER derived from usage frames — the
     correctness ruling), routed by the sessionwatcher; the /context
     panel resolves from the SAME fact.
   - ACCOUNT: the config dir comes from WSM's spawn-identity facts; the
     resolver reads that root's .claude.json for the email; logged-out
     is a drawn state, never blank.
   - PREREQUISITES: WSM, shim client.
   - ACCOUNT-SWITCH CONSTRAINTS (mechanics the wave's, constraints
     binding): the config dir is DETERMINED by the daemon — workspace's
     main repo under $MULTI_REPO_ROOT → the multi-repo config dir, else
     the default — never by the shim; switching one to the other PORTS
     the vendor transcript between the two roots' project dirs, and the
     DAEMON does the porting itself (a file move before the ordinary
     resume under the new root; no shim involvement — a deliberate
     simplification over shim-owned porting).
   - INVARIANT — THE ACCOUNT IS DETERMINED, NEVER SELECTED: the
     repo-under-root rule is the ONLY source of a workspace's account;
     no request field, no override, no inheritance carries one — the
     old create-time explicit selection DIES, and the implementation
     must make a differently-accounted workspace structurally
     unrepresentable (there is no input through which one could be
     asked for).

10b. PRESCRIBED — THE SESSIONWATCHER (the user's design, settled
   2026-08-28; supersedes the session-manager and response-handler
   shapes from the same walk). ONE INSTANCE PER WORKSPACE/SHIM, with
   exactly three responsibilities:
   1. WATCH THE STREAMS: it owns every shim watch for its session —
      WatchSession, the turn's WatchAgent, one per live detached item
      (WatchAgent / WatchBash / WatchWorkflow) — opened EAGERLY on
      announcement (liveness is structural: the open set IS the
      live-work set), held for the item's life, reaped at terminals.
   2. SOURCE OF TRUTH ON CONNECTIVITY: it is the ONLY thing watching
      the workspace's shim, so "is the session connected" is its
      answer and nobody else's (invariant 11's daemon↔shim hop).
   3. ROUTE, FANNING OUT AS NEEDED: every stream response routes by
      type to the corresponding resolver(s) — activity to feed (via
      the standing output address) and footer and accounting;
      questions/permissions to feed and footer; turn terminals to
      feed, footer, and the turn-lifecycle announcement the prompt
      queue drains on; detached-work announcements to the feed's
      bubble head plus opening that item's own watch; session-update
      arms per kind (diagnostics and context usage to the topbar
      resolver, account usage to the footer, query died to footer and
      feed).
   - NO PULLS, NO WRITES: the former pull verbs are FOLDED INTO
     WatchSession as pushed arms (diagnostics 25, context_usage 26),
     so the sessionwatcher is purely stream-consuming. Anything the
     daemon SENDS is not its concern: prompt submission goes through
     the prompt queue only; simple synchronous reads
     (ReadHistory-class) may call the shim client directly; but ALL
     async streaming data enters the daemon through the sessionwatcher
     — no other daemon code consumes shim streams.
   - THE TWO-LEG DETACHED-WORK FLOW (invariant): the daemon↔shim leg
     is EAGER (above); the webapp↔daemon leg is LAZY — an expand's
     OpenFeed decodes the bubble's FeedId, serves the newest page from
     history, and WatchFeed merely SUBSCRIBES the webview to rows the
     resolver was producing regardless; collapse cancels only the
     client leg. The expand never creates a shim-side route.
   - PREREQUISITES: shim client; the resolvers and prompt queue
     consume from it.

11. INVARIANT — CONNECTIVITY TRUTH PER HOP (the user's specification,
   2026-08-28): a workspace's connected state is witnessed ONLY by the
   liveness of its three standing streams — shim.v1 WatchSession for
   daemon↔shim, agentrepl.v1 WatchWebWorkspace for webapp↔daemon,
   agentrepl.v1 WatchHostWorkspace for Emacs↔daemon — the workspace is
   CONNECTED iff all are live, and any one down means not connected.
   Silence on a live stream is never evidence of anything.

10c. PRESCRIBED — THE FIVE RESOLVERS, existence and purpose only:
   exactly these five exist — the FEED resolver, the FOOTER resolver,
   the TOPBAR resolver, the SIDEBAR resolver, and the HOLD TRAY
   resolver — each with ONE general purpose: convert response items
   from conversation.v1 (and daemon facts) into its component's
   frontend.v1 view for verbatim rendering. Their exact
   responsibilities and internals are DELIBERATELY NOT PRESCRIBED —
   the implementing orchestrator's, within the already-landed
   invariants.
   - RESOLVER STATE IS FINE, UNPERSISTED: a resolver may accumulate
     in-memory state across piecemeal frames (the topbar resolver
     assembling its view from facts arriving in different WatchSession
     frames) and ships COMPLETE SNAPSHOTS only — the webapp never
     tracks partial state, because the contract's non-optional fields
     are semantically non-optional and a partial push would violate
     them; accumulation happens daemon-side, before the wire.

12. DISCRETIONARY — THE CONNECT SERVER AND PUBLISHERS: the rpc
   handlers validate per the base-function convention and DELEGATE to
   the landed components (SubmitPrompt → the prompt handler, workspace
   verbs → WSM and the orchestrators, adopts → the rollout rendezvous,
   answers → the queue and shim); publishers deliver resolver views to
   subscribers. Internals are the implementing orchestrator's.

13. INVARIANT — SUBSCRIPTIONS NEVER MISS AND NEVER END STALE: a Watch
   subscriber receives every complete view published from its
   subscribe point onward, in order, with the most-recently-published
   view (IF one exists) delivered first; no published view ever falls
   in the crack between subscribing and receiving. This does NOT mean
   immediacy: when a resolver has not yet produced a complete view,
   the subscriber's first frame is the first view ever published —
   absence of any frame is the legal "not yet resolved" state, and an
   empty or partial message is never sent (the non-optional-fields
   rule). Applies uniformly to every Watch stream; the feed's
   token-pinned tail is this same guarantee's feed spelling.

14. NOTES carried from the conventions walk (2026-08-28):
   - SUBAGENT PARITY: a subagent is handled exactly as the turn is —
     same write type, same frame type, same daemon-side queuing —
     differing only in address.
   - SPAWN / ATTACH / END ARE DECOUPLED: sessions and turns outlive
     the daemon; spawning is unary, attaching is a Watch that creates
     and ends nothing, ending is an explicit Kill — restart recovery
     is re-running the corresponding Watch.
   - STATE PLACEMENT IS A STRONG PREFERENCE, not a hard rule: the
     daemon holds state and most of the persistence model; the shim,
     store and sidecar stay constant-cost — exceptions need a good
     reason and should be rare. The store exists solely to persist
     vendor information: a datalayer client, never a state manager.
   - THE STORE IS NUKED, NEVER MIGRATED: no backfill, hydration, or
     migration code anywhere; where contents are in the way, drop and
     recreate.

## OPEN — unruled audit findings (the triage backlog)

These feature-loss audit findings are NOT yet ruled; each awaits a
remediate / do-not-remediate ruling per meta rule 14:

- SHIM-CONNECTION group: ALL RULED 2026-08-28 — redial/readiness/
  first-connect/crash-adopt landed on the shim client entry; message
  distribution resolved by THE RESPONSE HANDLER prescription (10b);
  the model list is SessionStarted's catalog routed through the
  handler to the topbar resolver; connectivity truth landed as
  invariant 11 (per-hop stream liveness).
- INTAKE SIDE EFFECTS group: DROPPED FOR THIS PROJECT (ruled
  2026-08-28) — auto-declining parked permission asks, cancelling owed
  redeliveries, engagement declaration at the funnel, and the
  synchronous accepted-edge publish with retraction are all future-PR
  material; for now the landed permission/question API (fully
  webapp-side) is the only path.
- DRAIN group: ALL RULED 2026-08-28 — freeness-under-the-lease,
  one-transaction orphan close, and all-or-nothing hold restore landed
  as WSM invariants; rate-limited refusal logging adopted (drain
  entry); interrupt-before-disconnect REJECTED (teardown never
  interrupts the vendor).
- MERGE leftovers: dequeue-offer timing (the old flow raises it from
  the INTERRUPT, our entry says at completion); the agent-driven
  merge-skill detached window (a separate concept from the daemon
  merge); cross-repo multi-queue membership (one workspace queued on
  several repos; Standing reports the first, Dequeue takes all).
- MERGE-VARIANTS findings: ALL RULED as of 2026-08-28 (evidence:
  docs/overhaul/reports/merge-variants-2026-08-27.md). DEAD BY
  RULING: the old boot-time repair of missing merge layout facts (the
  facts are recorded at creation and refused when absent — no repair
  exists); Emacs's durable merged/merge-failed memory AND the
  merged-tab hiding/greying (removed outright — the information is
  deliberately not provided); the doom-multi-repo-mode toggle (killed
  from Emacs too — path-under-$MULTI_REPO_ROOT is the ONLY account
  rule everywhere). WONT-DO (gap accepted): --pr-was-merged has no
  engine counterpart; parent-notification-on-child-merge is not
  implemented.

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
