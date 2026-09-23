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
   - USAGE: the lease is HYBRID by ruling — kernel file locks stay the
     cross-process ARBITRATION mechanism (self-releasing on death, no
     stale-pid state), and WSM holds only the lease's POLICY metadata
     (holder label, refusal policy): the lock decides, the row
     describes. LOCK HOLDER (ruled 2026-08-29): the SHIM holds the two
     conversation locks (session-keyed + workspace-keyed) for its
     lifetime — the daemon PROBES them before spawning; a fresh daemon
     boot reads a held lock as "a surviving shim owns this
     conversation" even before that shim dials in. Daemon-side
     occupancy (which peer may drive the session) is the WSM lease
     row's, in-memory-guarded per the shim-client mutex. The lease projects PER-HOLDER REFUSAL POLICY onto new
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
     - FEED PAGE POSITION (corrected 2026-08-29): per-READER and
       EPHEMERAL — the walk position is keyed to the open connection,
       dropped at every open, and never persisted; a fresh or
       re-attached webview lands at the tail and pages back. Nothing
       about the walk survives a restart (the earlier persist-per-
       workspace prescription is superseded).
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
   - RULED 2026-08-29 (final-audit triage):
     - FRESH DATABASE: the rebuilt daemon starts with a FRESH WSM file;
       the old state.db is abandoned in place — no import, no
       migration (consistent with no-backwards-compat).
     - CORRUPT ⇒ REFUSE, GENERALIZED: a corrupt or partially-readable
       durable record refuses the whole load LOUDLY — never a
       fabricated default — for EVERY table (the all-or-nothing
       hold-restore rule, made general).
     - FAULT CLOSURE: fault records carry open/closed with a PERSISTED
       resolved-at instant; the daemon writes the closing edge when
       the condition clears — a card that reopens unresolved on every
       boot is unrepresentable.
     - TASKS: WSM stores the user's task list and workspace↔task
       assignments (the task-verbs increment writes them; the roster's
       task view reads them).
     - MERGE-LEASE LEDGER: a minimal durable ledger per merge (lease
       id, tab intervals) so replay resolvers can join it with the
       persisted turn origin and rebuild historical merge-bubble tab
       membership exactly.
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
     diverge) — as a CONSTRAINT, not a default. MODEL RULES (ruled
     2026-08-29): the model FACT is last-writer-wins by SHIM-observed
     order (the shim serializes its set-confirmation and the stream's
     re-announcement, so the daemon stores one ordered truth — an
     in-flight submit can never silently revert a user's model change);
     bare argument-less /model is refused/absorbed daemon-side (the
     CLI's own picker is unreachable through us — the topbar picker is
     the only argument-less path).
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
   - ORIGIN IS DURABLE (ruled 2026-08-29): every delivery's PromptOrigin
     persists onto the turn's durable record via StartTurn (the
     prompt-origin increment) — replay resolvers read it to route
     merge-born rows and to label restart re-drives instead of drawing
     them as fresh user turns.
   - BOOT RECONCILIATION (ruled 2026-08-29): on boot/adoption the daemon
     re-opens watches for (or reconciles via GetLiveWork) every
     in-flight daemon-originated turn, so no machine-submitted turn is
     ever unwatched to completion.
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
       sessionless; the displaced user turn is captured durably, then
       ENDED (KillTurn) once the capture is durable, then resubmitted
       exactly once at lease release, across a daemon bounce (ruled
       2026-09-02; confirmed against the daemon's implementation). The
       kill is UNFORCED (ruled 2026-09-23): it ends the synchronous turn
       only, its detached work runs on, and the admitted merge waits on
       the fleet's freeness before it drives the session.
   - RULED (merge-variants investigation):
     - TWO INGRESSES, ONE ENGINE: Emacs merges arrive via the
       MergeWorkspace rpc, non-Emacs merges via the workspace
       command-file merge verb; the engine is identical behind both, a
       merge never round-trips through Emacs, and a workspace with no
       live session merges sessionless (skipping displaced-turn capture
       and post-merge teardown).
     - THE FILE ROUTE IS A GENERAL INGRESS (ruled 2026-08-29): the
       durable command-file protocol survives beyond merge — its
       inbound verbs (prompt, send, create, close, open, and the rest
       scripts and skills dispatch) map onto the SAME internal paths as
       the corresponding rpcs (prompt→queue, close→CloseWorkspace, …);
       only the daemon→Emacs direction is dead.
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
   - AGENT BRIEFS ARE FILES (ruled 2026-08-29): the synthesized briefs
     (conflict resolution, test-fix with its escalation marker,
     add-support) are read from the prompts/ directory at USE time —
     customizable without a rebuild, loud on a missing file or bad
     placeholder; never compiled-in constants.
   - GOTCHAS: phase history is feed content, not WSM columns (the WSM
     merge-lease ledger records only lease id + tab intervals for
     replay reconstruction — never the content); an
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
        workspace's shim (which keeps running and KEEPS its kernel
        locks — the locks are shim-held, ruled 2026-08-29), records the
        handoff in WSM, and announces the transfer on the OLD
        connection. Emacs then treats the new connection as that
        workspace's home and tells the NEW daemon "workspace X is
        yours — resume pending operations." The new daemon adopts the
        running shim, claims serving ownership (WSM facts — the kernel
        locks are the SHIM's, held continuously through the handover),
        drains the held intake in order, and pushes fresh views (the
        webview re-attaches at the tail per the per-reader page rule).
     5. DRAIN AND EXIT. After the last transfer the old daemon exits
        gracefully; the new daemon's WSM handle becomes the sole
        writer.
   - THE WIRE (landed post-freeze increment): Emacs holds `WatchDaemon`
     (push `shutdown_announced { address }` — ENRICHMENT RULED
     2026-08-29: the arm gains cause, a bounded expected outage, and a
     minted-at instant; absence of an address means a plain bounce, so
     clients can draw "restarting (reason)" instead of the severed-link
     treatment); `WatchHostWorkspace` gains
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
   - ASSET ORIGIN (ruled 2026-08-29, what makes the hot swap real): the
     DAEMON serves the webapp's assets; the HTML entry point is
     re-stat'd per request so a rebuild self-corrects with no restart;
     `Cache-Control: no-store` on the entry point ONLY (the fix for
     webviews pinning a deleted bundle).
   - SHIM RELAUNCH (product spec — the settled flow; the same engine
     serves the build-staleness bounce, one engine two triggers):
     1. REBUILD once; per live workspace, independently and in
        parallel, PRELAUNCH the new shim process — up and
        daemon-connected but INERT by construction (no session started:
        no vendor process, no store writes, no keep-alives), so it
        coexists with the old shim indefinitely.
     2. WAIT FOR FREENESS (no in-flight turn, no live detached work).
        Freeness at shim kill is an INVARIANT of the rollout's design —
        by construction nothing is running under the vendor process
        when it dies, so killing the CLI is inconsequential by design
        (ruled 2026-08-29; no orphan-process question arises).
        Never-free gets the same wait-forever + periodic-warn ruling as
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
     switch; re-attach lands at the tail per the per-reader page rule.
   - INVARIANT — no daemon↔daemon channel: coordination is Emacs relay
     + WSM facts + the shim-held kernel locks only (a daemon crash
     mid-window leaves every workspace adoptable by the survivor — it
     dials the still-locked shim and claims serving ownership in WSM).
   - INVARIANT — WSM contention scope: during the overlap every
     mutable WSM fact is workspace-scoped (arbitrated by that
     workspace's recorded serving ownership) or repo-scoped (the
     per-repo merge queue gets its own kernel lock); any future
     cross-cutting table must be ownership-scoped or rollout-frozen.
   - BOUNCE ACCOUNTABILITY (ruled 2026-08-29): at stand-down the
     outgoing daemon writes an intent manifest (per-session shim pid +
     intent); the incoming daemon reconciles it against the kernel
     locks actually held, and PRESERVED / ROLLED / DIED / UNKNOWN are
     never collapsed — after a crash or force-kill, which sessions
     silently died is surfaced per workspace, not counted.
   - BOOT EXCLUSIVITY (ruled 2026-08-29): a daemon binds its address
     FIRST as the exclusivity claim, before touching any socket; a
     successor is distinguishable because it is SPAWNED with an
     explicit joining argument — an unflagged second daemon loses the
     claim and exits without disturbing the incumbent's listeners.
   - DEPLOY CHAIN (ruled 2026-08-29): build mechanics and ordering are
     the ONE deploy script's domain — the rollout invokes it, never a
     second build path; a proto-prefix change classifies as touching
     every consumer of the regenerated bindings.
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

14a. PRESCRIBED — THE GIT CLIENT (the second LEAF client, the shim
   client's sibling: a dumb, no-policy executor of git actions,
   knowing nothing of WSM or any peer; the peers compose it with WSM
   exactly as they compose the shim client — creation records facts in
   WSM and materializes through this client; the merge orchestrator
   drives the landing through it under the lease; teardown calls it
   after terminal publication; the self-reload trigger uses its
   identity check).
   REQUIREMENTS (implementation details the orchestrator's):
   - CREATION: derive the slug (from the initial prompt when present),
     the branch from the slug, the worktree dir; resolve the optional
     base ref (default: the repo's default branch); run the git;
     registration happens only after the worktree materializes; the
     merge layout facts (source branch, source dir, target dir,
     origin) are recorded at creation — a merge is REFUSED when they
     are absent, never guessed.
   - THE MERGE LANDING: a NO-FF merge commit onto the target's default
     branch — never cherry-pick, never rebase; conflicts are detected
     and left staged for the conflicts flow; rollback is reverting the
     one merge commit; a git failure fails the merge LOUDLY with the
     git output preserved as evidence.
   - POST-MERGE WORKTREE REMOVAL: daemon-owned, only AFTER the merge's
     terminal publication — never mid-run.
   - NUKE: delete the worktree AND the branch, forced.
   - SELF-REPO IDENTITY: the git common-dir comparison with symlink
     canonicalization — the one computation behind the merge-method
     split and the self-reload trigger.
   - LANDED-RANGE DERIVATION: the merge commit's second-parent history
     (default..branch), read off the commit itself.
   - LOCAL ONLY: the daemon never pushes, fetches, or touches remotes
     — remote work is the prompts' agents' business.
   - ENV HYGIENE (ruled 2026-08-29): every git invocation strips
     inherited repository-selecting GIT_* env vars (git hooks export
     them into children); `-C dir` is the ONLY repository selector.
   - PREREQUISITES: none (leaf).

15. RULED (the internal-components close, 2026-08-28):
   - FAILURE CLASSIFICATION IS NOT A COMPONENT: it falls out of the
     spec'd design — the shim produces the vendor taxonomy's typed
     arms, the feed resolver respells turn errors per its arm table,
     faults arrive typed on the diagnostics push, refusals are derived
     error arms per verb; what remains is per-site switches inside
     components that already exist.
   - ACCOUNTING IS NOT A COMPONENT, and the SESSIONWATCHER ROUTES ONLY
     TO RESOLVERS — never to any non-resolver aggregator. Each
     resolver owns determining what information it needs to collect,
     accumulates it IN MEMORY (never persisted — aggregation, not
     storage), forwards its view only once it is COMPLETE (the
     non-optional rule: the first message sent is fully populated;
     before that, no message is the legal state), and thereafter
     forwards the updated whole message immediately on every relevant
     sessionwatcher frame. The footer resolver accumulates the turn's
     figures; the topbar resolver the session's — same facts, each
     resolver's own accumulation, no shared accumulator.

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

## Contract increments owed (final-audit triage, 2026-08-29)

Sanctioned post-freeze increments ruled at the triage; each lands as a
proto change (project-lead-only edit class) with its owning doc updated:

- CREATION FACTS: `CreateWorkspaceRequest` gains optional
  before_ws_merge + postprocessing_prompt, priority, fork-from (source
  workspace/conversation), the ungated-permission consent flag (refusal
  without it when the mode disables the gate), user-supplied name,
  parentage (source workspace), and model — plus a dedicated ONE-SHOT
  creation form (likely an arm): Emacs supplies {prompt, model,
  parentage} and the DAEMON owns the whole sequence (naming, worktree,
  prompt decoration, self-merge/PR postprocessing).
- LOGIN: `OpenLogin` + one duplex byte stream (pty bytes out, keystrokes
  in, a resize control) + close — per-account idempotent; the daemon owns
  the pty running `claude /login` exactly as today; logged-out detection
  stays the .claude.json email probe.
- HIBERNATE: a shim.v1 directive — daemon calls, shim compacts then
  acks, daemon stands the shim down after the ack.
- SHUTDOWN ANNOUNCEMENT ENRICHED: `shutdown_announced` gains cause,
  expected-outage bound, and minted-at (no address = a plain bounce, not
  a handover).
- DRAIN SCHEDULE: `UpdateShutdownSchedule.schedule` gains a REQUIRED
  reason; a drain-scheduled push (WatchDaemon arm) carries reason +
  at_ms so every client can draw the standing banner.
- FAN-WIDE CANCEL: `CancelDetachedAgents` verb (typed outcome) + an
  `Interrupt` refusal arm `confirm_required{live_agent_count}` answered
  by resending with confirm.
- `OpenExternal{url}` (WEB LINK section): the daemon opens the pinned
  external browser profile.
- `SetPermissionMode` (TOPBAR section, mirrors SetModel).
- `SetWorkspacePriority` (or an UpdateWorkspace arm); the roster
  resolver orders by priority and the roster row carries the badge fact.
- TASK VERBS: CreateTask / UpdateTask{title|done} / AssignWorkspaceTask;
  WSM stores tasks + assignments; the roster's task view becomes
  fillable.
- TYPED NOTIFICATIONS: the host `notification` push gains a typed kind
  oneof (agent_addressed | permission_requested | ...) beside the
  composed text; a permission ask FIRES the push and sets the attention
  marker (Emacs's existing focus policy does the rest).
- PROMPT ORIGIN DURABLE: StartTurn's origin persists onto the turn's
  durable record (conversation.v1/store) so replay can route merge-born
  rows and label restart re-drives.
- SHIM BUILD SHA on SessionStarted (the staleness bounce's carrier).
- `compacting` SessionUpdate arm (vendor auto-compaction start signal).
- /CONTEXT RICH SCHEMA: SessionContextUsage (and context_panel) are
  redesigned by the orchestrator to carry the vendor's FULL
  get_context_usage answer, strongly structured (tool calls as their own
  encapsulated list, etc.); the webapp renders the panel custom with
  tool calls in an auto-folded foldable render.
- `RestartWorkspace{force}` semantics: the daemon owns everything the
  restart entails INCLUDING bouncing the webapp view (via reload_webapp
  after the ack if Emacs coordination is needed).

## Contract context (for implementers)

Orientation for daemon implementation agents. The .proto files under
`proto/src/` are the contract; their comments are the authoritative
documentation — read the files you implement against. This section is the
map: the ideas, the package layout, and how the pieces relate. Generic
wire/schema conventions (identity spaces, echo tokens, response-outcome,
bounded streams, presence, no-seq, no-keepalive, the file/package model)
live in `the teamlead prompt (standing conventions) and the proto comments` — read that too;
nothing there is restated here. The full design record is
the canonical design record (the project lead's; on conflict, escalate).
Implementers never touch protobufs — a needed proto change is routed
upward, never made.

### What the daemon IS

- The ONE component allowed to hold state.
  - Shim, store, and sidecar hold NO variable-size state; every
    observation there costs a constant number of single indexed lookups.
  - The daemon's durable state (WSM) is an in-process SQLite library, one
    writer, no wire shape (`state.v1` was deleted; internal DDL, not proto).
- Resolvers own ALL formatting; clients derive NOTHING.
  - No client-side precedence tables, phase→word maps, color maps, label
    lookups, arithmetic, or path→URL mapping — daemon-resolved values ride
    the wire and re-publish on every change.
  - One narrow exception: the cold-context gate ships raw facts (token
    count, last-request instant, model) and the client owns wording.
  - The daemon parses ANSI (merge test output) and syntax-highlights code
    into paint-class spans itself; clients only paint classes.
- The daemon is the ONLY prompt queue.
  - The vendor binary's internal queue is never modeled or surfaced; the
    daemon submits only when no turn is in flight for that agent.
- Command recognition is daemon-side and transparent: the client submits
  the composer text whole; `SubmitPrompt`'s success arm forks into "a
  minted turn" vs "a recognized command's panel" — clients know no
  command names.

### Package map

All packages under `proto/src/`. Each service file (`service.proto`) lists
every rpc with one `endpoint_<rpc>.proto` per rpc; shared vocabulary gets
its own file only when >1 endpoint needs it.

- `agentrepl/v1` — the Connect service the daemon SERVES to Emacs and the
  webapp. Sections mirror drawn components plus control surfaces:
  - FEED: SubmitPrompt; OpenFeed → WatchFeed (open mints the token, watch
    tails it) + GetFeedPage (daemon-held walk, first/next); Interrupt;
    AnswerPermission / AnswerQuestion / AnswerColdGate.
  - SIDEBAR: WatchWorkspaceRoster (the ONE global stream — no workspace
    field; every webview watches the same roster) + the workspace verbs
    Create/Open/Close/Kill/Nuke/Merge/Restart.
  - TOPBAR: WatchTopbar + SetModel. FOOTER: WatchFooter.
  - DAEMON-HOLD TRAY: WatchDaemonHolds + UpdateHeldPrompt + AnswerHeldOffer.
  - DAEMON ADMIN: UpdateShutdownSchedule, UpdateMergeQueue, DaemonHealth,
    SessionHealth, ClientLog.
  - HOST (Emacs-only, not drawn): RegisterWorkspace (idempotent by dir),
    SelectWorkspace, WatchHostWorkspace (per-workspace, correlation and
    composer gating), WatchDaemon (daemon-scoped facts, e.g. shutdown
    announcement), AdoptHostWorkspace.
  - WEB LINK: WatchWebWorkspace + AdoptWebWorkspace — the webview's
    standing daemon link the graceful rollout rides.
  - Every per-workspace request carries `WorkspaceRef`; requests are
    verbs, never state; error arms start EMPTY and are derived from the
    daemon's real refusal sites at implementation time.
- `frontend/v1` — the views the daemon's resolvers produce, one file per
  drawn component: feed (self-similar — a subagent bubble IS a feed),
  sidebar, topbar, footer, daemon_hold (the held tray), the slash panels
  (status/context/agents/help/mcp/todos), failure (the entry-less failure
  vocabulary). Never imports `agentrepl.v1`; both import `workspace.v1`.
- `shim/v1` — the service the daemon CONSUMES, one shim per session.
  Sections: Session (StartSession/WatchSession/SetSessionModel/
  SetSessionPermissionMode/KillSession), Agent (StartTurn/WatchAgent/
  UpdateAgent/KillTurn), Detached work (WatchBash/StopBash,
  GetWorkflow/WatchWorkflow/StopWorkflow, DetachForeground), History
  (ReadHistory, next-only). Wraps `conversation.v1` frames; no paint
  attestation — a response never claims anything was rendered.
- `conversation/v1` — the shared conversation VOCABULARY every surface
  reads (agent frames, activity units, content blocks, permission,
  question, session start / cold context, slash commands, turn identity,
  detached work, workflow, vendor API outcomes, history pages). Leaf-most;
  vendor-content-neutral block model; carries vendor fields even with no
  UI consumer (fidelity layer).
- `workspace/v1` — one line: the leaf identity package minting
  `WorkspaceRef`/`RepositoryRef` echo tokens.
- `store/v1` — NOT the daemon's surface. THE DAEMON MUST NEVER IMPORT IT
  (an isolation gate in `store.proto`'s header forbids it). Its callers
  are the shim and the sidecar only; everything the daemon is entitled to
  see is `conversation.v1`, served over `shim.v1` (ReadHistory + the
  Watch streams).

### Identity, as the daemon lives it

- The daemon MINTS: `TurnId` (returned by SubmitPrompt; stamped on the
  feed rows a turn produced — the client matches its own prompt by it,
  no optimistic rows, no pending-request maps), `WorkspaceRef` /
  `RepositoryRef` (a path is never an identity — Register/Create return
  the id), `FeedId` (ENCODE/DECODE, not a table: the daemon encodes the
  identity of what the row draws and decodes it on echo, so the same row
  gets the same id across pushes, restarts and replays; a bubble row's id
  IS its sub-feed's OpenFeed address), session and controller-generation
  identities on the host stream.
- Typed identity spaces (unit, agent, task, ask) STOP at the daemon; the
  frontend holds only "which row" and "inside what" — the one typed
  survivor on the client wire is `TurnId`.
- The shim mints unit ids (`activity_id`, stable for a unit's whole life)
  and the durable `main_agent_id`; vendor identities (uuid, message.id,
  session id) never cross the daemon↔client contract.
- `SubmitPrompt` alone carries a client-minted `idempotency_key`
  (duplicate refusal) — distinct from `TurnId`, which the daemon mints.

### Session lifecycle: spawn / attach / end, resume, the cold gate

- Sessions and turns OUTLIVE the daemon; the three acts are decoupled.
  - SPAWN is unary and returns an identity the daemon persists
    (StartSession, StartTurn — StartTurn returns once the prompt is
    accepted, WITH the first page).
  - ATTACH (WatchSession/WatchAgent/WatchBash/WatchWorkflow) creates
    nothing and ends nothing; closing it leaves work running; opening
    late or after a daemon restart misses nothing (opens with a page, or
    the gap above the caller's own known-through mark).
  - END is a Kill that refuses while work is live unless forced and NAMES
    what it killed; narrow stops stay single-target (UpdateAgent.stop,
    StopBash, StopWorkflow, agentrepl Interrupt) — plus the ONE
    fan-wide verb ruled 2026-08-29: CancelDetachedAgents stops every
    detached agent, and Interrupt with live agents refuses with
    confirm_required{live_agent_count}, answered by a confirm resend.
- On restart the daemon simply re-runs the Watches from persisted
  identities and its persisted opaque history pointer; catch-up is the
  same open-with-a-page path as cold paint.
- Cold gate: a cold context (cache lapsed, or model switching) is REFUSED
  with its cost named, never silently paid. The daemon reopens naming a
  remediation — pay | clear | compact{model, scope} — chosen by the user
  (AnswerColdGate) or daemon policy. Compaction is shim-implemented via a
  throwaway session; no consumer sees it. The footer only says the
  session is parked; the feed's gate row is the answering surface.
- Hibernation is daemon POLICY (idle-cutoff sweep + implicit revive on
  prompt) with ONE wire act (ruled 2026-08-29): before standing a shim
  down for hibernation the daemon calls the shim's Hibernate directive —
  the shim compacts, then acks; only after the ack does the stand-down
  proceed, so revival never pays a cold context. No frontend-visible
  fact beyond the cold gate.
- SPAWN ON MOUNT (ruled 2026-08-29): mounting a parked workspace's
  frontend IS an implicit revival — the shim spawns and ReadHistory
  serves; there is no shim-less read path (the store isolation gate
  stands absolute).
- FRESH-CONVERSATION INVARIANT (ruled 2026-08-29): StartSession(fresh)
  is legal ONLY with proof the workspace never had a conversation at
  all; anything else resumes or refuses loudly — a conversation is never
  silently replaced (abandonment is irreversible; every alternative is
  recoverable). Mechanism the implementer's; the rule is binding.
- RESUME GUARDS (ruled 2026-08-29; AMENDED 2026-09-03): the guard is now
  ONE TRANSCRIPT-AWARE SOURCE CLASSIFIER used by BOTH bring-up paths —
  the cold start and the rollout's relaunch resume. No record or an
  empty vendor id is FRESH; a deleted session refuses; a vendor id whose
  transcript is FOUND resumes; a vendor id whose transcript is MISSING
  comes up FRESH, opening a `conversation_abandoned` fault once with the
  abandoned vendor session id as evidence. The AMENDMENT retires the
  old blanket refusal: a session that pre-minted a vendor id and never
  took a turn writes no transcript, so refusing it cost the workspace
  its session entirely (the shim answered `unknown_session`, no client
  was installed and every prompt then answered `no_session` forever).
  The trade-off is stated: a truly VANISHED transcript now also comes up
  fresh, with the fault as the record of what was abandoned rather than
  a refusal. A resume that still reaches the shim without a transcript
  gets the NAMED `unknown_session` arm, and a hard resume failure keeps
  its ruling — the workspace's own error, remediated as it comes up,
  with NO retry machinery; after resume the shim
  VERIFIES the query landed on the exact conversation asked for —
  identity mismatch is an error (/clear discharges the commitment).
- Workspace verb triad: Close = view-level, requires quiet (live work or
  held prompts refuse it — undelivered user intent is never silently
  discarded; a standing cold gate or a parked session does NOT block);
  Kill = forced session death, never blocks, data survives; Nuke = kill
  then delete worktree and branch — the only data destruction.
- Resume gotcha: model and permission mode are recoverable from the
  transcript but the SDK does NOT restore them — the shim passes them
  back explicitly; the daemon carries the cold-gate remediation into the
  next open. Account/config-dir is DETERMINED (path under
  $MULTI_REPO_ROOT), never selected; the daemon itself ports the vendor
  transcript between roots on an account switch.

### The shim boundary, from the daemon's side

- A subagent is handled EXACTLY as the turn: one write type, one frame
  type, one queue — requests differ only in the address. "Main agent"
  never appears on the API.
- The shim forwards what CHANGED and accumulates nothing; the daemon owns
  the fold — the feed resolver keeps the prose buffer per in-flight unit.
  Response/thinking deltas have no offsets; the terminal arm restates the
  WHOLE text, so a lost fragment self-corrects (bash keeps its offset —
  no settled whole exists to recover from). ONE relaxation of the
  no-client-timer rule (ruled 2026-08-29): a streaming-preview card whose
  stream has DIED may be retired client-side — a torn-down query sends no
  terminal, and nothing else can retire the card.
- `start` means "this stream now carries this unit," not "work began"; a
  re-announcement after detach or restart repeats the ORIGINAL start
  instant (recovered from the store, not shim memory). `update` arms
  exist only for kinds with genuine growth (response, thinking, bash).
- Edges vs levels: the vendor's live-background-task set is a LEVEL with
  replace semantics; the daemon must NEVER pair start/end edges to
  reconstruct membership (ordering vs the level is unspecified upstream).
  Detachment's defined instant is membership change in that set.
- Announcement rides the spawning stream, recursively; provenance is
  implicit in which stream announced an item. One stream per detached
  item; zero item streams structurally IS "no detached work in flight."
- Keep-alives are INVISIBLE on the control plane: the shim submits them,
  yields to real work (rewind + discard), and never lets the daemon see
  one — but they are visible on the record plane (accounting; superseded
  turns excluded from replay, so paged history must tolerate them).
- Shim health is stated, not timed: heartbeats are relayed as facts and
  the daemon re-pushes "last progress" instants (clients tick locally);
  the shim owns the wedge ruling; a stream ending without a terminal
  frame is the transport failure.
- Two-leg detached flow: daemon↔shim watches are EAGER (opened on
  announcement — the open set IS the live-work set); webapp↔daemon is
  LAZY (a bubble expand only subscribes to rows already produced;
  collapse cancels only the client leg).
- Live-work reconciliation is the SHIM's (`GetLiveWork` against the
  store at session start: re-adopt or write the closing terminal); the
  daemon's invariant is simply that every started thing eventually gets
  a terminal row.

### Queue, holds, leases — contract facts

- One turn in flight PER AGENT, structurally: a second submit while a
  turn runs on that agent is a daemon fault, refused outright.
- A prompt submitted while a turn runs is HELD daemon-side (WSM,
  keyed by TurnId, content is a `conversation.v1.UserSaid` — one
  canonical form client → daemon → tray → shim → record), classified,
  and delivered later; the vendor never sees it until then.
- The held tray is its own component and stream (WatchDaemonHolds),
  whole-list-replaced: `HeldPrompt` (deliver/force/cancel via
  UpdateHeldPrompt) and `HeldOffer` (a daemon-parked question, e.g. the
  merge-dequeue offer, answered via AnswerHeldOffer). Gates are NOT
  holds: a merge lease REFUSES input, and so does a hibernating
  session's own occupancy lease FOR THE STAND-DOWN WINDOW ONLY — while
  the Hibernate directive, its ack and the shim's exit are still in
  flight, that lease refuses rather than holds. ONCE THE SHIM IS GONE
  THE REFUSAL IS OVER: a prompt to a hibernated workspace implicitly
  revives it (see "Hibernation is daemon POLICY (idle-cutoff sweep +
  implicit revive on prompt)" and SPAWN ON MOUNT above), so a parked
  workspace is never an input dead end. The uninterruptible context cut
  is a classification, not a hold.
- Occupancy leases carry PER-HOLDER REFUSAL POLICY: the merge lease
  projects to error-on-submit; restart-pending and shutdown-drain
  project to holds. A prompt arriving after a merge began is refused
  (never held); prompts already held stay held and the dequeue offer
  resolves their fate.
- The genuine daemon holds are exactly three: shutdown drain, revival
  pending, build refresh (keep-alives are wholly shim-internal — the
  daemon holds nothing for them; corrected 2026-08-29).
- PERMISSION DECLINE IS DENY-AND-CONTINUE (deliberate reversal, ruled
  2026-08-29): declining a permission denies that tool and the agent may
  route around it; the turn does NOT end. Stopping is what Interrupt is
  for.

### Merge (daemon-synthesized)

- Merging is a daemon action the vendor knows nothing about; the daemon
  coalesces everything produced during a merge into one feed bubble.
- The merge bubble is a sub-feed (own FeedId, OpenFeed/WatchFeed — the
  same plumbing as subagent bubbles). Routing is ADDRESS-DRIVEN: the
  feed resolver is merge-agnostic and honors a generic output address
  {target feed, parent row} supplied by lease holders; only the merge
  orchestrator and the FOOTER resolver know "merge" as a concept.
- Two methods keyed by self-repo-or-not; PARKED is recognized purely
  from lease state (no content classifier) and the conversational
  parked flow is the only resume path — no hand-resolution verb exists.
- Merge phase history is not stored state — it is feed content the
  daemon synthesizes on the fly.

### Resolvers and push duties

- Five resolvers — feed, footer, topbar, sidebar, hold tray — each
  converting `conversation.v1` items plus daemon facts into that
  component's `frontend.v1` view; internals deliberately unprescribed.
- The Session Manager (one per live session) is the only consumer of
  shim output: owns every shim watch, routes every frame by type through
  one table to feed resolver, footer resolver, accounting, and the
  turn-lifecycle announcement the prompt queue drains on. Simple reads
  the shim now PUSHES on the session stream (context usage, diagnostics)
  — no pull rpcs remain.
- Views push WHOLE, event-driven, no ticks (the standing conventions (teamlead prompt) and proto comments 3.4); every
  footer panel ships fully resolved on every push because panel
  selection is webview-local (folded-menu convention) — the daemon never
  learns which panel is open. Resolvers dispatch feed and footer pushes
  in parallel per state change.
- Resolver accumulation state may stay unpersisted: resolvers buffer
  piecemeal frames in memory and ship complete snapshots.
- The daemon never issues commands to Emacs (the host command loop is
  deleted); Emacs registers, selects, watches, and reacts.

### Failure classification — where each failure lives

- One failure, one home:
  - Entry-correlated failures are arms of the specific row's own `error`
    (turn-terminal rows carry the vendor API taxonomy; why a response
    died lives on the turn-terminal, not the response bubble).
  - Entry-less residue (machinery/shim/internal/client-local) is
    `frontend.v1` failure.proto's vocabulary — a VOCABULARY file, not a
    component; surfaces embed the evidence and render it their own way.
  - Unmodeled tools are NOT failures and never feed rows — their home is
    the topbar's warning dropdown, one warning per distinct name.
  - Health verdicts: unhealthy is an ANSWER (success arm), never an rpc
    error; DaemonFault and SessionFault are deliberately separate types
    (different producers), arms derived from real fault sites only.
  - The STORE must hold every failure frame (the shim persists what it
    sees on the stream): the vendor transcript PROVABLY lacks several
    failure classes (retried-then-recovered, result-level
    classification, control-channel errors are stream-only) and is
    never the recovery source for them (corrected 2026-08-29).
- Liveness is layered, never keepalive-framed: upstream silence is
  stated by the daemon as a fact (shim-degraded arms); pipe death is the
  transport's; a wedged publisher is the daemon's own watchdog's
  (DaemonHealth), never client frame-timing.

### Rollout / handover

- Blue-green self-rollout: old daemon spawns the rebuilt one (joining
  mode), transfers workspaces one by one at FREENESS (no in-flight turn,
  no live detached work). No daemon↔daemon channel — client relay + WSM
  facts + per-workspace kernel locks.
- Ordering by REFUSAL: the new daemon refuses unowned workspaces
  (`not_yet_adopted`); the old refuses with `transferring_away{address}`;
  lagging clients self-heal. Adoption completes only when every expected
  participant has called its adopt verb (AdoptHostWorkspace /
  AdoptWebWorkspace); headless workspaces transfer with zero rendezvous.
- During handover, intake is HELD and replays in order on the new daemon
  (the merge-in-flight refusal still applies).

### Standing gotchas

- Usage is stamped on EXACTLY ONE unit per API response (the first
  content block's unit); summing units triple-counts. Absent usage means
  "not the carrying unit," never "free." Thinking-token estimates are
  unbilled — never add them to a bill.
- Finality of a response is derived per render from the turn's own
  conclusion, never a positional or wire fact.
- On a nested activity frame, the unit upserted is the INNERMOST id;
  the outer id is the containment path.
- Foreground shell output is structurally unobservable (the spool
  materializes at exit); incremental output exists only for detached
  bash, produced by the sidecar.
- Feed liveness is structural: no FeedTurnEnded row for the current turn
  means the turn is live; a connection dying without one is a transport
  failure.
- GetFeedPage's walk position is DAEMON-held (one webview per
  workspace); `next` with no walk standing is a refusal, not an empty
  page; the daemon picks page sizes.
- Domain outcomes (no matches, denial, unhealthy, "nothing running")
  are SUCCESS answers.
- Money/cost is deliberately absent from the entire API.
- The store is nuked, never migrated — write no migration or backfill.
- Unset non-optional fields are illegal everywhere: error the producer
  on a request; raise loudly at the consumer on a stream push. Every
  logical branch gets a DEBUG log; warnings are remediated to zero.

### Additional rulings (final-audit triage, 2026-08-29)

- LOG SURFACES (the full existing design is ported): per-workspace
  durable log targets with shared-fd write access, a size cap with
  periodic maintenance, eviction on workspace close, and a
  NON-CLOSEABLE BORROW handle (one caller's Close once poisoned the
  inode for every writer and the next spawn inherited a closed fd 3); a
  restart-scoped run log with retained backups and an in-run cap whose
  open-failure is a boot fatal; ClientLog lines land in the owning
  workspace's log; the terminal mirror is DECOUPLED from the durable
  sink and must never block it (a stalled pty reader once added ~7s to
  boot).
- ENV CONTRACTS (all four): `-fake` forces the offline scripted-SDK mode
  onto every session including respawns (the mock plane the projectlead's
  no-real-calls mandate rides); `AGENT_REPL_FORBID_VENDOR_CALLS` hard-
  refuses at every vendor exec site; `AGENT_REPL_STATE_DIR` is a NAMED
  cross-process contract — daemon, Emacs, and the skills must resolve
  the SAME state root, and divergence is a loud misconfig (it silently
  drops messages otherwise); `AGENT_REPL_OWNED` is propagated through
  the shim spawn env so vendor hook scripts recognize our processes.
- PPROF: an opt-in, LOCAL-ONLY profiling surface (unix socket or
  loopback; wildcard refused), opened BEFORE dependency boot so a wedged
  boot is still diagnosable.
- ADD-SUPPORT: an unsupported slash command's refusal card carries the
  "engineer support for it" offer; accepting spawns a support workspace
  with a daemon-composed brief (ordinary creation + initial prompt).
- SKILL CARD (windows are dead): a Skill invocation's bubble is
  populated from exactly the TWO shim messages — the invocation and the
  skill document content; NO temporal window folds subsequent responses
  under it.
- METAPROMPT STRIPPING: the feed resolver strips the host's
  sentinel-marked injected spans when composing the DRAWN prompt row
  (the full text stays on the record) — the client never sniffs
  sentinels.
- ROSTER ORDER: the sidebar resolver orders by priority (the
  SetWorkspacePriority increment's fact); clients — Emacs tabs
  included — follow roster order strictly and never re-sort.
- KNOWN-OPEN — THE /agents AND /help PANELS: their vendor catalogs died
  with the handshake reversion and no producer exists; the shim's
  probe-resolved command list is the named restoration path. Escalate
  to the project lead before implementing; an EMPTY panel is not an
  acceptable quiet outcome.
- /status DEGRADES BY DESIGN: with the handshake deferred, the panel
  resolves version + spliced account/model/mode rows ONLY (cwd, auth,
  plugins, memory return if the handshake deferral ever lands) — a
  viewer seeing the thin panel is seeing the settled consequence, not
  a bug.
- WORKFLOW IS KICKED: workflow watch/store/ingest APIs stay in the
  contract but are NOT implemented in this wave; no frontend surface
  exists for the run, deliberately.

## Kickoff increments and rulings (2026-08-29, project lead)

- LANDED PROTO INCREMENTS: `OpenInEditor{workspace,path,optional line}` (WEB
  LINK) relayed as the host push arm `open_in_editor` — the daemon validates
  and relays verbatim, opens nothing; FeedRow arms `command_panel` and
  `command_refused` — recognized panels and refusals MIRROR into the ROOT
  FEED as synthesized NON-DURABLE rows (resolver memory; never stored, never
  replayed after a restart); SubmitPromptSuccess gains `command_refused`;
  `RequestCommandSupport{workspace,command}` composes the support brief and
  creates the support workspace (ordinary creation underneath);
  SubmitPromptRequest gains the REQUIRED `origin` (persisted onto the turn);
  WatchLoginTerminal is a SERVER stream + unary `SendLoginInput`; TopbarView
  gains `permission_mode_picker` (the daemon serves exactly the switchable
  set); AgentUpdate gains the page-line arms `context_cut` (drawn as the
  separation divider) and `api_error` (mid-turn evidence, never a terminal);
  UpdateHeldPrompt gains `accept` (legal only on hold_for_turn_end);
  shim.v1 failure `kind`/`cause` arms and SessionFault.kind landed from the
  shim's real refusal sites.
- /agents AND /help (user ruling): the daemon RECOGNIZES them, never forwards
  them to the shim, and answers `command_refused` with the add-support offer.
  No catalog increment exists.
- R1: momentary footer statuses are retired by a daemon-side one-shot
  successor push. R3: WatchDaemon serves Emacs AND every webview. R13: the
  held-prompt classifier is the daemon's own headless vendor run, guarded by
  AGENT_REPL_FORBID_VENDOR_CALLS, with a scripted `-fake` mode. R14: no
  daemon fact is ever written to the store.
- The daemon keys every WorkspaceRef on `id` and REFUSES a ref whose `dir`
  disagrees with the registry. A merged, closed or killed workspace's roster
  row carries `closed = true` (Emacs derives its tab set from it); a nuked
  workspace leaves the roster. Emacs launches `daemon/bin/claude-repld` with
  no required argv (state via the env); a successor is spawned with the
  daemon's own joining argument. agent-shim/wire is deleted by the daemon
  rewrite once nothing imports it.

- CROSS-SYSTEM PROCESS CONTRACTS (project lead, kickoff): one state root
  `$AGENT_REPL_STATE_DIR` (default ~/.claude-emacs); the daemon binds ONE
  loopback TCP listener serving Connect (HTTP/1.1 + h2c, binary + JSON) and
  the webapp assets on one origin, writes `127.0.0.1:<port>` to
  `$AGENT_REPL_STATE_DIR/daemon.addr` (atomic replace; removed on orderly
  exit; a joining successor writes it only after it owns every workspace);
  the webview URL is `http://<daemon.addr>/?workspace=<id>&dir=<dir>`
  (`&composer=1` only in dev mode); the shim is spawned as `node
  agent-shim/claude/shim/dist/main.js --listen <uds> --store-socket <uds>
  --log-fd 3 [--fake]` with CLAUDE_CONFIG_DIR, AGENT_REPL_OWNED=1,
  AGENT_REPL_STATE_DIR, SHIM_BUILD_SHA (tests add
  AGENT_REPL_FORBID_VENDOR_CALLS=1), cwd = the workspace; session facts
  travel only in StartSession; readiness = the first healthy `diagnostics`
  push on WatchSession; the store serves on ~/.cache/agent-repl/sock/
  store.sock (tests: env AGENT_REPL_STORE_SOCKET, a flag beats it); kernel
  locks live in ~/.cache/agent-repl/run/ — `workspace-<md5hex(symlink-resolved
  abs dir)[:8]>.lock` (shim-held from StartSession to process death, per the
  2026-09-02 relay ruling below; the daemon probes ONLY this one,
  flock LOCK_EX|LOCK_NB) and `session-<vendor session id>.lock` (taken
  inside StartSession; pre-minted on a fresh start); proto/vocab/
  render-colors.json + paint-classes.json are the daemon's, consumed by
  webapp and Emacs; Go modules pin connectrpc.com/connect v1.17.0 and
  golang.org/x/net v0.43.0 (Go 1.24 on this machine; every module stays
  `go 1.23`).

## Landing 3 relay (2026-08-29, project lead)

- StartTurnSuccess.page is the opening page; page_size/known_through on the request are live.
- UpdateAgent prompt to a subagent refuses `not_deliverable` (SDK limit); DetachForeground refuses `unsupported` when the SDK cannot initiate a detachment. Both surface honestly in the feed; whether the controls are hidden this wave is the user's call (put to the user).
- AgentId minting rule (proto comment on AgentId): main = original vendor session id; subagent = spawning call's tool_use_id. The daemon never derives a spawn unit's identity from an AgentId even though the bytes coincide.
- FILE-PLANE-ONLY facts: AgentContextInjected, the write/edit `diagnostics` consequence arm and SessionUpdate.context_budget_warning arrive only through the store tail (WatchAgent replay/follow), never on the shim's live WatchSession; the daemon must not wait for them on the session stream.
- Left unset by the stream plane (never invented): subagent spawn_depth/working_dir/transcript_suppressed/structured_result, worktree provenance/cleanup, AgentActivity.effort, ApiRateLimited.retry_after_ms from a result, AgentHookStart.gated_call, a foreground bash's termination, an image bash result's media_type.
- Your error-arm batch is landing 4.

## Landing 4 relay (2026-08-29, project lead)

- Every `<Rpc>Error` carries its typed arms; refusal sites switch off the seam constants; ERROR-ARMS.md rows are retired as each lands. Arms prescribed for unlanded packages are landed too; report any that end up unused and I retire them.
- FooterAllowance is sourced from SessionUpdate.rate_limit_status (typed status); the verbatim string is gone.
- context_budget_warning arrives on the agent plane (AgentUpdate); the WatchSession routing goes.
- Re-adopted live work is always `created`-origin (shim ruling); DetachedWorkId == the unit's AgentActivityId, so `created`-origin monitors are retired by their own terminal.

## Implementation overrides recorded by the daemon lead (2026-08-29)

- MERGE RECOVERY: an in-flight merge found at boot (lease row present) is
  re-queued at the FRONT of its repository's queue and re-run from the
  queue tab rather than re-entered at its last recorded tab; the tab
  history already published stays as feed content, the new run appends
  its rounds. Rationale: the git state after a crash is only trustworthy
  from a clean re-run; re-entering mid-tab would guess at partial state.

## Landing 6 relay (2026-09-01, project lead)

- SubmitPromptSuccess.command_acted lands: the act path answers it instead of the notimpl sentinel; retire that ERROR-ARMS row.
- SubmitPromptError.duplicate_submission lands for the idempotency_key repeat; UpdateMergeQueueError.unknown_repository lands for pause/resume on an unknown RepositoryRef.
- SubmitPromptError.turn_already_open is RETIRED (tag 8 reserved); delete promptqueue.ErrTurnAlreadyOpen and the server mapping.
- /status uses the EXISTING SubmitPromptCommandPanel.status arm (tag 1); resolve the thin panel into StatusPanelView rows. AnswerQuestionError's ask_not_standing/unserved_value split already exists; drop the Refusal.NotFound workaround in favor of the two arms.
- Watch* refusals stay transport-closed by ruling; record them as such, not as unlanded arms.

## Ruling relay: shim relaunch vs the workspace lock (2026-09-02, project lead)

- CONFLICT FOUND at integration: the rollout's SHIM RELAUNCH prelaunches an
  inert shim beside the live one, but the kickoff contract had the shim take
  `workspace-<hash>.lock` (flock LOCK_EX) at STARTUP, so the prelaunched
  process blocked in flock and never listened.
- RULING (a): the workspace lock means "this conversation is owned"; an inert
  prelaunched shim owns no conversation and holds nothing, so the SHIM takes
  `workspace-<hash>.lock` inside StartSession, beside the session lock. The
  daemon's pre-spawn probe semantics are unchanged (a held lock still means a
  live shim owns the conversation; the rollout transfer still waits on it).
  The shim lead lands the change.
- RECORDED OVERRIDE (daemon lead, interim): until the shim lead confirms, the
  relaunch engine runs SEQUENTIALLY — at freeness: hold intake
  (restart-pending), stand down and reap the old shim (the reap is the gate),
  then launch the new shim and StartSession(resume), then drain intake. The
  flip back to prelaunch-then-wait is one site in the relaunch engine plus
  the harness fake shim taking the lock at StartSession.
- LANDED (shim, overhaul/shim be119abbf, 2026-09-02): the shim takes no
  lock at startup; both locks are taken inside StartSession and released
  together on kill/stand-down; a workspace conflict answers StartSession
  `conversation_owned`. The interim sequential override above is RETIRED:
  the relaunch engine runs the prescribed prelaunch-then-wait flow, and the
  harness fake shim takes the workspace lock at StartSession. The shim's
  signal handlers now precede its "serving" record, so a supervisor keying
  off "serving" is safe.

## Landing 7 relay (2026-09-02, project lead)

Adapt to protos ab7e681f2 / bindings c10714a41 (see PROTO-CHANGES.md):
- SubmitPromptError.bubble_refused{detail, kind} REPLACES server.UnlandedArm
  for both bubble refusals; server.bubbleRefused maps
  UpdateAgentFailure.not_deliverable → kind.not_deliverable and
  UpdateAgentFailure.agent_busy → kind.agent_busy. Delete the ERROR-ARMS
  unlanded rows; the by-design red bubble test goes green.
- StartSessionFresh.model optional: drop the DefaultModel fallback in
  workspace/sessions.go; leave model UNSET when the user chose none and read
  SessionStarted.effective_model.
- WatchSessionResponse is a oneof: handle frame.session_started on every
  watch open (adoption = pure attach; the facts come from the shim, not the
  durable record); ignore a repeat on a watch that already has them. The
  handover rendezvous test's last assertion goes green.
- CloseWorkspaceBlocked: fill the five fields from the quiet check; the
  footer's activity line and `summary` are the same composed sentence.
- FeedMergeAbandoned.summary: compose from the abandon cause (user drop,
  workspace closed, daemon shutdown). LANDED: `merge.AbandonCause` declares
  each cause beside its one resolved sentence; the producers are Evict, the
  dequeue release, `OnWorkspaceClosed` (Kill and Nuke) and `recoverWaiting`
  (a merge the restart cannot re-queue).

## Landing 8 relay (2026-09-02, project lead; user-approved)

Adapt to protos 1fdf85e63 / bindings 3791cd630 (PROTO-CHANGES.md "Landing 8"):
- ContextCut.compaction_failed → FeedSessionSeparation{kind: compaction_failed
  {error}, label composed ("compaction failed"), tokens UNSET}; the footer's
  compacting sub-status ends. Previously nothing was drawn.
- AgentFailure.max_turns / budget_exhausted / execution_error /
  structured_output_retry_exhausted / stop_hook_prevented → FeedTurnEnded
  .errored with the matching new arm (max_turns, max_budget, execution_error,
  turn_failed{stop_reason}, stop_hook_prevented), VendorFailureContext filled,
  headline composed per arm ("stopped at the turn limit", "stopped at the
  budget", "the run broke while executing", "the run ended: <stop_reason>",
  "a Stop hook ended the run"), message = the vendor's wording when recorded.
  The workspace resolves PURPLE per failure.proto for the four vendor arms.

## Landing 11 relay (2026-09-04, project lead; user-approved)

- FeedShellLost.how / FeedSubagentLost.how: resolve/feed/seams.go's
  `applyShellLostHow` / `applySubagentLostHow` relay the DetachedLost arm BY
  NAME onto the feed's lost rows, off the same `detachedLostCause` the lost
  detection already reads. Call sites: `shellSettled` (from
  AgentBashInterrupted.cause.lost) and `subagentFailureOutcome` (from
  AgentSubagentFailure.cause.lost), both in resolve/feed/subagent.go.
- AN ARM THIS BUILD DOES NOT CARRY IS NEVER DEFAULTED: the mappers report it
  instead, and the caller states it once through the workspace logger
  (`daemon.feed.shell_lost_unlanded_arm`, `daemon.feed.subagent_lost_unlanded_arm`)
  and sends the row with `how` unset rather than with a wrong arm.
- A wire `lost` whose own `how` is UNSET stays as it was: `lostCauseOf` reads
  no arm, so the row draws cancelled/failed and makes no lost claim. The feed
  never states an arm the producer did not name.

## Landing 10 relay (2026-09-04, project lead; user-approved)

- FeedAgentPrompt.delivery: resolve/feed/sendmessage.go sets queued_to_live /
  resumed_recipient on the sender's row from AgentSendMessageSuccess's arm.
  (Landing 14 adds the third arm: the same composer's failure case sets
  `refused`, ALWAYS — a contentless refusal is still a refusal — with the
  producer's own words read by failureText as its reason.)
- FeedPermissionAnswered.denied_undecidable{text}: resolve/feed/permission.go
  `decisionArm` draws the shim's AgentPermissionDenied.undecidable as its own
  arm (today folded onto denied_by_policy).
- FeedTurnErrorQueryDied.cause: resolve/feed/turnended.go carries
  SessionQueryDied's arm through.
- SetModelError.cold: workspace/sender.go relays SetSessionModelFailure.cold
  as the typed arm; delete the ERROR-ARMS `SetModel | cold` row.

## Landing 9 relay (2026-09-03, project lead; user-approved)

- OpenWorkspaceError.vendor_start_failed{detail}: replace the unlanded-arm
  relay in workspace/sessions.go with the typed arm; delete the ERROR-ARMS
  row. Also RULED (proto wins): an interrupted Bash run draws
  FeedToolCallReturned verdict `succeeded` + interrupted text, not `failed`
  (resolve/feed/toolcall.go ~588-611); update the tests that pinned `failed`.
