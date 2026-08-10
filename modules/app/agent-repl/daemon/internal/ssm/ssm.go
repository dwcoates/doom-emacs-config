package ssm

import (
	"database/sql"
	"fmt"
	"sort"
	"sync"
	"sync/atomic"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"claude-repld/internal/dlog"
	"claude-repld/internal/workspace/merge"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"google.golang.org/protobuf/proto"
)

// Resolver maps ANY identity a session is known by to the one binding that
// session has. The binding lives in the daemon's session registry (the stitch
// phase wires the real one); the SSM takes it injected so it stays free of
// registry knowledge.
type Resolver interface {
	// Session returns the binding for sessionID, and whether one exists.
	// sessionID may be either the daemon-minted s_<hex> id or the vendor
	// session uuid the store files events under.
	Session(sessionID string) (Binding, bool)
}

// Binding is one session's resolved identity: the workspace it drives and the
// DAEMON-MINTED id that names it everywhere the SSM records or compares
// ownership.
//
// THE WORKSPACE AND THE CANONICAL ID COME FROM ONE LOOKUP, deliberately. They
// are two fields of one registry record, and resolving them separately is what
// allowed the SSM to file a row under an identity no claimant check would ever
// match (see normalizeSessionIdentity). A resolver that can answer one can
// always answer the other, so the pair is returned together and the mismatch is
// unrepresentable.
type Binding struct {
	// Workspace is the session's CWD. A binding is never reported for a
	// session with no workspace.
	Workspace string
	// SessionID is the daemon-minted s_<hex> id, whatever identity was asked
	// about.
	SessionID string
}

// Options configure a Manager.
type Options struct {
	// DBPath is the state store's path. Empty uses defaultDBPath
	// (~/.cache/agent-repl/ssm/state.db). Ignored when DB is set.
	DBPath string
	// DB is an already-open state store, shared with the store's other
	// owner (the session registry, whose identity tables live in the same
	// database so a cursor and an identity move in ONE transaction). When
	// set, the Manager runs its migrations on the handle but does NOT own
	// it: Close leaves it open for whoever did.
	DB *sql.DB
	// Logf is the loud transition/anomaly logger. Required.
	Logf dlog.Logf
	// Warnf is the WARN channel, for a record that accompanies a regression
	// the user can see: a lease that stays open and keeps refusing prompts, a
	// turn claim that outlives its turn, a stopped turn that will not be
	// resumed. At info those are indistinguishable from the routine
	// transitions this manager logs constantly.
	//
	// Nil falls back to Logf, so the record is still made and only its
	// severity is lost.
	Warnf dlog.Logf
	// Errorf is the ERROR channel, for a failure that leaves durable state
	// wrong rather than merely degraded. Nil falls back to Warnf.
	Errorf dlog.Logf
	// Resolver binds session ids to workspaces. Required for Apply.
	Resolver Resolver
	// Clock returns wall-clock unix millis; injectable for tests. Nil uses
	// time.Now. The Manager enforces per-instance monotonicity on top of it
	// so the (workspace, at) primary key never collides.
	Clock func() int64
	// ClearingTimeout bounds how long the clearing axis may stand open
	// without the ContextCleared that closes it. Zero uses
	// defaultClearingTimeout. See Manager.ApplyClearing.
	ClearingTimeout time.Duration
	// AfterFunc arms the clearing watchdog. Nil uses time.AfterFunc; a test
	// injects one that captures the callback so the expiry fires
	// deterministically instead of being waited on.
	AfterFunc func(time.Duration, func()) Timer
}

// Timer is the slice of *time.Timer the clearing watchdog needs, so
// Options.AfterFunc can be faked.
type Timer interface {
	// Stop cancels a pending fire, reporting whether it had not yet fired.
	Stop() bool
}

// subBufferSize bounds each subscriber's channel. A subscriber that falls
// this far behind is a slow consumer: the push is dropped (loudly), not
// blocked on, and the consumer recovers via a fresh Snapshot.
const subBufferSize = 64

// Manager is the session-state manager. All mutations funnel through mu, so
// the SELECT-then-INSERT idempotency check and the monotonic clock are
// race-free with a single writer connection.
type Manager struct {
	db *sql.DB
	// ownsDB is false when the state store was handed in already open (see
	// Options.DB): closing another owner's handle would take the session
	// registry's tables down with the SSM.
	ownsDB bool
	logf   dlog.Logf
	// warnf and errorf are the leveled channels described on Options. Never
	// nil after Open; reached through warn and logError.
	warnf    dlog.Logf
	errorf   dlog.Logf
	resolver Resolver
	clock    func() int64

	// emitMu admits ONE drainer of pendingPublications at a time. It is taken
	// with mu RELEASED and is the only lock held while a caller's synchronous
	// publisher runs, which is what keeps this manager out of the frontend's
	// lock order — see stagePublishLocked.
	emitMu sync.Mutex

	// flightMu guards snapshotFlight ONLY. It is deliberately not mu: the whole
	// point of the flight is to coalesce callers while mu is free, so a joiner
	// that had to take mu to discover the flight would queue behind exactly the
	// work it is trying not to duplicate.
	flightMu sync.Mutex
	// snapshotFlight is the resolve currently in progress, or nil. Every caller
	// that arrives while one is in flight shares its result instead of running
	// its own — see Snapshot.
	snapshotFlight *snapshotFlight
	// snapshotResolves counts full-fleet resolves actually executed (not calls
	// to Snapshot). It is what makes coalescing observable to a test.
	snapshotResolves atomic.Uint64
	// snapshotGate and snapshotJoined are TEST SEAMS, nil in production.
	// snapshotGate runs at the end of a leader's lock-free resolve, so a test
	// can hold a snapshot open and prove the mutating entry points still run;
	// snapshotJoined runs when a caller coalesces onto an in-flight resolve, so
	// a test can release the gate only once every joiner has actually joined.
	snapshotGate   func()
	snapshotJoined func()

	mu sync.Mutex
	// pendingPublications is the ordered outbox of synchronous frontend
	// publications staged under mu and emitted after it is released. See
	// stagePublishLocked for why the barrier moved off the lock hold.
	pendingPublications []func()
	lastAt              int64
	last                map[string]frontendv1.RenderState // last-resolved state per workspace
	// lastTasks is the last-pushed live_task_count per workspace. The count is
	// an INPUT the frontend renders (the footer's live-task figure, sourced via
	// progress.ApplyWorkspaceState), so it can move while the render state does
	// not — 5 background tasks becoming 2 leaves a workspace idle_async either
	// way. Keying the push on state alone left that change unpushed, which is
	// how a swept ghost count stayed on screen.
	lastTasks map[string]int64
	// lastMergeStatus is the last-pushed merge_status per workspace. It is the
	// THIRD push key beside the render state and the task count, and it exists
	// because the merge pipeline reports progress the render state cannot see:
	// cherry_picking → testing → cherry_picking all resolve to the same
	// `merging` state, and both `merged` publications (the driver's, then the
	// coordinator's carrying the after-action's outcome) resolve to the same
	// `merged` one. Keying the push on state alone collapsed every one of those
	// into a single frame, so a frontend was handed the run's FIRST word on each
	// phase and never any of the progress that followed it.
	lastMergeStatus map[string]*frontendv1.MergeStatus
	// pushedAtMs is the per-workspace freshness watermark: the newest AtMs any
	// outgoing WorkspaceState carried. See stampFreshnessLocked — the frontend
	// refuses snapshots older than what it delivered, so AtMs must never
	// regress within a process.
	pushedAtMs map[string]int64
	// stampedComposite is the last frame stamped per workspace, retained so
	// stampFreshnessLocked can tell "the same state re-resolved" (which keeps
	// its revision) from "a different state that resolved off an older row"
	// (which earns watermark+1). Without the distinction the watermark clamp
	// handed two different composites one revision, which the webapp reports as
	// a `revision conflicted` invariant violation.
	stampedComposite map[string]*frontendv1.WorkspaceState
	// publishEpoch counts the frames stamped per workspace. It is the ONE fact
	// a lock-free Snapshot needs from the publication path: a workspace whose
	// epoch moved while the snapshot was resolving had a frame go out from
	// under it, so the resolved content may predate what the client already
	// holds, and that workspace is re-resolved under mu before it is stamped.
	// See Snapshot for the consistency this buys and the window it leaves.
	publishEpoch map[string]uint64
	// pipelineStatus is the newest MergeStatus the merge PIPELINE published per
	// workspace. It is what the construction funnel stamps when the run is this
	// process's; see mergestatus.go for why it is in memory and why the log
	// projection only ever answers for a previous daemon's rows.
	pipelineStatus map[string]*frontendv1.MergeStatus
	// mergeDequeueOffers is the outstanding dequeue QUESTION per workspace —
	// the one an interrupt raises instead of silently taking the workspace's
	// merge off the queue. See mergedequeue.go for why it is in memory and why
	// at most one may stand per workspace.
	mergeDequeueOffers map[string]*frontendv1.MergeDequeueOffer
	// interruptedTurn names the workspaces whose IN-FLIGHT turn was stopped by
	// a user-commanded interrupt the shim acknowledged as INTERRUPTED. The
	// mark is consumed by that turn's own TurnEnded, which then reports
	// `interrupted` instead of `done`/`vendor_blocked`.
	//
	// DELIBERATELY IN MEMORY, and deliberately not a row. It is not a state —
	// it is a note about the single event that has not arrived yet, and it
	// lives for the few hundred milliseconds between the shim's ack and the
	// turn end it caused. Persisting it would invent a durable fact the log
	// has no clearing token for; losing it to a restart in that window costs
	// one turn painting `done`, which is the honest fallback.
	interruptedTurn map[string]*interruptMark
	// clearingTimers holds each workspace's live clearing watchdog. The AXIS
	// is a durable row; only the watchdog is in memory, and Open re-arms it
	// from the log so a restart mid-clear cannot leave one unexpirable.
	clearingTimers  map[string]Timer
	clearingTimeout time.Duration
	afterFunc       func(time.Duration, func()) Timer
	// mergeLeases is the in-memory projection of the durable merge_lease
	// ledger, keyed by workspace and ordered oldest window first. See
	// mergelease.go: the projection is what lets Held and the provenance
	// lookup be total, and the TABLE remains the authority both are warmed
	// from.
	mergeLeases map[string][]leaseWindow
	// mergeLeaseHolders marks the workspaces whose open lease window is held
	// by a live merge IN THIS PROCESS. The durable table cannot carry this
	// distinction: after a daemon bounce an open window's holder is provably
	// gone, and openMergeLease uses the absence of an in-process holder to
	// ADOPT the orphaned window instead of wedging every later merge on the
	// unique open-window index. See mergelease.go.
	mergeLeaseHolders map[string]bool
	// mergedAt is the in-memory projection of the durable workspace_merged
	// table: the instant each workspace's merge landed. See merged.go — the
	// projection is what lets the WorkspaceState construction funnel stamp
	// merged_at_ms without an error channel, and the TABLE remains the
	// authority it is warmed from.
	mergedAt map[string]int64
	// mergedTeardown stands a merged workspace's session down, bound by
	// ssm.NewMergeLease. A daemon assembled without the merge subsystem has
	// none, and a merged workspace's session then simply keeps running (loudly
	// noted at the transition).
	mergedTeardown MergedTeardown
	// mergeQueue is the merge subsystem's queue, bound by ssm.NewMergeLease.
	// Its presence is what makes a held merge lease well-formed: only
	// NewMergeLease can produce a lease and it binds the queue, so a lease with
	// no queue behind it is a wiring bug the push path reports
	// (mergeWiringCheckLocked). It no longer feeds any wire field — the flat
	// merge_queue_position / merge_queue_depth pair is retired, and MergeStatus's
	// enqueued arm reports the place the run was ADMITTED at instead.
	mergeQueue merge.Queue
	// hibernationLeases makes settledness and turn admission one SSM-owned
	// decision. A workspace entry excludes prompt acceptance and TurnStarted.
	hibernationLeases    map[string]uint64
	nextHibernationLease uint64
	// controllerRegistrations counts controller generations whose bring-up has
	// begun but has not reached its operational edge. Hibernation and controller
	// registration are mutually exclusive under mu, so no settled projection
	// from an older generation can authorize stopping the generation being
	// published.
	controllerRegistrations map[string]map[string]struct{}
	subs                    map[int]chan *frontendv1.WorkspaceState
	nextSub                 int
	closed                  bool
}

// Open opens the SSM database and warms the last-resolved cache from the
// persisted log so a reopen does not re-announce every workspace as a fresh
// transition (state survives the reopen).
// warn emits through the Manager's WARN channel (Options.Warnf, or Logf when
// that is unwired). It is the sole reader of warnf.
func (m *Manager) warn(format string, args ...any) { m.warnf(format, args...) }

// logError emits through the Manager's ERROR channel (Options.Errorf, falling
// back to Warnf and then Logf). It is the sole reader of errorf.
func (m *Manager) logError(format string, args ...any) { m.errorf(format, args...) }

func Open(opts Options) (*Manager, error) {
	path := opts.DBPath
	if path == "" {
		p, err := defaultDBPath()
		if err != nil {
			return nil, err
		}
		path = p
	}
	if opts.Logf == nil {
		return nil, fmt.Errorf("ssm: Options.Logf is required")
	}
	logf := opts.Logf
	warnf := opts.Warnf
	if warnf == nil {
		warnf = logf
	}
	errorf := opts.Errorf
	if errorf == nil {
		errorf = warnf
	}
	clock := opts.Clock
	if clock == nil {
		clock = func() int64 { return time.Now().UnixMilli() }
	}
	db, ownsDB := opts.DB, false
	if db == nil {
		opened, err := openDB(path, logf)
		if err != nil {
			return nil, err
		}
		db, ownsDB = opened, true
	} else if err := migrate(db, logf); err != nil {
		return nil, err
	}
	clearingTimeout := opts.ClearingTimeout
	if clearingTimeout <= 0 {
		clearingTimeout = defaultClearingTimeout
	}
	afterFunc := opts.AfterFunc
	if afterFunc == nil {
		afterFunc = func(d time.Duration, f func()) Timer { return time.AfterFunc(d, f) }
	}
	m := &Manager{
		db:                      db,
		ownsDB:                  ownsDB,
		logf:                    logf,
		warnf:                   warnf,
		errorf:                  errorf,
		resolver:                opts.Resolver,
		clock:                   clock,
		last:                    make(map[string]frontendv1.RenderState),
		lastTasks:               make(map[string]int64),
		lastMergeStatus:         make(map[string]*frontendv1.MergeStatus),
		mergeDequeueOffers:      make(map[string]*frontendv1.MergeDequeueOffer),
		pushedAtMs:              make(map[string]int64),
		interruptedTurn:         make(map[string]*interruptMark),
		mergeLeases:             make(map[string][]leaseWindow),
		mergeLeaseHolders:       make(map[string]bool),
		mergedAt:                make(map[string]int64),
		hibernationLeases:       make(map[string]uint64),
		controllerRegistrations: make(map[string]map[string]struct{}),
		clearingTimers:          make(map[string]Timer),
		clearingTimeout:         clearingTimeout,
		afterFunc:               afterFunc,
		subs:                    make(map[int]chan *frontendv1.WorkspaceState),
	}
	if err := m.warm(); err != nil {
		if ownsDB {
			db.Close()
		}
		return nil, err
	}
	return m, nil
}

// warm resolves every known workspace once at Open to seed the
// last-resolved cache WITHOUT logging transitions or pushing (a restore is
// not a transition). It also advances the monotonic clock past the newest
// persisted `at`.
func (m *Manager) warm() error {
	m.mu.Lock()
	defer m.mu.Unlock()
	var maxAt sql.NullInt64
	if err := m.db.QueryRow(`
		SELECT MAX(at) FROM (
			SELECT at FROM workspace_state
			UNION ALL
			SELECT at FROM session_connectivity
			UNION ALL
			SELECT at FROM session_fault
		)
	`).Scan(&maxAt); err != nil {
		return fmt.Errorf("ssm: warm read max(at): %w", err)
	}
	if maxAt.Valid {
		m.lastAt = maxAt.Int64
	}
	if err := m.repairPersistedOrphanTaskEndsLocked(); err != nil {
		return err
	}
	// Before any workspace resolves, so the first restored WorkspaceState
	// already carries merge_lease_held rather than a frame that says the
	// workspace is open to prompts it will then refuse.
	if err := m.warmMergeLeasesLocked(); err != nil {
		return err
	}
	// Before any workspace resolves too, for the same reason: a merged
	// workspace's very first restored WorkspaceState must already carry
	// merged_at_ms, or a reconnecting sidebar would drop it out of its
	// recently-merged ordering until something happened to that workspace
	// again — and nothing ever happens to a merged workspace again.
	if err := m.warmMergedAtLocked(); err != nil {
		return err
	}
	// Before the cache is seeded, so a released row is what gets restored
	// rather than the stale one it replaces.
	if err := m.releasePersistedPermissionsLocked(); err != nil {
		return err
	}
	// Same placement and the same reason: a workspace still latched behind a
	// compact-first revival's turn — whose compaction the log shows completed —
	// must be healed before it resolves, or the restored WorkspaceState carries
	// a turn band nothing is running and the restart guard refuses the workspace
	// all over again. Open is the one moment this is honest: nothing is wired to
	// a daemon that has just started (see hibernateEveryWorkspaceLocked below),
	// so no claim here can belong to a turn that is genuinely live.
	if err := m.reconcileCompletedCompactionClaimsLocked(); err != nil {
		return err
	}
	// NOTHING IS WIRED TO A DAEMON THAT HAS JUST STARTED. The session-status lifecycle
	// history survives the restart and the shim connections do not, so every
	// restored workspace is hibernated until something wires it again — which is
	// exactly what the connection-truth law says a tab with no live session
	// must claim. Same placement, and the same reason, as the permission
	// release above.
	if err := m.hibernateEveryWorkspaceLocked(); err != nil {
		return err
	}
	if err := m.hibernatePersistedConnectivityLocked(); err != nil {
		return err
	}
	if err := m.seedMissingConnectivityLocked(); err != nil {
		return err
	}
	names, err := distinctWorkspaces(m.db)
	if err != nil {
		return err
	}
	restored := 0
	for _, ws := range names {
		r, err := resolve(m.db, ws, m.logf)
		if err != nil {
			return err
		}
		composite, compositeFound, err := resolveComposite(m.db, ws)
		if err != nil {
			return err
		}
		if r.found || (compositeFound && composite.LifecycleTop != "") {
			if !r.found {
				r = resolved{found: true, state: frontendv1.RenderState_RENDER_STATE_UNSPECIFIED}
			}
			msg, err := m.workspaceMessageLocked(ws, r)
			if err != nil {
				return err
			}
			m.last[ws] = msg.GetState()
			m.lastTasks[ws] = msg.GetLiveTaskCount()
			m.lastMergeStatus[ws] = msg.GetMergeStatus()
			restored++
		}
	}
	if restored > 0 {
		m.logf("ssm: restored %d workspace(s) from %s", restored, "state log")
	}
	return m.rearmClearingWatchdogsLocked()
}

// nextAt returns a strictly-increasing per-instance timestamp in millis, so
// two events in the same millisecond still get distinct (workspace, at)
// primary keys.
func (m *Manager) nextAt() int64 {
	now := m.clock()
	if now <= m.lastAt {
		now = m.lastAt + 1
	}
	m.lastAt = now
	return now
}

// Apply ingests a lifecycle event forwarded from a shim stream. Supported
// payloads: session started/ended, turn started/ended (session-status lifecycle), task
// started/ended (live-task counting), degraded state (extension). Applying
// the same event twice (same session+seq) is a no-op. Ephemeral and
// unmodeled payloads are ignored but loud-logged — never silently dropped.
func (m *Manager) Apply(ev *corev1.Event) error {
	if ev == nil {
		return fmt.Errorf("ssm: Apply got a nil event")
	}
	sid := ev.GetSessionId()
	if sid == "" {
		return fmt.Errorf("ssm: Apply got an event with no session_id (seq %d)", ev.GetSeq())
	}
	switch ev.GetPayload().(type) {
	case *corev1.Event_TurnStarted, *corev1.Event_TurnEnded:
		// THE ENFORCEMENT RUNG, and the reason turn liveness has exactly one
		// answer. Apply is the general door every lifecycle event comes
		// through, and it used to fold turn boundaries into the session-status
		// axis on its OWN idempotency rule — (event_session_id, cause_seq) —
		// while the durable turn ledger folded the very same events on a
		// different one. Two folds, two answers: a replay under a replacement
		// session was fresh work to the ledger and a duplicate to the axis, so
		// the sidebar painted green over a turn the prompt queue was holding
		// every prompt behind.
		//
		// A boundary now has ONE destination, ApplyTurnBoundary, which moves the
		// ledger and paints the axis from the same derivation in one
		// transaction. Reaching turn liveness by folding the event stream here
		// is not merely discouraged; it is refused, so a future consumer cannot
		// re-create the second authority by accident.
		//
		// A VIOLATED INVARIANT, NOT A ROUTING PREFERENCE. Nothing is written and
		// nothing is degraded: the caller gets the error and the log carries the
		// structured account of what tried to fold what.
		err := fmt.Errorf("ssm: turn boundaries must use ApplyTurnBoundary, never Apply; turn liveness has exactly one derivation (session=%s seq=%d kind=%s turn_id=%q)",
			sid, ev.GetSeq(), payloadKind(ev), turnCorrelation(ev))
		m.logf("ssm: INVARIANT VIOLATION operation=apply event_session=%s seq=%d kind=%s plane=%s turn_id=%q request_id=%q dedup_key=%q — a turn boundary reached the general lifecycle apply, which would fold turn liveness a second time on its own idempotency rule; refused with no row written: %v",
			sid, ev.GetSeq(), payloadKind(ev), ev.GetPlane().String(),
			turnCorrelation(ev), ev.GetRequestId(), ev.GetDedupKey(), err)
		return err
	case *corev1.Event_TurnClaimBridge:
		return fmt.Errorf("ssm: TurnClaimBridge must use the durable turn-claim ledger, never Apply (session=%s seq=%d turn_id=%q)",
			sid, ev.GetSeq(), ev.GetTurnClaimBridge().GetTurnId())
	}

	state, causeKind, ok := agentOrTaskSignal(ev)
	if !ok {
		// Legitimately not an SSM-relevant lifecycle event (ephemeral delta,
		// vendor payload, unparsed, progress). Loud so it is never invisible.
		m.logf("ssm: ignoring non-lifecycle event kind=%s session=%s seq=%d", payloadKind(ev), sid, ev.GetSeq())
		return nil
	}

	if m.resolver == nil {
		return fmt.Errorf("ssm: no resolver injected; cannot bind session %s to a workspace", sid)
	}
	binding, bound := m.resolver.Session(sid)
	if !bound {
		return fmt.Errorf("ssm: no workspace bound to session %s (kind=%s seq=%d)", sid, causeKind, ev.GetSeq())
	}
	ws := binding.Workspace
	// THE ROW IS OWNED BY THE DAEMON SESSION, NOT BY THE EVENT'S IDENTITY. The
	// store files events under the VENDOR uuid, while every party that closes,
	// invalidates or re-claims a turn — CloseStaleTurn, MarkPromptAccepted, the
	// durable turn ledger — names the session by its daemon id. Writing the
	// vendor uuid here put two names for one session on one axis, and every
	// claimant comparison then read them as two sessions: the turn's own
	// teardown declined to close its `thinking` and the session's own next
	// prompt was refused as another session's. Canonicalizing at the write
	// boundary is what makes that unrepresentable. The event's own identity is
	// still recorded, in event_session_id, because idempotency is a fact about
	// the store's seq space rather than about the session.
	owner := binding.SessionID
	if owner == "" {
		return fmt.Errorf("ssm: session %s resolved to workspace %q with no daemon session id (kind=%s seq=%d); a status row with no owner can never be claimed or closed", sid, ws, causeKind, ev.GetSeq())
	}
	if owner != sid {
		m.logf("ssm: event identity canonicalized ws=%s event_session=%s owner_session=%s kind=%s seq=%d — the store files this conversation under its vendor uuid; the row is owned by the daemon session so claim checks compare one identity",
			ws, sid, owner, causeKind, ev.GetSeq())
	}

	m.mu.Lock()
	defer m.mu.Unlock()

	// Idempotency: a replayed event (same store identity + seq) makes no
	// change.
	applied, err := seqApplied(m.db, sid, ev.GetSeq())
	if err != nil {
		return err
	}
	if applied {
		m.logf("ssm: duplicate event skipped kind=%s session=%s seq=%d ws=%s", causeKind, sid, ev.GetSeq(), ws)
		return nil
	}
	if _, started := ev.GetPayload().(*corev1.Event_TurnStarted); started {
		if err := m.rejectStartDuringHibernationLocked(ws, "TurnStarted apply"); err != nil {
			return err
		}
	}

	// THE INTERRUPTED TURN OUTCOME (I1). A user-commanded stop the shim
	// acknowledged as INTERRUPTED marked the turn it stopped; THIS is where
	// that mark is spent, on the very TurnEnded the stop produced.
	//
	// It is applied by rewriting the row this turn end would have written
	// anyway — never by adding a second one — which is what keeps
	// `interrupted` the same kind of fact as `done` and `vendor_blocked`: one
	// session-status lifecycle row naming how the turn ended, superseded by whatever the
	// agent does next.
	state, causeKind = m.applyInterruptMarkLocked(ws, ev, state, causeKind)

	// NO-REGRESS GUARD. A readiness assertion may never knock an ACTIVE turn
	// back to a settled state.
	//
	// SessionStarted is now emitted at the SHIM's own readiness rather than
	// off the vendor's first `system:init`, which means it can legitimately
	// arrive while a turn is already running — a shim relaunch, a revive, a
	// re-handshake. The session-status lifecycle resolves on its LATEST row, so appending
	// `ready` underneath a live `thinking` would resolve the workspace green
	// while the agent was mid-turn. That regression was observed directly
	// (a THINKING→IDLE flip at 01:24:20 in the readiness logs).
	//
	// Readiness is a floor, not a transition: it says "the route works",
	// which a running turn already proves more strongly. So the row is
	// dropped rather than appended, and the running turn stands. The turn's
	// own TurnEnded still moves the workspace off thinking normally.
	//
	// THE CLAIM BELONGS TO A SESSION, and only that session can keep it. A
	// `thinking` row is a promise that the session which wrote it will report
	// the turn's end; a DIFFERENT session now driving the workspace means that
	// promise can never be kept, so the row is a dead claim rather than a
	// stronger one. Falling through appends the readiness row, which supersedes
	// it — the invalidation IS the write, not a separate deletion.
	if state == sigReady {
		active, claimant, err := turnClaim(m.db, ws)
		if err != nil {
			return err
		}
		switch {
		case active && (claimant == "" || claimant == owner):
			m.logf("ssm: readiness suppressed (turn in flight) ws=%s session=%s seq=%d — the running turn is the stronger claim",
				ws, owner, ev.GetSeq())
			return nil
		case active:
			m.logf("ssm: turn claim INVALIDATED ws=%s stale_session=%s new_session=%s seq=%d — the session holding `thinking` no longer drives this workspace and can never report that turn's end, so readiness supersedes it rather than being suppressed by it",
				ws, claimant, owner, ev.GetSeq())
		}
	}

	causeSeq := sql.NullInt64{Int64: int64(ev.GetSeq()), Valid: true}
	if state == sigTaskEnded {
		if err := m.appendTaskEndLocked(ws, owner, sid, causeKind, causeSeq, taskIDOf(ev)); err != nil {
			return err
		}
	} else {
		at := m.nextAt()
		if err := appendEventRow(m.db, ws, owner, sid, state, causeKind, causeSeq, at, taskIDOf(ev)); err != nil {
			return err
		}
	}

	// A COMPACTION CANNOT OUTLIVE ITS TURN. The vendor opens the window with a
	// status ticker and is not obliged to close it — a turn that dies mid-fold
	// simply stops reporting — so the turn's own end is a hard bound on the
	// window. Without it the phase word would stand over a settled session-status lifecycle
	// with nothing arriving that could ever release it.
	if _, ended := ev.GetPayload().(*corev1.Event_TurnEnded); ended {
		m.closeCompactingLocked(ws, causeTurnEnded)
	}

	return m.reresolveLocked(ws, causeKind, ev.GetSeq())
}

func turnCorrelation(ev *corev1.Event) string {
	if started := ev.GetTurnStarted(); started != nil {
		return started.GetTurnId()
	}
	if ended := ev.GetTurnEnded(); ended != nil {
		return ended.GetTurnId()
	}
	if bridge := ev.GetTurnClaimBridge(); bridge != nil {
		return bridge.GetTurnId()
	}
	return ""
}

// MarkTurnInterrupted records that a user-commanded stop was DELIVERED to the
// workspace's running turn (the shim acked INTERRUPTED), so that turn's own
// TurnEnded reports `interrupted` rather than `done` or `vendor_blocked`.
//
// ONLY the frontend interrupt command path calls this. The queue's interject
// sends the same Interrupt to the same shim as pure machinery — the user did
// not ask for the turn to stop, they asked for a prompt to run sooner — so it
// must never paint the outcome, and it reaches this method from nowhere.
//
// The mark is spent by the next turn end and dropped by the next turn start
// (see applyInterruptMarkLocked); it never accumulates.
func (m *Manager) MarkTurnInterrupted(workspace string) error {
	if workspace == "" {
		return fmt.Errorf("ssm: MarkTurnInterrupted got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	// The shim acked INTERRUPTED, so a turn WAS live shim-side. If this log
	// has not yet applied that turn's own TurnStarted (it is still in flight
	// through the store), the start will arrive AFTER the mark — and dropping
	// the mark on it would hand the stopped turn's end to vendor_blocked. So
	// the mark tolerates exactly one late start in that case; a start arriving
	// once the tolerance is spent is a genuinely NEW turn and drops the mark
	// as a stale one.
	active, err := turnActive(m.db, workspace)
	if err != nil {
		return fmt.Errorf("ssm: MarkTurnInterrupted read turn state for %q: %w", workspace, err)
	}
	m.interruptedTurn[workspace] = &interruptMark{tolerateLateStart: !active, at: m.nextAt()}
	m.logf("ssm: interrupt marked ws=%s tolerate_late_start=%t — the turn this stop ended will report `interrupted`", workspace, !active)
	return nil
}

// interruptMark is one workspace's pending interrupt outcome (see
// interruptedTurn).
type interruptMark struct {
	// tolerateLateStart is set when the stop was marked BEFORE the stopped
	// turn's own TurnStarted came back through the store: that one late start
	// belongs to the marked turn and must not drop the mark.
	tolerateLateStart bool
	// at is when the shim acked the stop, on the same clock the state log's
	// rows carry.
	//
	// IT IS THE ONE PIECE OF EVIDENCE THAT AN INTERRUPT WENT UNANSWERED. The
	// mark is spent by the stopped turn's own end and dropped by the next turn's
	// start, so a mark that is STILL STANDING is a stop the shim acked as
	// INTERRUPTED for which no boundary of any kind has since arrived. How long
	// it has been standing is what turns that from a moment's latency into a
	// contradiction the reconciliation may act on
	// (sessioncontroller.reconcileOrphanedTurn).
	at int64
}

// UnansweredInterruptAgeMs reports how long the workspace's standing interrupt
// mark has gone unanswered, and whether one stands at all.
//
// A STANDING MARK IS A SHIM CONTRADICTING ITSELF. The shim answered a stop with
// INTERRUPTED, which is a claim that a turn WAS live and has now been aborted;
// the mark is then spent by that turn's own end and dropped by any later start.
// So a mark still standing long afterwards says the shim acked a turn it then
// never ended and never replaced — the exact shape of a phantom live turn, and
// the only evidence available for one without asking the shim to stop something
// again.
//
// It reports the AGE rather than a verdict, because how long is long enough is a
// policy question and this package holds no policy.
func (m *Manager) UnansweredInterruptAgeMs(workspace string) (int64, bool) {
	if workspace == "" {
		return 0, false
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	mark := m.interruptedTurn[workspace]
	if mark == nil || mark.at <= 0 {
		return 0, false
	}
	age := m.clock() - mark.at
	if age < 0 {
		// A mark stamped in the future is a clock that moved backwards, not a
		// stop answered before it was sent. Report it as freshly marked: the
		// conservative direction is to leave the turn alone.
		return 0, true
	}
	return age, true
}

// applyInterruptMarkLocked spends (or drops) a workspace's interrupt mark for
// the event being applied, returning the signal token and cause kind to
// append.
//
// A TURN END spends it: that end is the one the stop caused, so the row
// becomes `interrupted` instead of whatever the stop reason alone would have
// said. A TURN START drops it: a new turn beginning while the mark still
// stands means the marked turn's end was never observed, and honoring a stale
// mark on the NEXT turn would report a stop that turn never received.
//
// Caller holds mu.
func (m *Manager) applyInterruptMarkLocked(ws string, ev *corev1.Event, state, causeKind string) (string, string) {
	mark := m.interruptedTurn[ws]
	if mark == nil {
		return state, causeKind
	}
	switch ev.GetPayload().(type) {
	case *corev1.Event_TurnEnded:
		delete(m.interruptedTurn, ws)
		m.logf("ssm: turn end reported as `interrupted` ws=%s session=%s seq=%d (superseding %s) — a user-commanded stop was delivered to this turn",
			ws, ev.GetSessionId(), ev.GetSeq(), state)
		return sigInterrupted, causeInterrupted
	case *corev1.Event_TurnStarted:
		if mark.tolerateLateStart {
			// The stopped turn's own start, arriving through the store after
			// the stop already landed. It belongs to the marked turn: keep the
			// mark for the end that follows it.
			mark.tolerateLateStart = false
			m.logf("ssm: interrupt mark kept ws=%s session=%s seq=%d — the stopped turn's own late start arrived after the stop",
				ws, ev.GetSessionId(), ev.GetSeq())
			return state, causeKind
		}
		delete(m.interruptedTurn, ws)
		m.logf("ssm: interrupt mark dropped ws=%s session=%s seq=%d — a new turn started before the stopped turn's end was observed",
			ws, ev.GetSessionId(), ev.GetSeq())
	}
	return state, causeKind
}

// ApplySessionRotated reconciles the workspace's session-status lifecycle across a VENDOR
// SESSION UUID ROTATION: the vendor retired one transcript identity mid-stream
// and minted another (a `/clear` does exactly this), so the conversation
// continues under a new store seq space.
//
// A ROTATION IS A HARD BOUNDARY, and that is the whole reason this exists. The
// turn running when the uuid changed will never report its end under the OLD
// identity — its TurnEnded belongs to the new one — so the `thinking` row this
// log is holding has no arriving event that can supersede it. Left alone the
// workspace sits red forever, which is precisely the "footer stuck in THINKING"
// this reconciliation answers.
//
// It appends `idle` rather than `done`: nothing is running under the retired
// identity, and claiming a turn COMPLETED would put a conclusion on the wire
// that no vendor message ever reported. The row is daemon-local (no store seq),
// exactly as merge transitions and connection-degraded observations are, and it
// is superseded normally by whatever the agent does next — including the very
// TurnEnded that arrives moments later once the new space replays.
//
// A settled session-status lifecycle is left ALONE. A workspace sitting in `done` when the
// uuid rotated has nothing stuck to unstick, and appending `idle` over it would
// discard a more specific true statement. `permission` is NOT settled in that
// sense — it is a live turn wearing a green row — so it is released first and
// the turn it was covering is reconciled normally.
//
// A STANDING INTERRUPT MARK IS DROPPED, loudly. The mark names ONE turn — the
// one a user-commanded stop was delivered to — and spends itself on that turn's
// own end (applyInterruptMarkLocked). Across a rotation that end will never
// arrive, so the mark can only be spent by some LATER turn's end, reporting a
// stop that turn never received. Stale is the honest reading.
func (m *Manager) ApplySessionRotated(workspace, previous, next string) error {
	if workspace == "" {
		return fmt.Errorf("ssm: ApplySessionRotated got an empty workspace")
	}
	if next == "" {
		return fmt.Errorf("ssm: ApplySessionRotated got an empty rotated session id for workspace %q", workspace)
	}
	m.mu.Lock()
	defer m.mu.Unlock()

	if _, marked := m.interruptedTurn[workspace]; marked {
		delete(m.interruptedTurn, workspace)
		m.logf("ssm: interrupt mark DROPPED as stale ws=%s (vendor session rotated %s -> %s) — the stopped turn's end belongs to the retired identity and will never arrive, so the mark could only be spent by a later turn that received no stop",
			workspace, previous, next)
	}

	// NEITHER CONTEXT-CUT AXIS SURVIVES THE ROTATION, for opposite reasons.
	//
	// A COMPACTION does not survive the rotation that ends its identity: its
	// ContextCompacted belongs to the retired session and will never arrive,
	// exactly as the in-flight turn's end will not.
	//
	// A CLEAR does not survive it because the rotation IS the clear completing.
	// A `/clear` causes this rotation and nothing else the daemon dispatches
	// does, so an axis standing open when the uuid changes has just been
	// answered. This axis used to be held open here, waiting for a
	// ContextCleared "produced under the NEW identity" — a promise no component
	// keeps on its own: that event has exactly one producer, the shim-sidecar
	// tailing the vendor transcript, and when the sidecar is not running or has
	// not yet discovered the new uuid's transcript nothing ever closes the axis
	// and the clear rides the full ClearingTimeout into its expiry log. A
	// ContextCleared that DOES arrive afterwards still runs its close, which the
	// axis reports as unchanged.
	m.closeCompactingLocked(workspace, causeSessionRotated)
	m.closeClearingLocked(workspace, causeSessionRotated)

	// A PENDING PERMISSION DOES NOT SURVIVE THE ROTATION EITHER: the shim that
	// asked the question is bounced, every waiter on it is abandoned, and the
	// re-asked question arrives under the new identity as a fresh request. It is
	// released FIRST so the reconciliation below sees the session-status lifecycle's real
	// truth: the row buries the `thinking` of the turn that asked, and left
	// standing it would make the in-flight turn this method exists to unstick
	// look like a settled workspace with nothing stuck.
	m.closePermissionLocked(workspace, causeSessionRotated)

	active, err := turnActive(m.db, workspace)
	if err != nil {
		return err
	}
	if !active {
		m.logf("ssm: vendor session rotated ws=%s %s -> %s — no turn was in flight, session-status lifecycle left as it stands",
			workspace, previous, next)
		return nil
	}
	cause := causeSessionRotated + ":" + next
	// THE LEDGER GOES WITH THE AXIS. A rotation retires the conversation the
	// open claims belong to, so no identity remains that could ever end them.
	// Appending `idle` while leaving them standing would put the color and the
	// one turn-liveness derivation in contradiction — see retireTurnsLocked.
	tx, err := m.db.Begin()
	if err != nil {
		return fmt.Errorf("ssm: begin vendor rotation transaction for workspace %q: %w", workspace, err)
	}
	defer tx.Rollback()
	closed, liveness, err := m.retireTurnsLocked(tx, workspace, "", TurnCloseShimStopped)
	if err != nil {
		return err
	}
	if err := appendRow(tx, workspace, "", sigIdle, cause, sql.NullInt64{}, m.nextAt(), ""); err != nil {
		return err
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("ssm: commit vendor rotation for workspace %q: %w", workspace, err)
	}
	if len(closed) > 0 {
		m.logf("ssm: turn claims RETIRED BY A VENDOR ROTATION ws=%s %s -> %s closed=%s cause=%s liveness=%s — the conversation these turns ran in was retired mid-flight, so their ends can never arrive and each killed start's store coordinate is recorded durably against a later replay",
			workspace, previous, next, formatClosedTurnIDs(closed), TurnCloseShimStopped, liveness)
	}
	m.logf("ssm: vendor session rotated ws=%s %s -> %s — the in-flight turn's end belongs to the retired identity, so the session-status lifecycle is reconciled to `idle` rather than held in `thinking`",
		workspace, previous, next)
	return m.reresolveLocked(workspace, cause, 0)
}

// InvalidateTurnClaim releases a workspace's standing turn-in-flight claim when
// the session that made it can no longer end it — today, a session the user
// deleted mid-turn.
//
// A TURN CLAIM DIES WITH ITS SESSION. `thinking` is a promise that the session
// which wrote it will report the turn's end; a deleted session's shim is
// stopped and its stream is over, so nothing will ever supersede the row. Left
// standing it does two visible harms: the workspace resolves THINKING forever,
// and the readiness of whatever session comes next is suppressed by a claim
// that outlived its claimant.
//
// It is deliberately NARROW. The row is released only when the axis really
// tops out in `thinking` AND that row names staleSessionID — an unattributed
// claim, or one belonging to some other session, is not this session's to
// spend. A settled axis is left alone, exactly as ApplySessionRotated leaves it.
func (m *Manager) InvalidateTurnClaim(workspace, staleSessionID, reason string) error {
	if workspace == "" {
		return fmt.Errorf("ssm: InvalidateTurnClaim got an empty workspace")
	}
	if staleSessionID == "" {
		return fmt.Errorf("ssm: InvalidateTurnClaim for workspace %q got an empty session id; a claim can only be released on behalf of the session that made it", workspace)
	}
	m.mu.Lock()
	defer m.mu.Unlock()

	active, claimant, err := turnClaim(m.db, workspace)
	if err != nil {
		return err
	}
	if !active {
		m.logf("ssm: turn claim release ws=%s session=%s reason=%q — no turn was in flight, session-status lifecycle left as it stands",
			workspace, staleSessionID, reason)
		return nil
	}
	if claimant != staleSessionID {
		m.logf("ssm: turn claim release ws=%s session=%s reason=%q DECLINED — the standing `thinking` is held by session=%q, which is not this one's to spend",
			workspace, staleSessionID, reason, claimant)
		return nil
	}
	cause := causeSessionEnded + ":" + reason
	// THE LEDGER GOES WITH THE AXIS, for the same reason the rotation's does:
	// turn liveness is derived from the claims, so releasing the row while
	// leaving the claim standing would leave the color idle and the prompt
	// queue holding.
	tx, err := m.db.Begin()
	if err != nil {
		return fmt.Errorf("ssm: begin turn claim invalidation for workspace %q: %w", workspace, err)
	}
	defer tx.Rollback()
	closed, liveness, err := m.retireTurnsLocked(tx, workspace, staleSessionID, TurnCloseShimStopped)
	if err != nil {
		return err
	}
	if err := appendRow(tx, workspace, staleSessionID, sigIdle, cause, sql.NullInt64{}, m.nextAt(), ""); err != nil {
		return err
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("ssm: commit turn claim invalidation for workspace %q: %w", workspace, err)
	}
	if len(closed) > 0 {
		m.logf("ssm: turn claims RETIRED WITH THEIR SESSION ws=%s session=%s reason=%q closed=%s cause=%s liveness=%s — the session that opened these turns is gone, so their ends can never arrive and each killed start's store coordinate is recorded durably against a later replay",
			workspace, staleSessionID, reason, formatClosedTurnIDs(closed), TurnCloseShimStopped, liveness)
	}
	m.logf("ssm: turn claim INVALIDATED ws=%s session=%s reason=%q — the session holding `thinking` is gone and its turn's end will never arrive, so the session-status lifecycle is reconciled to `idle` rather than held in `thinking`",
		workspace, staleSessionID, reason)
	return m.reresolveLocked(workspace, cause, 0)
}

// ApplyBackfillState records the workspace's transcript-backfill outcome.
//
// A FAILED backfill compromises the route and resolves BLUE: the workspace's
// history is incomplete, so anything painted from it is a partial account of
// the conversation, and calling that ready would be the lie the whole
// vocabulary exists to prevent.
//
// DONE and the empty-workspace case both CLEAR the axis. "Nothing to
// backfill" is a real, correct answer — a genuinely fresh workspace — not an
// unknown, so it must not hold the workspace blue.
//
// STATE is the sessioncontroller token ("pending" | "done" | "failed"). `pending` is
// deliberately NOT blue on its own: a session mid-backfill has not been wired
// yet and the legacy connectivity projection already holds it blue, and treating pending as a
// separate blue would mean a REOPENED session whose history is already in the
// store — and which therefore never emits a fresh transition — could never
// leave it. See sessioncontroller.settleBackfillFromStore for the other half of that.
func (m *Manager) ApplyBackfillState(workspace, state string) error {
	if workspace == "" {
		return fmt.Errorf("ssm: ApplyBackfillState got an empty workspace")
	}
	token := sigBackfillOK
	if state == "failed" {
		token = sigBackfillFailed
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	at := m.nextAt()
	cause := "backfill:" + state
	if err := appendRow(m.db, workspace, "", token, cause, sql.NullInt64{}, at, ""); err != nil {
		return err
	}
	return m.reresolveLocked(workspace, cause, 0)
}

// ApplyConnectionDegraded records the daemon's OWN observation of the shim
// transport going quiet, or coming back (F4).
//
// The legacy impairment projection already existed, fed by the shim's own DegradedState
// events. What did not reach it was the transport-level miss the daemon
// detects itself: the missed-heartbeat window called the Degraded sink and
// nothing appended a row, so a heartbeat miss produced a banner and no state
// at all. Retiring that banner without this would have lost the ambience
// entirely.
//
// degraded=false CLEARS the axis, the same token the shim's recovery writes,
// so the two sources of the same fact settle the same way.
func (m *Manager) ApplyConnectionDegraded(workspace string, degraded bool, reason string) error {
	if workspace == "" {
		return fmt.Errorf("ssm: ApplyConnectionDegraded got an empty workspace")
	}
	token := sigDegradedClear
	cause := "connection_recovered"
	if degraded {
		token = sigDegraded
		cause = "connection_degraded"
	}
	if reason != "" {
		cause = cause + ":" + reason
	}

	m.mu.Lock()
	defer m.mu.Unlock()

	at := m.nextAt()
	// Daemon-local: the observation is the daemon's own, so it carries no
	// store seq and no task id.
	if err := appendRow(m.db, workspace, "", token, cause, sql.NullInt64{}, at, ""); err != nil {
		return err
	}
	return m.reresolveLocked(workspace, cause, 0)
}

// ApplyMergeTransition records a daemon-local merge phase change (merge
// state lives ONLY in the SSM, §9.2). phase is a merge signal token
// (merge_enqueuing|merging|merge_queued|merge_conflict|merge_failed|merged)
// or the empty
// string / merge_none to clear the merge axis. cause is a short human note
// recorded as the cause kind's detail; it never carries a store seq.
// A `merged` phase additionally establishes the workspace's durable merged-at
// fact and stands its session down. Both happen HERE, at the one transition
// that knows the merge landed, rather than at a second producer that would
// have to be told.
func (m *Manager) ApplyMergeTransition(workspace, phase, cause string) error {
	return m.applyMergeTransition(workspace, phase, cause, nil)
}

// ApplyMergeStatus records a merge transition TOGETHER with the phase-level
// MergeStatus the merge pipeline published for it.
//
// It is the pipeline's entry point, and it is one call rather than two on
// purpose: the axis row and the status describe the same event, so a caller that
// could record one without the other is a caller that can leave a frontend
// rendering a phase word that disagrees with the progress beneath it.
//
// The status rides the ONE WorkspaceState construction site
// (workspaceMessageLocked, via stampMergeStatusLocked), exactly as
// merge_lease_held and the merged-at instant do — never stamped onto a frame
// around it.
func (m *Manager) ApplyMergeStatus(workspace, phase, cause string, status *frontendv1.MergeStatus) error {
	if status == nil {
		// A pipeline publication with no status is the caller having lost the
		// very thing this entry point exists to carry. ApplyMergeTransition is
		// the call for a transition that has no run behind it.
		return fmt.Errorf("ssm: ApplyMergeStatus for workspace %q phase %q got a nil status", workspace, phase)
	}
	return m.applyMergeTransition(workspace, phase, cause, status)
}

func (m *Manager) applyMergeTransition(workspace, phase, cause string, status *frontendv1.MergeStatus) error {
	if workspace == "" {
		return fmt.Errorf("ssm: ApplyMergeTransition got an empty workspace")
	}
	token, err := mergeToken(phase)
	if err != nil {
		return err
	}
	causeKind := causeMergeTransition
	if cause != "" {
		causeKind = causeMergeTransition + ":" + cause
	}

	mergedAt, established, err := m.appendMergeTransition(workspace, token, causeKind, status)
	if err != nil {
		return err
	}
	if token != sigMerged {
		return nil
	}
	if !established {
		// A SECOND `merged` row for a workspace that already landed. The merge
		// pipeline writes one when it finishes the after-action so the terminal
		// status can carry the after-action's outcome, and the durable merged-at
		// fact is set-once, so this row moves nothing. Standing the session down
		// AGAIN would hibernate a session the first teardown already took, which
		// the log would report as a failure of something that in fact succeeded.
		m.logf("ssm: merged teardown SKIPPED ws=%s merged_at_ms=%d — this workspace already landed and was already stood down; the row updates the phase status only",
			workspace, mergedAt)
		return nil
	}
	// OUTSIDE THE LOCK, and after the `merged` state has been handed to the
	// subscribers. The teardown re-enters the SSM (its hibernation writes a
	// connectivity edge), so holding mu across it would deadlock — and running
	// it after the push is what guarantees a frontend is told the workspace
	// merged before anything starts taking the session away. See
	// merged.go for why it reports failure through the log rather than here.
	m.teardownMerged(workspace, mergedAt)
	return nil
}

// appendMergeTransition writes the merge row, records the merged-at fact when
// the phase is `merged`, and pushes the resolved state. It owns the lock for
// exactly that span so the teardown that may follow runs outside it.
//
// mergedAt is the workspace's durable merge instant, and 0 for every phase
// other than `merged`. established reports whether THIS row is the one that
// landed the merge, which is what gates the one-time teardown.
func (m *Manager) appendMergeTransition(workspace, token, causeKind string, status *frontendv1.MergeStatus) (mergedAt int64, established bool, err error) {
	m.mu.Lock()
	defer m.mu.Unlock()

	switch {
	case status != nil:
		// Retained BEFORE the resolve below, so the very frame this transition
		// pushes already carries the pipeline's account of it.
		m.recordPipelineStatusLocked(workspace, status)
	case token == sigMergeNone:
		// The axis is cleared: the run is over and nothing it reported is true
		// any more, so the retained status goes with it rather than describing a
		// run no frontend should still be rendering.
		m.retirePipelineStatusLocked(workspace, causeKind)
	case token == sigMergeEnqueuing:
		// A FRESH ATTEMPT SUPERSEDES THE OLD VERDICT. This row is a NEW merge
		// arriving, so whatever the previous run published — most consequentially
		// a terminal `failed` status and its cause text — describes a run that is
		// no longer what this workspace is doing. The row itself supersedes the
		// axis (it is the newest merge row from here on, so a stale `merge_failed`
		// stops resolving), and this retires the account that hung off it; the new
		// run publishes its own the moment it has one. Leaving it retained put the
		// old failure's cause on every frame of the retry. See mergefailedclear.go.
		m.retirePipelineStatusLocked(workspace, causeKind)
	}
	// A MERGE THAT REACHED ITS OWN END ANSWERS THE QUESTION BY ENDING. Whether
	// it merged, failed, or had its axis cleared, there is no longer a merge on
	// the queue to take off — so the card comes down here rather than standing
	// over a workspace whose merge is already gone, offering to dequeue it.
	//
	// This is also the path a CONFIRMED dequeue comes down: the abort or the
	// eviction publishes the run's terminal `failed` status, which lands right
	// here. The answer handler clears the offer too, and deliberately: the two
	// are independent, so an eviction that failed to publish still takes the
	// card down, and a merge that terminated on its own never leaves one up.
	if token == sigMerged || token == sigMergeFailed || token == sigMergeNone {
		m.clearMergeDequeueOfferLocked(workspace, "merge_transition:"+token)
	}

	at := m.nextAt()
	// Daemon-local: no store seq, cause_seq stays NULL, and a merge is not a
	// task so it carries no task id.
	if err := appendRow(m.db, workspace, "", token, causeKind, sql.NullInt64{}, at, ""); err != nil {
		return 0, false, err
	}
	if token == sigMerged {
		// BEFORE the resolve below, so the very frame that first says `merged`
		// already carries merged_at_ms. Stamping it afterwards would push one
		// merged state with a zero instant, and a frontend that ordered on it
		// would file the workspace at the beginning of time.
		mergedAt, established, err = m.recordMergedAtLocked(workspace, at)
		if err != nil {
			return 0, false, err
		}
	}
	if err := m.reresolveLocked(workspace, causeKind, 0); err != nil {
		return 0, false, err
	}
	return mergedAt, established, nil
}

// reresolveLocked recomputes the workspace's state, loud-logs a transition
// when it changed, and pushes the new WorkspaceState to subscribers. Caller
// holds mu.
//
// A push happens when EITHER the render state or the live-task count moved.
// The count is not a debug field: it is what the webapp footer's live-task
// figure renders, so a count-only change (background tasks going 5→2, or a
// reconciliation sweeping ghosts) that produced no push left a stale number on
// screen with nothing that could ever correct it.
func (m *Manager) reresolveLocked(workspace, causeKind string, causeSeq uint64) error {
	r, err := resolve(m.db, workspace, m.logf)
	if err != nil {
		return err
	}
	if !r.found {
		composite, found, err := resolveComposite(m.db, workspace)
		if err != nil {
			return err
		}
		if !found || composite.LifecycleTop == "" {
			return nil
		}
		r = resolved{found: true, state: frontendv1.RenderState_RENDER_STATE_UNSPECIFIED}
	}
	msg, err := m.workspaceMessageLocked(workspace, r)
	if err != nil {
		return err
	}
	old, had := m.last[workspace]
	oldTasks, hadTasks := m.lastTasks[workspace]
	stateMoved := !had || old != msg.GetState()
	tasksMoved := !hadTasks || oldTasks != msg.GetLiveTaskCount()
	// A MERGE RUN'S PROGRESS IS ITS OWN DELTA. Most of what a run publishes
	// never moves the render state — every phase from the first pick to the last
	// test resolves to `merging` — so without this key the whole middle of a
	// merge is dropped as "nothing visible changed" and the user watches a
	// progress bar that never advances. proto.Equal rather than a pointer
	// compare: the pipeline builds a FRESH MergeStatus per publication, so two
	// pointers differ even when the run said the same thing twice, and pushing on
	// that would republish an unchanged run on every unrelated re-resolve.
	statusMoved := !proto.Equal(m.lastMergeStatus[workspace], msg.GetMergeStatus())
	if !stateMoved && !tasksMoved && !statusMoved {
		return nil // nothing visible changed; stay quiet (§12: log deltas only)
	}
	m.last[workspace] = msg.GetState()
	m.lastTasks[workspace] = msg.GetLiveTaskCount()
	m.lastMergeStatus[workspace] = msg.GetMergeStatus()

	if stateMoved {
		oldName := "∅"
		if had {
			oldName = renderName(old)
		}
		// §12 SSM contract: every transition logged old→new + cause kind + seq.
		m.logf("ssm: transition ws=%s %s→%s cause_kind=%s cause_seq=%d turn_active=%t live_tasks=%d merge=%q",
			workspace, oldName, renderName(msg.GetState()), causeKind, causeSeq, msg.GetTurnActive(), msg.GetLiveTaskCount(), r.mergePhase)
	} else if !tasksMoved {
		// A merge-progress-only push. Logged on its own axis so a within-phase
		// tick is not reported as a live_tasks delta it did not cause.
		m.logf("ssm: merge_status ws=%s run=%s phase=%T cause_kind=%s cause_seq=%d merge=%q",
			workspace, msg.GetMergeStatus().GetRunId(), msg.GetMergeStatus().GetPhase(), causeKind, causeSeq, r.mergePhase)
	} else {
		m.logf("ssm: live_tasks ws=%s %d→%d cause_kind=%s cause_seq=%d state=%s",
			workspace, oldTasks, msg.GetLiveTaskCount(), causeKind, causeSeq, renderName(msg.GetState()))
	}

	return m.pushMessageLocked(workspace, msg)
}

// stagePublishLocked queues one SYNCHRONOUS frontend publication to run once mu
// is released. Caller holds mu.
//
// THE BARRIER MOVED OFF THE LOCK HOLD, and it had to. A synchronous publisher
// is the frontend's Broadcast: it takes the frontend's own lock, and the
// frontend's materialization release calls back into this manager's snapshot
// while holding that lock. Calling out from under mu therefore closed a cycle
// in which mu was held forever — and mu is what every session controller and
// the merge queue's status ingest need, so all of them wedged behind it. That
// happened twice in production.
//
// The ordering the lock hold bought is kept exactly: the position of a
// publication is fixed HERE, under mu, so no later transition can stage ahead
// of an earlier one, and drainPublications replays that order one at a time.
func (m *Manager) stagePublishLocked(emit func()) {
	m.pendingPublications = append(m.pendingPublications, emit)
}

// drainPublications emits every staged publication, in staging order, with mu
// RELEASED. emitMu admits one drainer, so a goroutine that has staged but not
// yet reached here cannot have its publication overtaken — whichever drainer
// runs first emits it, in the order mu fixed.
//
// Callers register it with `defer m.drainPublications()` BEFORE taking mu, so
// the deferred unlock runs first.
func (m *Manager) drainPublications() {
	m.emitMu.Lock()
	defer m.emitMu.Unlock()
	for {
		m.mu.Lock()
		if len(m.pendingPublications) == 0 {
			m.mu.Unlock()
			return
		}
		emit := m.pendingPublications[0]
		m.pendingPublications = m.pendingPublications[1:]
		m.mu.Unlock()
		emit()
	}
}

// pushLocked broadcasts a WorkspaceState to every subscriber. A full
// subscriber channel is a slow consumer: the update is dropped loudly (the
// consumer recovers from a subsequent Snapshot), never blocked on.
func (m *Manager) pushLocked(workspace string, r resolved) error {
	if len(m.subs) == 0 {
		return nil
	}
	msg, err := m.workspaceMessageLocked(workspace, r)
	if err != nil {
		return err
	}
	return m.pushMessageLocked(workspace, msg)
}

// workspaceMessageLocked builds THE WorkspaceState for a workspace. Caller
// holds mu.
//
// Every producer of a pushed, snapshotted or synchronously published frame
// funnels through here, which is what makes a frame missing the merge facts
// unrepresentable rather than merely unlikely: there is one construction site
// to stamp, not four to remember.
func (m *Manager) workspaceMessageLocked(workspace string, r resolved) (*frontendv1.WorkspaceState, error) {
	composite, found, err := resolveComposite(m.db, workspace)
	if err != nil {
		return nil, fmt.Errorf("ssm: resolve composite for push workspace=%q: %w", workspace, err)
	}
	msg, err := m.composeWorkspaceMessage(workspace, r, composite, found)
	if err != nil {
		return nil, err
	}
	m.stampWorkspaceMessageLocked(workspace, r, msg)
	return msg, nil
}

// composeWorkspaceMessage is the DB-RESOLVED HALF of the construction funnel:
// everything the frame gets from the log, and nothing it gets from this
// process's memory.
//
// It touches no Manager map, so it is safe to run with mu RELEASED — which is
// what lets Snapshot resolve a whole fleet without starving the mutating entry
// points. The in-memory half (the merge facts and the freshness stamp) is
// stampWorkspaceMessageLocked, and every producer still runs both: the split is
// where the lock is taken, not how many construction sites there are.
func (m *Manager) composeWorkspaceMessage(workspace string, r resolved, composite CompositeState, found bool) (*frontendv1.WorkspaceState, error) {
	var msg *frontendv1.WorkspaceState
	switch {
	case !found || composite.LifecycleTop == "":
		msg = r.toProto(workspace)
		// A WORKSPACE WITH NO CONTROLLER-GENERATION LIFECYCLE IS HIBERNATED,
		// which is the enum's own definition of the case: no generation is
		// current, no fault is implied, and the session may be brought up on
		// demand (frontend.proto SESSION_CONNECTIVITY_HIBERNATED). Leaving the
		// field at its zero value published an UNSPECIFIED connectivity, which
		// names a malformed frame rather than a verdict, and the webapp's
		// validating decoder refuses the whole frame over it.
		msg.Connectivity = frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_HIBERNATED
		m.logf("ssm: workspace state ws=%s has no controller-generation lifecycle; connectivity=hibernated (no generation is current)", workspace)
	default:
		built, err := compositeWorkspaceState(workspace, r, composite)
		if err != nil {
			return nil, err
		}
		msg = built
	}
	if err := connectivityResolved(workspace, msg, composite, found); err != nil {
		return nil, err
	}
	return msg, nil
}

// stampWorkspaceMessageLocked is the IN-MEMORY HALF of the construction funnel.
// Caller holds mu. Both stamps read (and the freshness stamp writes) per-process
// maps, so no frame may go out without this having run under the lock.
func (m *Manager) stampWorkspaceMessageLocked(workspace string, r resolved, msg *frontendv1.WorkspaceState) {
	m.stampMergeFactsLocked(workspace, r, msg)
	m.stampFreshnessLocked(workspace, msg)
}

// connectivityResolved is THE LAST GATE BEFORE THE WIRE. Every pushed,
// snapshotted and published WorkspaceState is built by workspaceMessageLocked,
// so refusing an unresolved connectivity there makes an UNSPECIFIED one
// unrepresentable downstream rather than merely unlikely: the frontend contract
// has no reading for it, and the webapp's validating decoder refuses the whole
// frame — including every snapshot a retained frame is replayed into.
//
// It is an error, never a substituted default: the daemon is the sole authority
// for this verdict and has no honest guess to offer.
func connectivityResolved(workspace string, msg *frontendv1.WorkspaceState, composite CompositeState, found bool) error {
	if msg.GetConnectivity() != frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_UNSPECIFIED {
		return nil
	}
	return fmt.Errorf("ssm: workspace %q resolved to UNSPECIFIED session connectivity (composite connectivity=%q lifecycle_top=%q composite_found=%t); refusing to publish a frame no frontend can read",
		workspace, composite.Connectivity, composite.LifecycleTop, found)
}

// stampFreshnessLocked makes AtMs MONOTONIC per workspace within this
// process. The composite builder stamps AtMs from whichever axis branch wins
// (a status row, a fault's open instant, a connectivity edge) — three
// different clocks, and a re-rank can hand the win back to an OLDER row. The
// frontend's snapshot-freshness check records the newest AtMs it ever
// delivered and refuses any snapshot older than it, so a regressing stamp
// wedged the connect loop into an unbounded retry storm (observed: a
// merge_failed push stamped newer than every later snapshot could resolve,
// which also starved that connection's command reads — merges silently
// vanished).
//
// The watermark is deliberately IN-MEMORY: the frontend's delivered watermark
// is in-memory too, and both reset together at daemon boot, so durability
// would add nothing but a stale floor.
//
// IT BUMPS, IT DOES NOT CLAMP, AND THAT IS THE WHOLE OF THE SECOND RULE.
// AtMs is not merely a freshness floor: the webapp treats it as the frame's
// REVISION and holds the invariant that one revision names one state. Lifting a
// regressing stamp to the watermark minted two DIFFERENT composites carrying the
// identical revision, which the webapp reported as `revision conflicted`. A
// composite that is not newer than the watermark and not identical to what was
// last delivered therefore takes watermark+1 — still monotonic, still never
// older than anything delivered, and never a duplicate revision.
//
// AN IDENTICAL COMPOSITE KEEPS ITS REVISION. A resync's Snapshot rebuilds the
// same frame the last push carried; bumping there would mint a fresh revision
// for a frame that says nothing new, and the frontend would see the state
// change on every reconnect. Identity is compared over the whole message with
// AtMs normalized away, so only a real content difference earns a new revision.
func (m *Manager) stampFreshnessLocked(workspace string, msg *frontendv1.WorkspaceState) {
	// Lazily initialized because the Manager has more than one construction
	// path (Open plus the test rigs' direct literals); a nil map here must
	// never turn a state push into a panic.
	if m.pushedAtMs == nil {
		m.pushedAtMs = make(map[string]int64)
	}
	if m.stampedComposite == nil {
		m.stampedComposite = make(map[string]*frontendv1.WorkspaceState)
	}
	if m.publishEpoch == nil {
		m.publishEpoch = make(map[string]uint64)
	}
	// EVERY outgoing frame passes here, which is what makes this the honest
	// place to count them: a lock-free snapshot compares the count it started
	// with against the count it finished with to learn whether anything was
	// published for the workspace underneath it.
	m.publishEpoch[workspace]++
	at := msg.GetAtMs()
	if prev, ok := m.pushedAtMs[workspace]; ok && at <= prev {
		if last := m.stampedComposite[workspace]; last != nil && sameComposite(last, msg) {
			msg.AtMs = prev
			return
		}
		at = prev + 1
	}
	msg.AtMs = at
	m.pushedAtMs[workspace] = at
	m.stampedComposite[workspace] = proto.Clone(msg).(*frontendv1.WorkspaceState)
}

// sameComposite reports whether two WorkspaceStates say the same thing, ignoring
// AtMs. AtMs is the revision the caller is deciding, so comparing it would make
// every frame differ from every other and defeat the check entirely.
func sameComposite(a, b *frontendv1.WorkspaceState) bool {
	lhs := proto.Clone(a).(*frontendv1.WorkspaceState)
	rhs := proto.Clone(b).(*frontendv1.WorkspaceState)
	lhs.AtMs = 0
	rhs.AtMs = 0
	return proto.Equal(lhs, rhs)
}

func (m *Manager) pushMessageLocked(workspace string, msg *frontendv1.WorkspaceState) error {
	for id, ch := range m.subs {
		select {
		case ch <- msg:
		default:
			m.logf("ssm: subscriber %d slow; dropped ws=%s state=%s connectivity=%s status=%s (will resync via Snapshot)",
				id, workspace, renderName(msg.GetState()), msg.GetConnectivity(), msg.GetStatus())
		}
	}
	return nil
}

// Current returns the resolved WorkspaceState for a workspace. found is
// false when the workspace has no render-bearing signals (unborn / only
// counters) — an explicit miss, not a silent default.
func (m *Manager) Current(workspace string) (*frontendv1.WorkspaceState, bool, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.currentLocked(workspace)
}

// currentLocked is Current's lock-owning core. Caller holds m.mu. Keeping the
// resolver behind this helper lets synchronous publication validate and emit
// one exact state without dropping the ordering lock between those actions.
func (m *Manager) currentLocked(workspace string) (*frontendv1.WorkspaceState, bool, error) {
	built, found, err := m.resolveWorkspace(workspace)
	if err != nil {
		return nil, false, err
	}
	if !found {
		return nil, false, nil
	}
	m.stampWorkspaceMessageLocked(built.workspace, built.resolution, built.msg)
	return built.msg, true, nil
}

// builtWorkspace is one workspace's frame BEFORE the in-memory stamps: the
// resolution it came from (the merge stamp needs it) plus the message the log
// alone produced.
type builtWorkspace struct {
	workspace  string
	resolution resolved
	msg        *frontendv1.WorkspaceState
}

// resolveWorkspace performs the ENTIRE database half of one workspace's frame:
// the render resolution, the composite resolution, the unborn-workspace miss,
// and the message construction. found=false is the same explicit miss Current
// reports.
//
// It reads no Manager map, so it runs correctly with or without mu. Callers
// holding mu (currentLocked) simply keep holding it; Snapshot runs it with mu
// RELEASED and stamps afterwards.
//
// Resolving the composite ONCE is also a real saving: Snapshot and Current both
// used to resolve it a second time inside the construction funnel, which at
// fleet scale was ~13% of a full snapshot spent asking the same question twice.
func (m *Manager) resolveWorkspace(workspace string) (builtWorkspace, bool, error) {
	r, err := resolve(m.db, workspace, m.logf)
	if err != nil {
		return builtWorkspace{}, false, err
	}
	composite, compositeFound, err := resolveComposite(m.db, workspace)
	if err != nil {
		return builtWorkspace{}, false, fmt.Errorf("ssm: resolve composite for push workspace=%q: %w", workspace, err)
	}
	if !r.found && (!compositeFound || composite.LifecycleTop == "") {
		return builtWorkspace{}, false, nil
	}
	if !r.found {
		r = resolved{found: true, state: frontendv1.RenderState_RENDER_STATE_UNSPECIFIED}
	}
	msg, err := m.composeWorkspaceMessage(workspace, r, composite, compositeFound)
	if err != nil {
		return builtWorkspace{}, false, err
	}
	return builtWorkspace{workspace: workspace, resolution: r, msg: msg}, true, nil
}

// snapshotFlight is one full-fleet resolve in progress. out and err are written
// by the leader before done is closed, so every sharer reads them with a
// happens-before edge and no lock of its own.
type snapshotFlight struct {
	done chan struct{}
	out  []*frontendv1.WorkspaceState
	err  error
}

// Snapshot returns the current WorkspaceState of every workspace with a
// resolved render state, in stable workspace order (for a frontend resync).
//
// IT DOES NOT HOLD mu WHILE IT RESOLVES, and that is the whole of the fix here.
// A full-fleet resolve is hundreds of SQL statements — at the observed live
// scale (161 workspaces, 22k state rows) seconds of work — and it used to run
// under mu from the first query to the last. Snapshot is called on every client
// connect, on every resync command and periodically per client by the lease
// loop, so during webview reconnect churn the lock was effectively never free:
// MarkPromptAccepted, Current, LastActivityMs and the lease renewals all queued
// behind it, and the observable symptom was the daemon never acking a user's
// prompt.
//
// CONCURRENT CALLERS COALESCE onto one resolve. N clients reconnecting together
// asked the same question N times and paid for it N times; now the first pays
// and the rest share its answer.
//
// WHAT CONSISTENCY IS PROMISED. Each frame is resolved from the database, whose
// own serialization makes it a consistent read of that workspace; the fleet is
// NOT resolved atomically, so two workspaces may be read an instant apart. That
// is exactly what the wire contract already assumes — WorkspaceState is a
// per-workspace frame and the frontend applies it per workspace.
//
// The one hazard of dropping the lock is publishing content OLDER than a frame
// the client already holds while stamping it with a NEWER revision, which the
// webapp reads as the state going backwards. It is closed rather than made
// unlikely: publishEpoch counts every frame stamped per workspace, and any
// workspace whose epoch moved during the lock-free resolve is re-resolved under
// mu before it is stamped — under mu no push can intervene, so the frame that
// goes out is never older than the last one published. A workspace whose log
// gained a row that nobody published a frame for is NOT re-resolved: nothing was
// delivered, so nothing can regress, and the push that eventually carries it
// supersedes the snapshot in the ordinary way.
func (m *Manager) Snapshot() ([]*frontendv1.WorkspaceState, error) {
	m.flightMu.Lock()
	if inflight := m.snapshotFlight; inflight != nil {
		m.flightMu.Unlock()
		if m.snapshotJoined != nil {
			m.snapshotJoined()
		}
		<-inflight.done
		return inflight.out, inflight.err
	}
	flight := &snapshotFlight{done: make(chan struct{})}
	m.snapshotFlight = flight
	m.flightMu.Unlock()

	flight.out, flight.err = m.resolveSnapshot()

	m.flightMu.Lock()
	m.snapshotFlight = nil
	m.flightMu.Unlock()
	close(flight.done)
	return flight.out, flight.err
}

// resolveSnapshot is Snapshot's leader path: resolve the fleet from the
// database with mu released, then take mu once to stamp the in-memory half of
// every frame.
func (m *Manager) resolveSnapshot() ([]*frontendv1.WorkspaceState, error) {
	m.snapshotResolves.Add(1)

	m.mu.Lock()
	startEpoch := make(map[string]uint64, len(m.publishEpoch))
	for ws, epoch := range m.publishEpoch {
		startEpoch[ws] = epoch
	}
	m.mu.Unlock()

	names, err := distinctWorkspaces(m.db)
	if err != nil {
		return nil, err
	}
	built := make([]builtWorkspace, 0, len(names))
	for _, ws := range names {
		b, found, err := m.resolveWorkspace(ws)
		if err != nil {
			return nil, err
		}
		if !found {
			continue
		}
		built = append(built, b)
	}

	// The seam sits at the END of the lock-free half: a test holding it open is
	// holding a resolve that is complete but unstamped, which is both the window
	// in which mu must still be free and the window in which a publication can
	// overtake the resolved content.
	if m.snapshotGate != nil {
		m.snapshotGate()
	}

	m.mu.Lock()
	defer m.mu.Unlock()
	out := make([]*frontendv1.WorkspaceState, 0, len(built))
	resolvedDuring := make(map[string]bool, len(built))
	for _, b := range built {
		resolvedDuring[b.workspace] = true
		if m.publishEpoch[b.workspace] != startEpoch[b.workspace] {
			fresh, found, err := m.resolveWorkspace(b.workspace)
			if err != nil {
				return nil, err
			}
			if !found {
				continue
			}
			b = fresh
		}
		m.stampWorkspaceMessageLocked(b.workspace, b.resolution, b.msg)
		out = append(out, b.msg)
	}
	// A WORKSPACE BORN DURING THE RESOLVE still belongs in the snapshot. Its
	// first frame was published while the fleet was being read — a client that
	// subscribed after that push and reads this snapshot would otherwise never
	// be told the workspace exists. The publication counter names it, so it is
	// resolved here under mu like any workspace the resolve raced.
	for ws, epoch := range m.publishEpoch {
		if resolvedDuring[ws] || epoch == startEpoch[ws] {
			continue
		}
		fresh, found, err := m.resolveWorkspace(ws)
		if err != nil {
			return nil, err
		}
		if !found {
			continue
		}
		m.stampWorkspaceMessageLocked(fresh.workspace, fresh.resolution, fresh.msg)
		out = append(out, fresh.msg)
	}
	// Stable workspace order is part of Snapshot's contract, and the late
	// additions above arrive out of order by construction.
	sort.Slice(out, func(i, j int) bool { return out[i].GetWorkspace() < out[j].GetWorkspace() })
	return out, nil
}

// Subscribe registers a push channel for state changes and returns it with
// an unsubscribe func. The frontend layer consumes these WorkspaceState
// pushes. The returned cancel is idempotent.
func (m *Manager) Subscribe() (<-chan *frontendv1.WorkspaceState, func()) {
	m.mu.Lock()
	defer m.mu.Unlock()
	id := m.nextSub
	m.nextSub++
	ch := make(chan *frontendv1.WorkspaceState, subBufferSize)
	m.subs[id] = ch
	var once sync.Once
	cancel := func() {
		once.Do(func() {
			m.mu.Lock()
			defer m.mu.Unlock()
			if c, ok := m.subs[id]; ok {
				delete(m.subs, id)
				close(c)
			}
		})
	}
	return ch, cancel
}

// Close closes all subscriber channels and the database.
func (m *Manager) Close() error {
	m.mu.Lock()
	if m.closed {
		m.mu.Unlock()
		return nil
	}
	m.closed = true
	for ws := range m.clearingTimers {
		m.disarmClearingWatchdogLocked(ws)
	}
	for id, ch := range m.subs {
		delete(m.subs, id)
		close(ch)
	}
	m.mu.Unlock()
	if !m.ownsDB {
		return nil
	}
	return m.db.Close()
}

// toProto renders the resolution as the frontend WorkspaceState message.
func (r resolved) toProto(workspace string) *frontendv1.WorkspaceState {
	return &frontendv1.WorkspaceState{
		Workspace:     workspace,
		SessionId:     r.sessionID,
		State:         r.state,
		TurnActive:    r.turnActive,
		LiveTaskCount: r.liveTaskCount,
		CauseKind:     r.causeKind,
		CauseSeq:      r.causeSeq,
		AtMs:          r.atMs,
	}
}

// taskIDOf returns the task id a task-lifecycle event is about, or "" for
// every other event. It is what lets the live-task counter dedupe per task:
// the same task ending twice (a spool's EXIT= marker after a TaskStop tool
// result, or twin planes reporting the same completion) must decrement the
// counter once, not once per event.
func taskIDOf(ev *corev1.Event) string {
	switch p := ev.GetPayload().(type) {
	case *corev1.Event_TaskStarted:
		return p.TaskStarted.GetTaskId()
	case *corev1.Event_TaskEnded:
		return p.TaskEnded.GetTaskId()
	default:
		return ""
	}
}

// agentOrTaskSignal maps a lifecycle Event to the signal token it appends
// and the cause kind recorded with it. The mapping mirrors the reference
// elisp sentinel handlers: session_start→idle (agent ready), turn (prompt)
// start→thinking, turn end→done (clean) / stop_failed (error), session
// end→dead; task start/end feed the live-task counter. ok is false for
// events that are not SSM-relevant.
func agentOrTaskSignal(ev *corev1.Event) (state, causeKind string, ok bool) {
	switch ev.GetPayload().(type) {
	case *corev1.Event_SessionStarted:
		// READY, not idle: the shim asserts SessionStarted at its OWN
		// readiness (session lock held, daemon handshake complete, SDK query
		// constructed), so this is the moment the route is proven usable
		// WITHOUT a first message ever having been sent.
		return sigReady, causeSessionStarted, true
	case *corev1.Event_TurnStarted:
		return sigThinking, causeTurnStarted, true
	case *corev1.Event_TurnEnded:
		// EXACTLY ONE session-status lifecycle row per turn end, naming HOW the turn
		// ended. `vendor_blocked` and `done` are the same kind of fact —
		// a report of the concluded turn — so they are the same axis and
		// the same row, and whatever the agent does next supersedes it.
		//
		// Writing a second row on a vendor axis instead was the latch that
		// made a workspace purple forever: a session that died blocked
		// could never emit the clean turn that released it.
		te := ev.GetTurnEnded()
		if VendorBlockingTurnEnd(te.GetStopReason(), te.GetIsError()) {
			return sigVendorBlocked, causeVendorBlocked, true
		}
		return sigDone, causeTurnEnded, true
	case *corev1.Event_SessionEnded:
		return sigDead, causeSessionEnded, true
	case *corev1.Event_TaskStarted:
		return sigTaskStarted, causeTaskStarted, true
	case *corev1.Event_TaskEnded:
		// Includes LOST (a lost task stopped; the counter decrements the same
		// way, distinct terminal status is preserved on the source event).
		return sigTaskEnded, causeTaskEnded, true
	case *corev1.Event_DegradedState:
		if ev.GetDegradedState().GetRecovered() {
			return sigDegradedClear, "degraded_recovered", true
		}
		return sigDegraded, "degraded", true
	default:
		return "", "", false
	}
}

// mergeToken validates and normalizes a merge phase argument to a stored
// signal token. An unknown phase errors loudly (no silent fallback).
func mergeToken(phase string) (string, error) {
	switch phase {
	case "", sigMergeNone:
		return sigMergeNone, nil
	case sigMergeEnqueuing, sigMerging, sigMergeBeforeAction, sigMergeAfterAction,
		sigMergeQueued, sigMergeConflict, sigMergeFailed, sigMerged:
		return phase, nil
	default:
		return "", fmt.Errorf("ssm: unknown merge phase %q", phase)
	}
}

// payloadKind names an event's payload for logging.
func payloadKind(ev *corev1.Event) string {
	switch ev.GetPayload().(type) {
	case *corev1.Event_ContentDelta:
		return "content_delta"
	case *corev1.Event_HeartbeatProgress:
		return "heartbeat_progress"
	case *corev1.Event_TaskProgress:
		return "task_progress"
	case *corev1.Event_Unparsed:
		return "unparsed"
	case *corev1.Event_Vendor:
		return "vendor"
	case nil:
		return "empty"
	default:
		return "other"
	}
}

// renderName is a short label for a RenderState in transition logs.
func renderName(s frontendv1.RenderState) string {
	if n, ok := frontendv1.RenderState_name[int32(s)]; ok {
		return n
	}
	return fmt.Sprintf("RenderState(%d)", int32(s))
}
