package ssm

import (
	"database/sql"
	"fmt"
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"claude-repld/internal/dlog"
	"claude-repld/internal/workspace/merge"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// Resolver maps a session id to the workspace it is bound to. The binding
// lives in the daemon's session registry (the stitch phase wires the real
// one); the SSM takes it injected so it stays free of registry knowledge.
type Resolver interface {
	// Workspace returns the workspace bound to sessionID, and whether a
	// binding exists.
	Workspace(sessionID string) (string, bool)
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
	ownsDB   bool
	logf     dlog.Logf
	resolver Resolver
	clock    func() int64

	mu     sync.Mutex
	lastAt int64
	last   map[string]frontendv1.RenderState // last-resolved state per workspace
	// lastTasks is the last-pushed live_task_count per workspace. The count is
	// an INPUT the frontend renders (the footer's live-task figure, sourced via
	// progress.ApplyWorkspaceState), so it can move while the render state does
	// not — 5 background tasks becoming 2 leaves a workspace idle_async either
	// way. Keying the push on state alone left that change unpushed, which is
	// how a swept ghost count stayed on screen.
	lastTasks map[string]int64
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
	// mergeQueue is the merge subsystem's queue, bound by ssm.NewMergeLease.
	// It is the sole source of merge_queue_position / merge_queue_depth, which
	// are cross-workspace facts no per-workspace row can carry.
	mergeQueue merge.Queue
	subs       map[int]chan *frontendv1.WorkspaceState
	nextSub    int
	closed     bool
}

// Open opens the SSM database and warms the last-resolved cache from the
// persisted log so a reopen does not re-announce every workspace as a fresh
// transition (state survives the reopen).
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
	clock := opts.Clock
	if clock == nil {
		clock = func() int64 { return time.Now().UnixMilli() }
	}
	db, ownsDB := opts.DB, false
	if db == nil {
		opened, err := openDB(path)
		if err != nil {
			return nil, err
		}
		db, ownsDB = opened, true
	} else if err := migrate(db); err != nil {
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
		db:              db,
		ownsDB:          ownsDB,
		logf:            logf,
		resolver:        opts.Resolver,
		clock:           clock,
		last:            make(map[string]frontendv1.RenderState),
		lastTasks:       make(map[string]int64),
		interruptedTurn: make(map[string]*interruptMark),
		mergeLeases:     make(map[string][]leaseWindow),
		clearingTimers:  make(map[string]Timer),
		clearingTimeout: clearingTimeout,
		afterFunc:       afterFunc,
		subs:            make(map[int]chan *frontendv1.WorkspaceState),
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
	// Before the cache is seeded, so a released row is what gets restored
	// rather than the stale one it replaces.
	if err := m.releasePersistedPermissionsLocked(); err != nil {
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
		if ev.GetPlane() != corev1.Plane_PLANE_STREAM {
			return fmt.Errorf("ssm: rejected non-authoritative turn lifecycle plane=%s session=%s seq=%d kind=%s turn_id=%q request_id=%q dedup_key=%q",
				ev.GetPlane().String(), sid, ev.GetSeq(), payloadKind(ev),
				turnCorrelation(ev), ev.GetRequestId(), ev.GetDedupKey())
		}
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
	ws, bound := m.resolver.Workspace(sid)
	if !bound {
		return fmt.Errorf("ssm: no workspace bound to session %s (kind=%s seq=%d)", sid, causeKind, ev.GetSeq())
	}

	m.mu.Lock()
	defer m.mu.Unlock()

	// Idempotency: a replayed event (same session+seq) makes no change.
	applied, err := seqApplied(m.db, sid, ev.GetSeq())
	if err != nil {
		return err
	}
	if applied {
		m.logf("ssm: duplicate event skipped kind=%s session=%s seq=%d ws=%s", causeKind, sid, ev.GetSeq(), ws)
		return nil
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
		case active && (claimant == "" || claimant == sid):
			m.logf("ssm: readiness suppressed (turn in flight) ws=%s session=%s seq=%d — the running turn is the stronger claim",
				ws, sid, ev.GetSeq())
			return nil
		case active:
			m.logf("ssm: turn claim INVALIDATED ws=%s stale_session=%s new_session=%s seq=%d — the session holding `thinking` no longer drives this workspace and can never report that turn's end, so readiness supersedes it rather than being suppressed by it",
				ws, claimant, sid, ev.GetSeq())
		}
	}

	causeSeq := sql.NullInt64{Int64: int64(ev.GetSeq()), Valid: true}
	if state == sigTaskEnded {
		if err := m.appendTaskEndLocked(ws, sid, causeKind, causeSeq, taskIDOf(ev)); err != nil {
			return err
		}
	} else {
		at := m.nextAt()
		if err := appendRow(m.db, ws, sid, state, causeKind, causeSeq, at, taskIDOf(ev)); err != nil {
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
	m.interruptedTurn[workspace] = &interruptMark{tolerateLateStart: !active}
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

	// A COMPACTION DOES NOT SURVIVE THE ROTATION that ends its identity: its
	// ContextCompacted belongs to the retired session and will never arrive,
	// exactly as the in-flight turn's end will not. THE CLEARING AXIS IS LEFT
	// ALONE for the opposite reason — a `/clear` CAUSES this rotation, and the
	// ContextCleared it is waiting for is produced under the NEW identity.
	m.closeCompactingLocked(workspace, causeSessionRotated)

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
	if err := appendRow(m.db, workspace, "", sigIdle, cause, sql.NullInt64{}, m.nextAt(), ""); err != nil {
		return err
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
	if err := appendRow(m.db, workspace, staleSessionID, sigIdle, cause, sql.NullInt64{}, m.nextAt(), ""); err != nil {
		return err
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
// (merging|merge_queued|merge_conflict|merge_failed|merged) or the empty
// string / merge_none to clear the merge axis. cause is a short human note
// recorded as the cause kind's detail; it never carries a store seq.
func (m *Manager) ApplyMergeTransition(workspace, phase, cause string) error {
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

	m.mu.Lock()
	defer m.mu.Unlock()

	at := m.nextAt()
	// Daemon-local: no store seq, cause_seq stays NULL, and a merge is not a
	// task so it carries no task id.
	if err := appendRow(m.db, workspace, "", token, causeKind, sql.NullInt64{}, at, ""); err != nil {
		return err
	}
	return m.reresolveLocked(workspace, causeKind, 0)
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
	if !stateMoved && !tasksMoved {
		return nil // nothing visible changed; stay quiet (§12: log deltas only)
	}
	m.last[workspace] = msg.GetState()
	m.lastTasks[workspace] = msg.GetLiveTaskCount()

	if stateMoved {
		oldName := "∅"
		if had {
			oldName = renderName(old)
		}
		// §12 SSM contract: every transition logged old→new + cause kind + seq.
		m.logf("ssm: transition ws=%s %s→%s cause_kind=%s cause_seq=%d turn_active=%t live_tasks=%d merge=%q",
			workspace, oldName, renderName(msg.GetState()), causeKind, causeSeq, msg.GetTurnActive(), msg.GetLiveTaskCount(), msg.GetMergePhase())
	} else {
		m.logf("ssm: live_tasks ws=%s %d→%d cause_kind=%s cause_seq=%d state=%s",
			workspace, oldTasks, msg.GetLiveTaskCount(), causeKind, causeSeq, renderName(msg.GetState()))
	}

	return m.pushMessageLocked(workspace, msg)
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
	var msg *frontendv1.WorkspaceState
	switch {
	case !found || composite.LifecycleTop == "":
		msg = r.toProto(workspace)
	default:
		msg, err = compositeWorkspaceState(workspace, r, composite)
		if err != nil {
			return nil, err
		}
	}
	m.stampMergeFactsLocked(workspace, msg)
	return msg, nil
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
	r, err := resolve(m.db, workspace, m.logf)
	if err != nil {
		return nil, false, err
	}
	composite, compositeFound, err := resolveComposite(m.db, workspace)
	if err != nil {
		return nil, false, err
	}
	if !r.found && (!compositeFound || composite.LifecycleTop == "") {
		return nil, false, nil
	}
	if !r.found {
		r = resolved{found: true, state: frontendv1.RenderState_RENDER_STATE_UNSPECIFIED}
	}
	msg, err := m.workspaceMessageLocked(workspace, r)
	if err != nil {
		return nil, false, err
	}
	return msg, true, nil
}

// Snapshot returns the current WorkspaceState of every workspace with a
// resolved render state, in stable workspace order (for a frontend resync).
func (m *Manager) Snapshot() ([]*frontendv1.WorkspaceState, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	names, err := distinctWorkspaces(m.db)
	if err != nil {
		return nil, err
	}
	out := make([]*frontendv1.WorkspaceState, 0, len(names))
	for _, ws := range names {
		r, err := resolve(m.db, ws, m.logf)
		if err != nil {
			return nil, err
		}
		composite, compositeFound, err := resolveComposite(m.db, ws)
		if err != nil {
			return nil, err
		}
		if !r.found && (!compositeFound || composite.LifecycleTop == "") {
			continue
		}
		if !r.found {
			r = resolved{found: true, state: frontendv1.RenderState_RENDER_STATE_UNSPECIFIED}
		}
		msg, err := m.workspaceMessageLocked(ws, r)
		if err != nil {
			return nil, err
		}
		out = append(out, msg)
	}
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
		MergePhase:    r.mergePhase,
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
	case sigMerging, sigMergeQueued, sigMergeConflict, sigMergeFailed, sigMerged:
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
