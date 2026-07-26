package ssm

import (
	"database/sql"
	"fmt"
	"log"
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"claude-repld/internal/dlog"

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
	// DBPath is the SSM database path. Empty uses defaultDBPath
	// (~/.cache/agent-repl/ssm/state.db).
	DBPath string
	// Logf is the loud transition/anomaly logger. Nil defaults to log.Printf.
	Logf dlog.Logf
	// Resolver binds session ids to workspaces. Required for Apply.
	Resolver Resolver
	// Clock returns wall-clock unix millis; injectable for tests. Nil uses
	// time.Now. The Manager enforces per-instance monotonicity on top of it
	// so the (workspace, at) primary key never collides.
	Clock func() int64
}

// subBufferSize bounds each subscriber's channel. A subscriber that falls
// this far behind is a slow consumer: the push is dropped (loudly), not
// blocked on, and the consumer recovers via a fresh Snapshot.
const subBufferSize = 64

// Manager is the session-state manager. All mutations funnel through mu, so
// the SELECT-then-INSERT idempotency check and the monotonic clock are
// race-free with a single writer connection.
type Manager struct {
	db       *sql.DB
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
	subs      map[int]chan *frontendv1.WorkspaceState
	nextSub   int
	closed    bool
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
	logf := opts.Logf
	if logf == nil {
		logf = log.Printf
	}
	clock := opts.Clock
	if clock == nil {
		clock = func() int64 { return time.Now().UnixMilli() }
	}
	db, err := openDB(path)
	if err != nil {
		return nil, err
	}
	m := &Manager{
		db:        db,
		logf:      logf,
		resolver:  opts.Resolver,
		clock:     clock,
		last:      make(map[string]frontendv1.RenderState),
		lastTasks: make(map[string]int64),
		subs:      make(map[int]chan *frontendv1.WorkspaceState),
	}
	if err := m.warm(); err != nil {
		db.Close()
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
	if err := m.db.QueryRow(`SELECT MAX(at) FROM workspace_state`).Scan(&maxAt); err != nil {
		return fmt.Errorf("ssm: warm read max(at): %w", err)
	}
	if maxAt.Valid {
		m.lastAt = maxAt.Int64
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
		if r.found {
			m.last[ws] = r.state
			m.lastTasks[ws] = r.liveTaskCount
			restored++
		}
	}
	if restored > 0 {
		m.logf("ssm: restored %d workspace(s) from %s", restored, "state log")
	}
	return nil
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
// payloads: session started/ended, turn started/ended (agent axis), task
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

	// NO-REGRESS GUARD. A readiness assertion may never knock an ACTIVE turn
	// back to a settled state.
	//
	// SessionStarted is now emitted at the SHIM's own readiness rather than
	// off the vendor's first `system:init`, which means it can legitimately
	// arrive while a turn is already running — a shim relaunch, a revive, a
	// re-handshake. The agent axis resolves on its LATEST row, so appending
	// `ready` underneath a live `thinking` would resolve the workspace green
	// while the agent was mid-turn. That regression was observed directly
	// (a THINKING→IDLE flip at 01:24:20 in the readiness logs).
	//
	// Readiness is a floor, not a transition: it says "the route works",
	// which a running turn already proves more strongly. So the row is
	// dropped rather than appended, and the running turn stands. The turn's
	// own TurnEnded still moves the workspace off thinking normally.
	if state == sigReady {
		active, err := turnActive(m.db, ws)
		if err != nil {
			return err
		}
		if active {
			m.logf("ssm: readiness suppressed (turn in flight) ws=%s session=%s seq=%d — the running turn is the stronger claim",
				ws, sid, ev.GetSeq())
			return nil
		}
	}

	at := m.nextAt()
	if err := appendRow(m.db, ws, sid, state, causeKind, sql.NullInt64{Int64: int64(ev.GetSeq()), Valid: true}, at, taskIDOf(ev)); err != nil {
		return err
	}

	// The VENDOR axis, written alongside the agent axis rather than instead
	// of it. A turn that concluded abnormally is two facts at once — the
	// turn ended, AND something only a human or the vendor can release
	// stopped it — and collapsing them into one row loses whichever is not
	// written. Keeping both means clearing the block reveals the settled
	// turn underneath instead of leaving the workspace with no state.
	if token, cause, blocked := vendorBlockOf(ev); blocked {
		if err := appendRow(m.db, ws, sid, token, cause,
			sql.NullInt64{Int64: int64(ev.GetSeq()), Valid: true}, m.nextAt(), ""); err != nil {
			return err
		}
		causeKind = cause
	}
	return m.reresolveLocked(ws, causeKind, ev.GetSeq())
}

// vendorBlockOf reports the vendor-axis row an event implies, if any.
//
// Only turn-CONCLUDING events produce one. A tool returning non-zero, or an
// API call the SDK retries past, is not a conclusion: the turn is still in
// flight, so the workspace stays red and nothing here fires.
//
// A CLEAN conclusion produces the clearing token rather than nothing, so a
// session that recovers stops reading as blocked. That is the only evidence
// the daemon accepts for a release — our own retries never write it, because
// retrying is not evidence the vendor unblocked.
func vendorBlockOf(ev *corev1.Event) (token, causeKind string, blocked bool) {
	te, ok := ev.GetPayload().(*corev1.Event_TurnEnded)
	if !ok {
		return "", "", false
	}
	if VendorBlockingTurnEnd(te.TurnEnded.GetStopReason(), te.TurnEnded.GetIsError()) {
		return sigVendorBlocked, causeVendorBlocked, true
	}
	return sigVendorClear, causeVendorCleared, true
}

// ApplyPaintAck records a frontend's attestation that it painted the
// workspace's conversation through THROUGHSEQ.
//
// The daemon tracks STATE; a frontend decides when that state is
// RENDERABLE. Nothing here can distinguish a webview that drew the history
// from one that received it and drew nothing, so the workspace stays on the
// unpainted (blue) token until a frontend says otherwise.
//
// Versioned by THROUGHSEQ: an ack that does not advance the watermark is
// dropped, so an ack minted before a route break cannot re-green the
// workspace after it, and a slow frontend's stale ack cannot green a gap a
// faster one already reported. A seq of 0 is a REAL attestation of an empty
// history, which is what lets a never-prompted session reach green.
func (m *Manager) ApplyPaintAck(workspace string, throughSeq uint64) error {
	if workspace == "" {
		return fmt.Errorf("ssm: ApplyPaintAck got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()

	prev, attested, err := paintWatermark(m.db, workspace)
	if err != nil {
		return err
	}
	if attested && throughSeq <= prev {
		m.logf("ssm: paint ack superseded ws=%s through_seq=%d watermark=%d — attestation unchanged",
			workspace, throughSeq, prev)
		return nil
	}

	at := m.nextAt()
	if err := appendRow(m.db, workspace, "", sigPainted, causePaintAck,
		sql.NullInt64{Int64: int64(throughSeq), Valid: true}, at, ""); err != nil {
		return err
	}
	return m.reresolveLocked(workspace, causePaintAck, throughSeq)
}

// ApplyPaintLost withdraws a workspace's paint attestation because the route
// broke — a shim death, a hibernation, a frontend disconnect.
//
// Disconnect resolves BLUE, never to a terminal color: a workspace whose
// session went away is not "done", it is unreachable, and the next frontend
// to attach must re-attest before green can be claimed again.
func (m *Manager) ApplyPaintLost(workspace, reason string) error {
	if workspace == "" {
		return fmt.Errorf("ssm: ApplyPaintLost got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()

	at := m.nextAt()
	cause := causePaintLost
	if reason != "" {
		cause = causePaintLost + ":" + reason
	}
	if err := appendRow(m.db, workspace, "", sigUnpainted, cause, sql.NullInt64{}, at, ""); err != nil {
		return err
	}
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
// STATE is the sessiondrv token ("pending" | "done" | "failed"). `pending` is
// deliberately NOT blue on its own: a session mid-backfill is covered by the
// paint axis (no frontend has attested yet), and treating pending as a
// separate blue would mean a REOPENED session whose history is already in the
// store — and which therefore never emits a fresh transition — could never
// leave it. See sessiondrv.settleBackfillFromStore for the other half of that.
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

// ApplyVendorCleared releases a vendor/account block because the vendor or a
// human released it — a clean turn conclusion, or a clean auth report.
//
// Our own retries never call this: retrying is not evidence the vendor
// unblocked, and treating it as such is how a blocked session comes to look
// available.
func (m *Manager) ApplyVendorCleared(workspace, reason string) error {
	if workspace == "" {
		return fmt.Errorf("ssm: ApplyVendorCleared got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()

	at := m.nextAt()
	cause := causeVendorCleared
	if reason != "" {
		cause = causeVendorCleared + ":" + reason
	}
	if err := appendRow(m.db, workspace, "", sigVendorClear, cause, sql.NullInt64{}, at, ""); err != nil {
		return err
	}
	return m.reresolveLocked(workspace, cause, 0)
}

// ApplyConnectionDegraded records the daemon's OWN observation of the shim
// transport going quiet, or coming back (F4).
//
// The degraded AXIS already existed, fed by the shim's own DegradedState
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
		return nil
	}
	old, had := m.last[workspace]
	oldTasks, hadTasks := m.lastTasks[workspace]
	stateMoved := !had || old != r.state
	tasksMoved := !hadTasks || oldTasks != r.liveTaskCount
	if !stateMoved && !tasksMoved {
		return nil // nothing visible changed; stay quiet (§12: log deltas only)
	}
	m.last[workspace] = r.state
	m.lastTasks[workspace] = r.liveTaskCount

	if stateMoved {
		oldName := "∅"
		if had {
			oldName = renderName(old)
		}
		// §12 SSM contract: every transition logged old→new + cause kind + seq.
		m.logf("ssm: transition ws=%s %s→%s cause_kind=%s cause_seq=%d turn_active=%t live_tasks=%d merge=%q",
			workspace, oldName, renderName(r.state), causeKind, causeSeq, r.turnActive, r.liveTaskCount, r.mergePhase)
	} else {
		m.logf("ssm: live_tasks ws=%s %d→%d cause_kind=%s cause_seq=%d state=%s",
			workspace, oldTasks, r.liveTaskCount, causeKind, causeSeq, renderName(r.state))
	}

	m.pushLocked(workspace, r)
	return nil
}

// pushLocked broadcasts a WorkspaceState to every subscriber. A full
// subscriber channel is a slow consumer: the update is dropped loudly (the
// consumer recovers from a subsequent Snapshot), never blocked on.
func (m *Manager) pushLocked(workspace string, r resolved) {
	if len(m.subs) == 0 {
		return
	}
	msg := r.toProto(workspace)
	for id, ch := range m.subs {
		select {
		case ch <- msg:
		default:
			m.logf("ssm: subscriber %d slow; dropped ws=%s state=%s (will resync via Snapshot)", id, workspace, renderName(r.state))
		}
	}
}

// Current returns the resolved WorkspaceState for a workspace. found is
// false when the workspace has no render-bearing signals (unborn / only
// counters) — an explicit miss, not a silent default.
func (m *Manager) Current(workspace string) (*frontendv1.WorkspaceState, bool, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	r, err := resolve(m.db, workspace, m.logf)
	if err != nil {
		return nil, false, err
	}
	if !r.found {
		return nil, false, nil
	}
	return r.toProto(workspace), true, nil
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
		if r.found {
			out = append(out, r.toProto(ws))
		}
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
	for id, ch := range m.subs {
		delete(m.subs, id)
		close(ch)
	}
	m.mu.Unlock()
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
		// ALWAYS `done` on the agent axis: the turn ended, and that is a
		// fact about the agent regardless of why. An abnormal conclusion
		// ADDITIONALLY blocks the vendor axis (see vendorBlockOf), which
		// outranks done — so the workspace reads purple while the block
		// stands and falls back to the green underneath once it clears,
		// rather than being left with no state at all.
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
