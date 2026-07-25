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

	mu      sync.Mutex
	lastAt  int64
	last    map[string]frontendv1.RenderState // last-resolved state per workspace
	subs    map[int]chan *frontendv1.WorkspaceState
	nextSub int
	closed  bool
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
		db:       db,
		logf:     logf,
		resolver: opts.Resolver,
		clock:    clock,
		last:     make(map[string]frontendv1.RenderState),
		subs:     make(map[int]chan *frontendv1.WorkspaceState),
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

	at := m.nextAt()
	if err := appendRow(m.db, ws, sid, state, causeKind, sql.NullInt64{Int64: int64(ev.GetSeq()), Valid: true}, at, taskIDOf(ev)); err != nil {
		return err
	}
	return m.reresolveLocked(ws, causeKind, ev.GetSeq())
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
func (m *Manager) reresolveLocked(workspace, causeKind string, causeSeq uint64) error {
	r, err := resolve(m.db, workspace, m.logf)
	if err != nil {
		return err
	}
	if !r.found {
		return nil
	}
	old, had := m.last[workspace]
	if had && old == r.state {
		return nil // no visible transition; stay quiet (§12: log deltas only)
	}
	m.last[workspace] = r.state

	oldName := "∅"
	if had {
		oldName = renderName(old)
	}
	// §12 SSM contract: every transition logged old→new + cause kind + seq.
	m.logf("ssm: transition ws=%s %s→%s cause_kind=%s cause_seq=%d turn_active=%t live_tasks=%d merge=%q",
		workspace, oldName, renderName(r.state), causeKind, causeSeq, r.turnActive, r.liveTaskCount, r.mergePhase)

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
		return sigIdle, causeSessionStarted, true
	case *corev1.Event_TurnStarted:
		return sigThinking, causeTurnStarted, true
	case *corev1.Event_TurnEnded:
		if ev.GetTurnEnded().GetIsError() {
			return sigStopFailed, causeTurnEnded, true
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
