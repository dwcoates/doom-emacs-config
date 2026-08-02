package ssm

import (
	"context"
	"database/sql"
	"fmt"
	"sync"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/workspace/merge"
)

// This file is the SSM's half of the merge exclusivity claim: the durable
// record of every window in which merge.Coordinator owned a workspace's shim,
// plus the two facts that record answers.
//
// IT SITS BESIDE THE TURN CLAIMS (turnclaims.go) DELIBERATELY. Both are
// exclusivity claims over the same session, both are durable so a daemon
// bounce cannot quietly drop one, and both are consulted before anything is
// allowed to submit. The difference is who they exclude: a turn claim excludes
// a second turn, and a merge lease excludes the USER.
//
// A LEASE IS A WINDOW, NOT A FLAG. The ledger keeps every window it has ever
// opened, closed ones included, because conversation provenance is decided by
// asking which window an item's timestamp falls in. That is what makes the
// verdict REPRODUCIBLE: a resync or a transcript replay run long after the
// merge has finished reaches exactly the same answer as the live push, instead
// of re-deriving provenance from a lease that has since been released.
//
// Every error here is recorded through Manager.logf with the workspace, the
// operation, and the concrete cause, the same canonical path the turn ledger
// and the connectivity axis use.

// SessionAuthority is the merge subsystem's whole authority over a workspace's
// session lifecycle: it stops the turn a lease displaces at the START of a
// merge, and stands the session down at the END of one. Satisfied by
// *sessioncontroller.Manager.
//
// THE TWO ARE ONE PORT DELIBERATELY. Both are the merge reaching into a single
// workspace's session, and both must reach the SAME controller fleet — the one
// whose shim the lease was taken over. Two injection points could be bound to
// two fleets, so a merge would interrupt one daemon's session and stop
// another's; one port makes that unrepresentable rather than merely unlikely.
//
// The interrupt half is a MACHINERY stop, not a user-commanded one: the user
// did not ask for their turn to end, the merge needs the session, so the turn's
// outcome must not be painted `interrupted`. That distinction is why this is
// its own port rather than the frontend interrupt command's entry point.
type SessionAuthority interface {
	// InterruptForMerge delivers the stop and reports a failure to deliver it.
	InterruptForMerge(ctx context.Context, workspace string) error
	MergedTeardown
}

// MergeLeaseConfig constructs a MergeLease. EVERY field is required: a lease
// that cannot interrupt the turn it is displacing, or that cannot see the
// queue it is draining, would be a lease that silently does less than it
// claims, so a nil is a hard construction error rather than a defaulted no-op.
type MergeLeaseConfig struct {
	// Manager owns the durable ledger the lease is written to. Required.
	Manager *Manager
	// Queue is the merge subsystem's durable per-repository request channel.
	// It is the ONLY source of merge_queue_position / merge_queue_depth: those
	// are cross-workspace facts, and no per-workspace row can carry them.
	// Required.
	Queue merge.Queue
	// Interrupter is the merge's authority over the workspace's session: it
	// stops the USER turn the lease displaces, and stands the session down once
	// the merge has landed. Required.
	Interrupter SessionAuthority
}

// MergeLease is the SSM's implementation of merge.Lease.
type MergeLease struct {
	m           *Manager
	interrupter SessionAuthority
}

// The frozen contract is the compile-time obligation, not a comment.
var _ merge.Lease = (*MergeLease)(nil)

// NewMergeLease validates cfg, binds the merge queue to the manager so the
// pushed WorkspaceState can carry the queue facts, and returns the lease.
func NewMergeLease(cfg MergeLeaseConfig) (*MergeLease, error) {
	if cfg.Manager == nil {
		return nil, fmt.Errorf("ssm: MergeLeaseConfig.Manager is required")
	}
	if cfg.Queue == nil {
		return nil, fmt.Errorf("ssm: MergeLeaseConfig.Queue is required; merge_queue_position and merge_queue_depth have no other source")
	}
	if cfg.Interrupter == nil {
		return nil, fmt.Errorf("ssm: MergeLeaseConfig.Interrupter is required; a lease that cannot interrupt the turn it displaces would leave the user driving the shim it claims to own")
	}
	if err := cfg.Manager.bindMergeQueue(cfg.Queue); err != nil {
		return nil, err
	}
	// The SAME authority drives the merged transition's teardown. Binding it
	// here rather than accepting a second config field is what keeps the
	// interrupt and the stand-down provably aimed at one controller fleet.
	if err := cfg.Manager.bindMergedTeardown(cfg.Interrupter); err != nil {
		return nil, err
	}
	return &MergeLease{m: cfg.Manager, interrupter: cfg.Interrupter}, nil
}

// Acquire takes the lease for ws and returns the release func.
//
// THE CLAIM IS TAKEN BEFORE THE INTERRUPT, and the order is load-bearing. The
// claim is what makes a second acquisition impossible, so taking it first means
// a losing caller stops nobody's turn; interrupting first would stop a live
// user turn on behalf of a merge that then discovers it never held the shim.
//
// An interrupt that cannot be delivered ROLLS THE CLAIM BACK. A lease whose
// displaced turn is still running is a lease that does not exclude what it
// exists to exclude, so the honest outcome is no lease at all and a loud error.
func (l *MergeLease) Acquire(ctx context.Context, ws string) (func(), error) {
	if ws == "" {
		err := fmt.Errorf("ssm: merge lease Acquire got an empty workspace")
		l.m.logf("ssm: merge lease decision=reject_validation workspace=%q error=%v", ws, err)
		return nil, err
	}
	at, err := l.m.openMergeLease(ws)
	if err != nil {
		return nil, err
	}
	if err := l.interrupter.InterruptForMerge(ctx, ws); err != nil {
		wrapped := fmt.Errorf("ssm: merge lease for workspace %q could not interrupt the in-flight turn: %w", ws, err)
		l.m.logf("ssm: merge lease decision=rollback workspace=%s acquired_at=%d error=%v — the displaced turn is still running, so the claim is withdrawn rather than held over a shim the user still drives",
			ws, at, err)
		if closeErr := l.m.closeMergeLease(ws); closeErr != nil {
			l.m.logf("ssm: merge lease ROLLBACK FAILED workspace=%s acquired_at=%d: %v — an open window remains on the ledger and will keep refusing user prompts until it is released",
				ws, at, closeErr)
		}
		return nil, wrapped
	}
	l.m.logf("ssm: merge lease decision=acquire workspace=%s acquired_at=%d — the merge owns this shim; user prompts are refused and every conversation item is stamped CONVERSATION_SOURCE_MERGE until release",
		ws, at)

	var once sync.Once
	return func() {
		once.Do(func() {
			if err := l.m.closeMergeLease(ws); err != nil {
				l.m.logf("ssm: merge lease RELEASE FAILED workspace=%s acquired_at=%d: %v — the window stays open and user prompts stay refused",
					ws, at, err)
				return
			}
			l.m.logf("ssm: merge lease decision=release workspace=%s acquired_at=%d — the shim is handed back to the user", ws, at)
		})
	}, nil
}

// Held reports whether the lease on ws is currently held.
func (l *MergeLease) Held(ws string) bool { return l.m.MergeLeaseHeld(ws) }

// leaseWindow is one merge exclusivity window, warmed from the ledger. An open
// window has releasedAt == 0.
type leaseWindow struct {
	acquiredAt int64
	releasedAt int64
}

// contains reports whether tsMs falls inside the window. The acquiring edge is
// INCLUSIVE and the releasing edge EXCLUSIVE, so an item produced in the same
// millisecond the shim was handed back belongs to the user who got it back.
func (w leaseWindow) contains(tsMs int64) bool {
	if tsMs < w.acquiredAt {
		return false
	}
	return w.releasedAt == 0 || tsMs < w.releasedAt
}

// bindMergeQueue attaches the merge subsystem's queue, the sole source of the
// cross-workspace queue facts. Binding twice is a wiring bug: two queues would
// give two answers to one question, so the second is refused loudly.
func (m *Manager) bindMergeQueue(q merge.Queue) error {
	if q == nil {
		return fmt.Errorf("ssm: bindMergeQueue got a nil merge.Queue")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.mergeQueue != nil {
		return fmt.Errorf("ssm: a merge.Queue is already bound; a second one would make merge_queue_position ambiguous")
	}
	m.mergeQueue = q
	return nil
}

// openMergeLease writes the opening edge of a lease window and re-pushes the
// workspace so merge_lease_held reaches the frontends immediately.
//
// AN ORPHANED OPEN WINDOW IS ADOPTED, NOT AN ERROR. The ledger deliberately
// keeps open windows across a daemon bounce (warmMergeLeasesLocked), and the
// merge that reopens the workspace — Drain resuming the durable queue head, or
// the user re-issuing the merge — must become the window's holder. Before this,
// the resume path INSERTed a second window and wedged forever on the unique
// open-window index: the very lease the restart preserved refused every merge
// that could have released it. A live in-process holder still rejects a second
// acquire — that is the double-merge protection the index exists for.
func (m *Manager) openMergeLease(workspace string) (int64, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	at := m.nextAt()
	if open := m.openMergeWindowsLocked(workspace); len(open) > 0 {
		if m.mergeLeaseHolders[workspace] {
			err := fmt.Errorf("ssm: open merge lease for workspace %q: its open window is already held by a live merge in this process", workspace)
			m.logf("ssm: merge lease decision=reject workspace=%s at=%d open_windows=%d error=%v",
				workspace, at, len(open), err)
			return 0, err
		}
		// The window's holder died with a prior daemon, so no release can ever
		// arrive for it. Adopting the window — rather than closing and
		// reopening it — keeps the ledger honest: the shim never left merge
		// ownership (prompts were refused across the whole bounce), so
		// provenance keeps stamping the gap CONVERSATION_SOURCE_MERGE.
		m.mergeLeaseHolders[workspace] = true
		m.logf("ssm: merge lease decision=adopt workspace=%s acquired_at=%d at=%d — the open window was orphaned by a daemon restart and this merge resumes as its holder",
			workspace, open[0].acquiredAt, at)
		if err := m.pushCurrentLocked(workspace); err != nil {
			return 0, err
		}
		return open[0].acquiredAt, nil
	}
	if _, err := m.db.Exec(
		`INSERT INTO merge_lease(workspace, acquired_at) VALUES (?,?)`,
		workspace, at,
	); err != nil {
		// The unique partial index stays as defense in depth under the
		// in-process holder check: it is what turns a racing double-acquire
		// into this error rather than into two coordinators believing they
		// own one shim.
		wrapped := fmt.Errorf("ssm: open merge lease for workspace %q: %w", workspace, err)
		m.logf("ssm: merge lease decision=reject workspace=%s at=%d open_windows=%d error=%v",
			workspace, at, len(m.openMergeWindowsLocked(workspace)), err)
		return 0, wrapped
	}
	m.mergeLeases[workspace] = append(m.mergeLeases[workspace], leaseWindow{acquiredAt: at})
	m.mergeLeaseHolders[workspace] = true
	if err := m.pushCurrentLocked(workspace); err != nil {
		return 0, err
	}
	return at, nil
}

// closeMergeLease writes the releasing edge of the workspace's open window.
// An absent open window is an error, never a no-op: it means the release was
// called on a lease this ledger never recorded, and answering "fine" would let
// a user-refusing window elsewhere stand unnoticed.
func (m *Manager) closeMergeLease(workspace string) error {
	m.mu.Lock()
	defer m.mu.Unlock()
	at := m.nextAt()
	res, err := m.db.Exec(
		`UPDATE merge_lease SET released_at=? WHERE workspace=? AND released_at IS NULL`,
		at, workspace,
	)
	if err != nil {
		m.logf("ssm: merge lease close FAILED workspace=%s at=%d error=%v", workspace, at, err)
		return fmt.Errorf("ssm: close merge lease for workspace %q: %w", workspace, err)
	}
	closed, err := res.RowsAffected()
	if err != nil {
		m.logf("ssm: merge lease close inspection FAILED workspace=%s at=%d error=%v", workspace, at, err)
		return fmt.Errorf("ssm: inspect merge lease close for workspace %q: %w", workspace, err)
	}
	if closed != 1 {
		err := fmt.Errorf("ssm: merge lease close for workspace %q affected %d windows, want exactly one", workspace, closed)
		m.logf("ssm: merge lease close decision=reject workspace=%s at=%d closed_windows=%d error=%v",
			workspace, at, closed, err)
		return err
	}
	windows := m.mergeLeases[workspace]
	for i := range windows {
		if windows[i].releasedAt == 0 {
			windows[i].releasedAt = at
		}
	}
	delete(m.mergeLeaseHolders, workspace)
	return m.pushCurrentLocked(workspace)
}

// MergeLeaseHeld reports whether merge.Coordinator currently owns workspace's
// shim. It is what the prompt path consults to refuse a user submission and
// what resolves merge_lease_held on the pushed WorkspaceState.
func (m *Manager) MergeLeaseHeld(workspace string) bool {
	m.mu.Lock()
	defer m.mu.Unlock()
	return len(m.openMergeWindowsLocked(workspace)) > 0
}

// ConversationSourceAt returns the PERSISTED provenance verdict for an item
// produced in workspace at tsMs.
//
// The answer is read off the durable lease ledger rather than off the lease's
// current state, which is the whole reason the ledger keeps closed windows: a
// resync replaying a merge's conversation an hour later must reproduce
// CONVERSATION_SOURCE_MERGE, and asking a released lease would answer
// CONVERSATION_SOURCE_USER and rewrite history.
//
// UNSPECIFIED is never returned with a nil error. An item the ledger cannot
// place in time is a producer bug, not an item to guess about, so it surfaces
// as an error and the caller refuses the frame.
func (m *Manager) ConversationSourceAt(workspace string, tsMs int64) (frontendv1.ConversationSource, error) {
	if workspace == "" {
		err := fmt.Errorf("ssm: conversation source lookup got an empty workspace (ts_ms=%d)", tsMs)
		m.logf("ssm: conversation source decision=reject_validation workspace=%q ts_ms=%d error=%v", workspace, tsMs, err)
		return frontendv1.ConversationSource_CONVERSATION_SOURCE_UNSPECIFIED, err
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	windows := m.mergeLeases[workspace]
	if len(windows) == 0 {
		// A workspace merge.Coordinator has never held is USER at every instant
		// there is, so the timestamp cannot change the answer and is not
		// required to produce it. This is the overwhelmingly common case, and
		// short-circuiting it keeps a missing ts_ms from being fatal anywhere
		// except where it could actually alter a verdict.
		return frontendv1.ConversationSource_CONVERSATION_SOURCE_USER, nil
	}
	if tsMs <= 0 {
		err := fmt.Errorf("ssm: conversation source for workspace %q got a non-positive ts_ms=%d over a ledger holding %d merge lease window(s); the item cannot be placed against them", workspace, tsMs, len(windows))
		m.logf("ssm: conversation source decision=reject_validation workspace=%q ts_ms=%d windows=%d error=%v",
			workspace, tsMs, len(windows), err)
		return frontendv1.ConversationSource_CONVERSATION_SOURCE_UNSPECIFIED, err
	}
	for _, w := range windows {
		if w.contains(tsMs) {
			return frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE, nil
		}
	}
	return frontendv1.ConversationSource_CONVERSATION_SOURCE_USER, nil
}

// openMergeWindowsLocked returns the workspace's open lease windows. Caller
// holds mu. More than one is impossible by the unique partial index; the slice
// is returned rather than a bool so a violation is countable in a log line.
func (m *Manager) openMergeWindowsLocked(workspace string) []leaseWindow {
	var open []leaseWindow
	for _, w := range m.mergeLeases[workspace] {
		if w.releasedAt == 0 {
			open = append(open, w)
		}
	}
	return open
}

// warmMergeLeasesLocked seeds the in-memory ledger projection from the durable
// table at Open. Caller holds mu.
//
// The projection exists so both readers of the ledger are TOTAL: Held has no
// error channel (the frozen contract fixes its signature) and the provenance
// lookup runs once per conversation item during a backfill of thousands. A
// workspace accumulates one window per merge it has ever run, so the whole
// ledger is small enough to hold.
func (m *Manager) warmMergeLeasesLocked() error {
	rows, err := m.db.Query(
		`SELECT workspace, acquired_at, released_at FROM merge_lease ORDER BY lease_id`)
	if err != nil {
		return fmt.Errorf("ssm: warm merge lease ledger: %w", err)
	}
	defer rows.Close()
	open := 0
	for rows.Next() {
		var ws string
		var acquiredAt int64
		var releasedAt sql.NullInt64
		if err := rows.Scan(&ws, &acquiredAt, &releasedAt); err != nil {
			return fmt.Errorf("ssm: scan merge lease window: %w", err)
		}
		w := leaseWindow{acquiredAt: acquiredAt}
		if releasedAt.Valid {
			w.releasedAt = releasedAt.Int64
		} else {
			open++
		}
		m.mergeLeases[ws] = append(m.mergeLeases[ws], w)
	}
	if err := rows.Err(); err != nil {
		return fmt.Errorf("ssm: iterate merge lease ledger: %w", err)
	}
	if open > 0 {
		// NOT CLEARED AT BOOT, unlike the connectivity axis. A held lease
		// outliving a daemon bounce is the contract: merge.Coordinator.Drain
		// reconstructs the merge behind it, and clearing it here would re-open
		// the workspace to user prompts while its cherry-pick resumes.
		m.logf("ssm: merge lease ledger restored with %d OPEN window(s) — those workspaces stay closed to user prompts until merge.Coordinator.Drain resumes and releases them", open)
	}
	return nil
}

// mergeQueueFactsLocked resolves the workspace's position on and the depth of
// its repository's merge queue. Caller holds mu.
//
// Both are ZERO when the workspace is not enqueued, which is what the frozen
// contract defines index 0 to mean. A daemon assembled without a merge
// subsystem has no queue at all, so nothing can be enqueued on it and zero is
// the true answer there too — but a workspace holding a LEASE with no queue
// bound is a wiring bug (only NewMergeLease can produce a lease, and it binds
// the queue), so that combination is logged rather than passed over.
func (m *Manager) mergeQueueFactsLocked(workspace string) (position, depth int32) {
	if m.mergeQueue == nil {
		if len(m.openMergeWindowsLocked(workspace)) > 0 {
			m.logf("ssm: INVARIANT VIOLATION ws=%s holds a merge lease with NO merge.Queue bound — merge_queue_position and merge_queue_depth are reported as 0 for a workspace that is demonstrably merging",
				workspace)
		}
		return 0, 0
	}
	for _, requests := range m.mergeQueue.Snapshot() {
		for i, req := range requests {
			if req.Workspace != workspace {
				continue
			}
			return int32(i + 1), int32(len(requests))
		}
	}
	return 0, 0
}

// stampMergeFactsLocked writes the merge fields no per-workspace state row can
// carry onto a WorkspaceState about to be pushed. Caller holds mu.
//
// merged_at_ms rides here rather than off the resolution for the same reason
// the queue facts do: it is a fact about the workspace's HISTORY, not about
// which row currently wins, so resolving it would tie a permanent instant to
// whichever transition happened to be newest.
//
// It is called from workspaceMessageLocked and nowhere else, which is what
// makes every push, snapshot and synchronous publication carry the same
// answer: one construction funnel, so a frame that omits them is
// unrepresentable rather than merely unlikely.
func (m *Manager) stampMergeFactsLocked(workspace string, msg *frontendv1.WorkspaceState) {
	position, depth := m.mergeQueueFactsLocked(workspace)
	msg.MergeQueuePosition = position
	msg.MergeQueueDepth = depth
	msg.MergeLeaseHeld = len(m.openMergeWindowsLocked(workspace)) > 0
	msg.MergedAtMs = m.mergedAtLocked(workspace)
}

// pushCurrentLocked re-resolves the workspace and pushes it unconditionally.
// Caller holds mu.
//
// reresolveLocked pushes only on a VISIBLE delta (render state or live-task
// count), and neither moves when a lease is taken or handed back. The lease is
// a pushed field of its own, so its edges need a push that does not depend on
// some other field having moved.
func (m *Manager) pushCurrentLocked(workspace string) error {
	msg, found, err := m.currentLocked(workspace)
	if err != nil {
		return err
	}
	if !found {
		// A workspace with no render-bearing signal at all. Nothing to push a
		// lease onto yet; the first resolved state carries it.
		return nil
	}
	return m.pushMessageLocked(workspace, msg)
}
