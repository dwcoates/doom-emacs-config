package ssm

import (
	"context"
	"database/sql"
	"errors"
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
	//
	// It also reports WHAT it stopped. A nil DisplacedTurn means the lease
	// displaced nothing (no live session, or a turn that had already ended);
	// a non-nil one is the user's prompt that was cut, which the release path
	// hands back through ResumeDisplacedTurn.
	InterruptForMerge(ctx context.Context, workspace string) (*DisplacedTurn, error)
	// ResumeDisplacedTurn re-submits the turn the lease displaced, once the
	// lease is gone. It is an ordinary USER submission: the prompt is the
	// user's, it was never theirs to cancel, and the conversation it lands in
	// is out from under the merge.
	ResumeDisplacedTurn(ctx context.Context, workspace string, turn DisplacedTurn) error
	MergedTeardown
}

// DisplacedTurn is the user turn a merge lease's interrupt stopped.
//
// IT EXISTS BECAUSE THE STOP IS NOT THE USER'S. The lease's interrupt is
// machinery: the user asked for work and a merge took the shim away
// mid-sentence. Before this the turn simply ended, the workspace went quiet,
// and the user's only sign was the refusal text telling them to resubmit once
// the merge finished — a manual step for something the daemon knew everything
// about.
//
// IT IS DURABLE, and that is the whole reason it rides the lease LEDGER rather
// than a field on the live session controller. A workspace merge of this repo
// bounces the daemon while the lease is still open — the ledger keeps that
// window on purpose (warmMergeLeasesLocked) and the resuming coordinator adopts
// it (openMergeLease) — so anything about the displaced turn held only in
// memory is exactly what the bounce would throw away, in exactly the case the
// resume matters most.
type DisplacedTurn struct {
	// Prompt is the text that started the stopped turn.
	Prompt string
	// PermissionMode is the mode that turn was running under, carried so the
	// resumption is the same turn rather than a similar one.
	PermissionMode string
}

// MergeLeaseConfig constructs a MergeLease. EVERY field is required: a lease
// that cannot interrupt the turn it is displacing, or that cannot see the
// queue it is draining, would be a lease that silently does less than it
// claims, so a nil is a hard construction error rather than a defaulted no-op.
type MergeLeaseConfig struct {
	// Manager owns the durable ledger the lease is written to. Required.
	Manager *Manager
	// Queue is the merge subsystem's durable per-repository request channel.
	// Binding it is what makes a held lease well-formed: the push path treats a
	// lease with no queue behind it as a wiring bug and says so. Required.
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

// NewMergeLease validates cfg, binds the merge queue to the manager so a held
// lease is verifiably well-formed, and returns the lease.
func NewMergeLease(cfg MergeLeaseConfig) (*MergeLease, error) {
	if cfg.Manager == nil {
		return nil, fmt.Errorf("ssm: MergeLeaseConfig.Manager is required")
	}
	if cfg.Queue == nil {
		return nil, fmt.Errorf("ssm: MergeLeaseConfig.Queue is required; a lease with no queue behind it is a merge subsystem assembled by a path that skipped construction")
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
	displaced, err := l.interrupter.InterruptForMerge(ctx, ws)
	if err != nil {
		wrapped := fmt.Errorf("ssm: merge lease for workspace %q could not interrupt the in-flight turn: %w", ws, err)
		l.m.logf("ssm: merge lease decision=rollback workspace=%s acquired_at=%d error=%v — the displaced turn is still running, so the claim is withdrawn rather than held over a shim the user still drives",
			ws, at, err)
		if closeErr := l.m.closeMergeLease(ws); closeErr != nil {
			l.m.logError("ssm: merge lease ROLLBACK FAILED workspace=%s acquired_at=%d: %v — an open window remains on the ledger and will keep refusing user prompts until it is released",
				ws, at, closeErr)
		}
		return nil, wrapped
	}
	// RECORDED BEFORE THE MERGE RUNS, on the window itself. From here the
	// displaced turn is the ledger's fact, so the bounce a self-merge causes
	// carries it to the daemon that resumes the merge instead of losing it.
	if displaced != nil {
		if err := l.m.recordDisplacedTurn(ws, *displaced); err != nil {
			// LOUD AND NON-FATAL. The lease itself is sound and the merge it
			// exists for must proceed; what is lost is the automatic resume,
			// which is precisely the kind of degradation that must be visible
			// rather than inferred from a turn that never came back.
			l.m.warn("ssm: merge lease displaced-turn record FAILED workspace=%s acquired_at=%d: %v — the merge proceeds, but the user's stopped turn will NOT be resumed automatically",
				ws, at, err)
		}
	}
	l.m.logf("ssm: merge lease decision=acquire workspace=%s acquired_at=%d displaced_turn=%t — the merge owns this shim; user prompts are refused and every conversation item is stamped CONVERSATION_SOURCE_MERGE until release",
		ws, at, displaced != nil)

	var once sync.Once
	return func() {
		once.Do(func() {
			// READ AND CLEARED AS PART OF THE CLOSE, so the resume is
			// exactly-once by the same write that ends the window: a second
			// release finds nothing, and a crash between the close and the
			// submit loses the resume rather than repeating the user's turn.
			// Losing it is the safe direction — a duplicated prompt is work
			// the user did not ask for twice.
			resume, err := l.m.takeDisplacedTurn(ws)
			if err != nil {
				l.m.warn("ssm: merge lease displaced-turn read FAILED workspace=%s acquired_at=%d: %v — the lease is still released, but nothing is resumed",
					ws, at, err)
			}
			if err := l.m.closeMergeLease(ws); err != nil {
				l.m.logError("ssm: merge lease RELEASE FAILED workspace=%s acquired_at=%d: %v — the window stays open and user prompts stay refused",
					ws, at, err)
				return
			}
			l.m.logf("ssm: merge lease decision=release workspace=%s acquired_at=%d resume_displaced_turn=%t — the shim is handed back to the user",
				ws, at, resume != nil)
			if resume == nil {
				return
			}
			// AFTER the close, never before: the submit is an ordinary USER
			// prompt and guardMergeLease refuses those while the window is
			// open. Resuming under the lease would refuse the very turn the
			// lease displaced.
			//
			// WithoutCancel because the resume outlives the acquisition. The
			// ctx here is whatever the caller took the lease under, and a
			// caller that scopes it to the merge RUN would cancel it at
			// exactly the terminal phase that triggers this release — making
			// the resume fail precisely when it is due. Today's coordinator
			// passes a lifetime context; nothing about this path should
			// depend on that staying true.
			if err := l.interrupter.ResumeDisplacedTurn(context.WithoutCancel(ctx), ws, *resume); err != nil {
				l.m.warn("ssm: merge lease displaced-turn RESUME FAILED workspace=%s acquired_at=%d: %v — the user's stopped turn was not put back and they will have to resubmit it",
					ws, at, err)
				return
			}
			l.m.logf("ssm: merge lease displaced-turn RESUMED workspace=%s acquired_at=%d — the turn the merge stopped is running again",
				ws, at)
		})
	}, nil
}

// recordDisplacedTurn writes the stopped user turn onto the workspace's OPEN
// lease window.
//
// An absent open window is an error rather than a no-op: it means the caller
// recorded a displacement against a lease this ledger does not believe is held,
// and answering "fine" would silently drop a turn the user is owed.
func (m *Manager) recordDisplacedTurn(workspace string, turn DisplacedTurn) error {
	m.mu.Lock()
	defer m.mu.Unlock()
	res, err := m.db.Exec(
		`UPDATE merge_lease SET displaced_prompt=?, displaced_permission_mode=?
		   WHERE workspace=? AND released_at IS NULL`,
		turn.Prompt, turn.PermissionMode, workspace,
	)
	if err != nil {
		return fmt.Errorf("ssm: record displaced turn for workspace %q: %w", workspace, err)
	}
	updated, err := res.RowsAffected()
	if err != nil {
		return fmt.Errorf("ssm: inspect displaced turn write for workspace %q: %w", workspace, err)
	}
	if updated != 1 {
		return fmt.Errorf("ssm: displaced turn write for workspace %q affected %d open window(s), want exactly one", workspace, updated)
	}
	return nil
}

// takeDisplacedTurn reads the workspace's open window's displaced turn and
// clears it in the SAME statement, returning nil when there is nothing to
// resume.
//
// THE CLEAR IS THE EXACTLY-ONCE GUARANTEE. It is not a courtesy tidy-up: the
// row is what a second release, a replayed release, or a coordinator resuming
// this window after a bounce would read, and a resume that is merely
// idempotent-by-convention is a duplicated prompt waiting for a schedule that
// exposes it.
func (m *Manager) takeDisplacedTurn(workspace string) (*DisplacedTurn, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	var prompt, mode sql.NullString
	err := m.db.QueryRow(
		`SELECT displaced_prompt, displaced_permission_mode FROM merge_lease
		   WHERE workspace=? AND released_at IS NULL`,
		workspace,
	).Scan(&prompt, &mode)
	if errors.Is(err, sql.ErrNoRows) {
		// No open window. The release path reaches this whenever the lease was
		// already closed, which carries no displaced turn by construction.
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("ssm: read displaced turn for workspace %q: %w", workspace, err)
	}
	if !prompt.Valid || prompt.String == "" {
		return nil, nil
	}
	if _, err := m.db.Exec(
		`UPDATE merge_lease SET displaced_prompt=NULL, displaced_permission_mode=NULL
		   WHERE workspace=? AND released_at IS NULL`,
		workspace,
	); err != nil {
		return nil, fmt.Errorf("ssm: clear displaced turn for workspace %q: %w", workspace, err)
	}
	return &DisplacedTurn{Prompt: prompt.String, PermissionMode: mode.String}, nil
}

// DisplacedTurnFor reports the turn the workspace's open lease window stopped,
// or nil when there is none. Read-only: it never clears, so it is safe for the
// diagnostics and tests that ask what a held lease is holding.
func (m *Manager) DisplacedTurnFor(workspace string) (*DisplacedTurn, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	var prompt, mode sql.NullString
	err := m.db.QueryRow(
		`SELECT displaced_prompt, displaced_permission_mode FROM merge_lease
		   WHERE workspace=? AND released_at IS NULL`,
		workspace,
	).Scan(&prompt, &mode)
	if errors.Is(err, sql.ErrNoRows) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("ssm: read displaced turn for workspace %q: %w", workspace, err)
	}
	if !prompt.Valid || prompt.String == "" {
		return nil, nil
	}
	return &DisplacedTurn{Prompt: prompt.String, PermissionMode: mode.String}, nil
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

// bindMergeQueue attaches the merge subsystem's queue. Binding twice is a
// wiring bug: two queues means two merge subsystems contending for one
// manager's leases, so the second is refused loudly.
func (m *Manager) bindMergeQueue(q merge.Queue) error {
	if q == nil {
		return fmt.Errorf("ssm: bindMergeQueue got a nil merge.Queue")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.mergeQueue != nil {
		return fmt.Errorf("ssm: a merge.Queue is already bound; a second one would put two merge subsystems behind one manager's leases")
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
		m.warn("ssm: merge lease close FAILED workspace=%s at=%d error=%v", workspace, at, err)
		return fmt.Errorf("ssm: close merge lease for workspace %q: %w", workspace, err)
	}
	closed, err := res.RowsAffected()
	if err != nil {
		m.warn("ssm: merge lease close inspection FAILED workspace=%s at=%d error=%v", workspace, at, err)
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

// mergeWiringCheckLocked reports the one combination the merge wiring must not
// produce: a workspace holding a merge LEASE while no merge.Queue is bound.
//
// A daemon assembled without a merge subsystem has no queue at all, and nothing
// can be enqueued on it, so no lease can exist either. Only NewMergeLease can
// produce a lease and it binds the queue, so a held lease with no queue is a
// wiring bug — the merge subsystem was assembled by some path that skipped
// construction. It is logged rather than passed over.
//
// It used to guard the flat merge_queue_position / merge_queue_depth stamped
// from a queue snapshot at push time. Those fields are retired (MergeStatus's
// enqueued arm reports the place the run was ADMITTED at instead), but the
// wiring bug the check detects is unchanged, so the check outlives them.
func (m *Manager) mergeWiringCheckLocked(workspace string, leaseHeld bool) {
	if m.mergeQueue != nil || !leaseHeld {
		return
	}
	m.logf("ssm: INVARIANT VIOLATION ws=%s holds a merge lease with NO merge.Queue bound — only NewMergeLease can produce a lease and it binds the queue, so the merge subsystem is mis-wired",
		workspace)
}

// stampMergeFactsLocked writes the merge fields no per-workspace state row can
// carry onto a WorkspaceState about to be pushed. Caller holds mu.
//
// merged_at_ms rides here rather than off the resolution because it is a fact
// about the workspace's HISTORY, not about which row currently wins, so
// resolving it would tie a permanent instant to whichever transition happened
// to be newest.
//
// It is called from workspaceMessageLocked and nowhere else, which is what
// makes every push, snapshot and synchronous publication carry the same
// answer: one construction funnel, so a frame that omits them is
// unrepresentable rather than merely unlikely.
func (m *Manager) stampMergeFactsLocked(workspace string, r resolved, msg *frontendv1.WorkspaceState) {
	leaseHeld := len(m.openMergeWindowsLocked(workspace)) > 0
	m.mergeWiringCheckLocked(workspace, leaseHeld)
	msg.MergeLeaseHeld = leaseHeld
	msg.MergedAtMs = m.mergedAtLocked(workspace)
	// merge_status carries its own queue place: the enqueued arm reports the
	// position the run was ADMITTED at, observed by the coordinator under the
	// repository's advance gate, rather than a snapshot re-read at push time.
	// It is the ONLY queue surface on the message now that the flat
	// merge_queue_position / merge_queue_depth pair is retired.
	m.stampMergeStatusLocked(workspace, r.mergePhase, msg)
	// The outstanding dequeue question rides the same funnel as the facts above
	// for the same reason they do: it is a field of the message, and a message
	// built anywhere else would be a card that silently fails to appear.
	m.stampMergeDequeueOfferLocked(workspace, msg)
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
