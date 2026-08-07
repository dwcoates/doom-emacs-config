package ssm

import "fmt"

// This file owns the MERGED-WORKSPACE LIFECYCLE FACTS: when a workspace's
// merge landed, and what the daemon does to the workspace's session once it
// has.
//
// Both exist because the frontends were stripped of merge state entirely. A
// sidebar ordering a recently-merged section has no memory of its own, so the
// instant has to be a daemon-held fact it can read off every WorkspaceState;
// and a frontend that tears nothing down means the daemon owns standing the
// merged workspace's session down.
//
// Every error here is recorded through Manager.logf with the workspace, the
// operation and the concrete cause, the same canonical path the merge lease
// ledger and the connectivity axis use.

// MergedTeardown stands a merged workspace's session down: the merge has
// landed, so nothing is left for its shim to do and its ~500MB is owed back.
// Satisfied by *sessioncontroller.Manager.
//
// It is the SAME authority that interrupts the turn a merge lease displaces
// (see SessionAuthority in mergelease.go), and deliberately so: both are the
// merge subsystem reaching into one workspace's session lifecycle, and a
// second injection point could be bound to a different controller fleet —
// stopping a shim on one daemon's session while another drove the merge.
type MergedTeardown interface {
	// TeardownMerged stops the workspace's session and its child shim. A
	// workspace with no live session is NOT an error (there is nothing to stand
	// down); anything that failed to stop is.
	TeardownMerged(workspace string) error
}

// recordMergedAtLocked persists the instant workspace's merge landed and
// reports whether this call is the one that established it. Caller holds mu.
//
// FIRST WRITE WINS, and the primary key is what decides it rather than a
// read-then-write in Go. A workspace that merges a second time keeps the
// instant it FIRST merged at: the fact the frontends order on is "this
// workspace's work reached its target", which became true once and cannot
// become truer.
func (m *Manager) recordMergedAtLocked(workspace string, at int64) (mergedAt int64, established bool, err error) {
	res, err := m.db.Exec(
		`INSERT INTO workspace_merged(workspace, merged_at) VALUES (?,?)
		 ON CONFLICT(workspace) DO NOTHING`,
		workspace, at,
	)
	if err != nil {
		m.warn("ssm: merged-at record FAILED workspace=%s at=%d error=%v — the workspace is merged and carries no merged_at_ms, so no frontend can order it",
			workspace, at, err)
		return 0, false, fmt.Errorf("ssm: record merged-at for workspace %q at %d: %w", workspace, at, err)
	}
	inserted, err := res.RowsAffected()
	if err != nil {
		m.warn("ssm: merged-at record inspection FAILED workspace=%s at=%d error=%v", workspace, at, err)
		return 0, false, fmt.Errorf("ssm: inspect merged-at record for workspace %q: %w", workspace, err)
	}
	if inserted == 1 {
		m.mergedAt[workspace] = at
		m.logf("ssm: merged-at ws=%s merged_at_ms=%d — the merge landed; the instant is durable and rides every WorkspaceState from here on", workspace, at)
		return at, true, nil
	}
	kept, ok := m.mergedAt[workspace]
	if !ok {
		// The row exists and the projection does not. The projection is warmed
		// from the table at Open and written beside every insert, so the two
		// cannot legitimately disagree.
		return 0, false, fmt.Errorf("ssm: merged-at for workspace %q is persisted but missing from the in-memory projection; the two are written together and warmed together, so they cannot disagree", workspace)
	}
	m.logf("ssm: merged-at ws=%s decision=keep merged_at_ms=%d ignored_at=%d — this workspace already merged, and the first landing is the fact the frontends order on",
		workspace, kept, at)
	return kept, false, nil
}

// mergedAtLocked returns the instant workspace's merge landed, or 0 when it
// has never merged. Caller holds mu.
//
// It reads the projection rather than the table so it is TOTAL, exactly as
// MergeLeaseHeld is: it runs inside the one WorkspaceState construction funnel,
// which has no error channel to report a failed query on.
func (m *Manager) mergedAtLocked(workspace string) int64 { return m.mergedAt[workspace] }

// warmMergedAtLocked seeds the in-memory merged-at projection from the durable
// table at Open. Caller holds mu.
func (m *Manager) warmMergedAtLocked() error {
	rows, err := m.db.Query(`SELECT workspace, merged_at FROM workspace_merged`)
	if err != nil {
		return fmt.Errorf("ssm: warm merged-at facts: %w", err)
	}
	defer rows.Close()
	for rows.Next() {
		var ws string
		var at int64
		if err := rows.Scan(&ws, &at); err != nil {
			return fmt.Errorf("ssm: scan merged-at fact: %w", err)
		}
		m.mergedAt[ws] = at
	}
	if err := rows.Err(); err != nil {
		return fmt.Errorf("ssm: iterate merged-at facts: %w", err)
	}
	if len(m.mergedAt) > 0 {
		m.logf("ssm: merged-at ledger restored with %d merged workspace(s)", len(m.mergedAt))
	}
	return nil
}

// bindMergedTeardown attaches the session authority that stands a merged
// workspace down. Binding twice is a wiring bug — two authorities would each
// stop a different fleet's shim for one merge — so the second is refused.
func (m *Manager) bindMergedTeardown(t MergedTeardown) error {
	if t == nil {
		return fmt.Errorf("ssm: bindMergedTeardown got a nil MergedTeardown")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.mergedTeardown != nil {
		return fmt.Errorf("ssm: a MergedTeardown is already bound; a second one would stand a merged workspace down through a controller fleet that never drove its merge")
	}
	m.mergedTeardown = t
	return nil
}

// teardownMerged stands the merged workspace's session down. It is called from
// ApplyMergeTransition, AFTER the lock is released and AFTER the `merged`
// WorkspaceState has been handed to the subscribers.
//
// THE ORDER IS STRUCTURAL, not a convention. Every state a frontend sees
// travels one ordered subscriber channel, and the merged frame is enqueued on
// it before this function is entered — so the teardown's own states (the
// hibernated connectivity edge) can only arrive behind it. A frontend can
// therefore never watch a workspace disappear without first being told it
// merged.
//
// IT RETURNS NOTHING ON PURPOSE. A teardown failure must not travel back up
// ApplyMergeTransition: that error reaches merge.Driver.finalizeMerged, which
// would report a cherry-pick that ALREADY LANDED as a failed merge and
// contradict the durable merged fact this same call just established. The
// failure is instead surfaced the way the merge lease's release failure is —
// loudly, through the canonical log, naming what is left running.
func (m *Manager) teardownMerged(workspace string, mergedAt int64) {
	m.mu.Lock()
	teardown := m.mergedTeardown
	m.mu.Unlock()
	if teardown == nil {
		m.logf("ssm: merged teardown ws=%s merged_at_ms=%d decision=none — no MergedTeardown is bound to this SSM, so the merged workspace's session keeps running",
			workspace, mergedAt)
		return
	}
	m.logf("ssm: merged teardown ws=%s merged_at_ms=%d decision=stand_down — the merge landed, so the daemon stops this workspace's session and shim",
		workspace, mergedAt)
	if err := teardown.TeardownMerged(workspace); err != nil {
		m.warn("ssm: merged teardown FAILED ws=%s merged_at_ms=%d error=%v — the merged fact STANDS and was already pushed; the session is still running and must be stood down by hand",
			workspace, mergedAt, err)
		return
	}
	m.logf("ssm: merged teardown ws=%s merged_at_ms=%d decision=complete — the session is stood down", workspace, mergedAt)
}
