// HIBERNATION AS ONE TRANSITION: the daemon stops the shim, marks the session
// asleep, and says WHY in a typed cause the revival gate can render.
//
// There are exactly three causes and this file covers two of them — the idle
// cutoff, and the user forcing it. The third (the cache going cold before a
// ping could fire) belongs to the keep-alive window and lives in
// keepalivetiming_e2e_test.go beside the policy it is a consequence of.
//
// WHY THE REFUSALS MATTER AS MUCH AS THE SUCCESS. A hibernate stops the shim.
// Honoring one while a turn is in flight would discard work the user is
// watching; honoring one while the merge lease is held would pull the shim out
// from under a merge that borrowed it. Both are refused LOUDLY — the user
// interrupts first — so the nack is the contract, not an implementation
// detail.
package e2e

import (
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// --- (3) the idle cutoff ------------------------------------------------------

// idleCutoffPolicy is a keep-alive policy whose IDLE CUTOFF trips before the
// ping window ever opens.
//
// The ordering is deliberate and is the only way this edge can be observed
// alone. A production cutoff sits far beyond the cache TTL, so an idle session
// that is being swept reaches the cache-expiry edge first and the two causes
// could not be told apart. Moving the cutoff below the ping window isolates it:
// at the check below, the cutoff is the ONLY thing that has been crossed.
func idleCutoffPolicy() keepAlivePolicy {
	p := testKeepAlivePolicy()
	p.idleCutoff = 3 * time.Minute
	return p
}

// TestE2ETheIdleCutoffHibernatesWithItsOwnCause covers AUTOMATIC SLEEP: a
// workspace left alone past the configured cutoff is put to sleep, and the
// cause says so.
func TestE2ETheIdleCutoffHibernatesWithItsOwnCause(t *testing.T) {
	// Arrange
	policy := idleCutoffPolicy()
	s := newKeepAliveSession(t, policy)

	// Act
	s.idleFor(t, policy.idleCutoff)

	// Assert
	detail := awaitHibernationDetail(t, s.conn, s.sessionID)
	if detail.GetIdleCutoff() == nil {
		t.Fatalf("hibernation cause is %T, want the idle_cutoff arm", detail.GetCause())
	}
	if got, want := detail.GetIdleCutoff().GetCutoffMs(), policy.idleCutoff.Milliseconds(); got != want {
		t.Errorf("idle_cutoff cutoff_ms = %d, want the configured cutoff %d: the gate says \"asleep after Nh idle\" from this field alone", got, want)
	}
}

// TestE2EAHibernationIsStampedWithWhenItHappened covers the OTHER half of the
// detail every cause carries: since_ms. Without it the gate can name the reason
// but not how long ago, which is the first thing a returning user wants.
func TestE2EAHibernationIsStampedWithWhenItHappened(t *testing.T) {
	// Arrange
	policy := idleCutoffPolicy()
	s := newKeepAliveSession(t, policy)
	before := time.Now().UnixMilli()

	// Act
	s.idleFor(t, policy.idleCutoff)

	// Assert — a real stamp, not a zero. The daemon's clock has been moved
	// forward, so the upper bound is the moved clock rather than wall time.
	detail := awaitHibernationDetail(t, s.conn, s.sessionID)
	if got := detail.GetSinceMs(); got < before {
		t.Errorf("hibernation since_ms = %d, want a stamp at or after this test began (%d)", got, before)
	}
}

// --- (4) the user forcing it ---------------------------------------------------

// TestE2EAForcedHibernationOfAnIdleWorkspaceIsHonored covers the DELIBERATE
// sleep: nothing is running, the user asks for the memory back, and the daemon
// does it — reporting the FORCED cause rather than pretending a timer fired.
func TestE2EAForcedHibernationOfAnIdleWorkspaceIsHonored(t *testing.T) {
	// Arrange — an idle, settled session.
	s := newKeepAliveSession(t, testKeepAlivePolicy())

	// Act
	sendHibernate(t, s.conn, "r-hibernate")

	// Assert
	if ack := awaitAck(t, s.conn, "r-hibernate", "the forced hibernate"); !ack.GetOk() {
		t.Fatalf("hibernateWorkspace on an idle workspace nacked: %s", ack.GetError())
	}
	detail := awaitHibernationDetail(t, s.conn, s.sessionID)
	if detail.GetForced() == nil {
		t.Fatalf("hibernation cause is %T, want the forced arm: the user asked for this one", detail.GetCause())
	}
}

// TestE2EAForcedHibernationPaintsTheWorkspaceAsleep covers what the rest of the
// editor sees: hibernation is the BENIGN absence of a session (teal), never the
// severance that says something broke.
func TestE2EAForcedHibernationPaintsTheWorkspaceAsleep(t *testing.T) {
	// Arrange
	s := newKeepAliveSession(t, testKeepAlivePolicy())

	// Act
	sendHibernate(t, s.conn, "r-hibernate")

	// Assert
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a WorkspaceState reporting the workspace hibernated": func(frame *frontendv1.FrontendFrame) bool {
			return workspaceStateFor(frame, s.cwd).GetState() == frontendv1.RenderState_RENDER_STATE_HIBERNATED
		},
	})
}

// TestE2EAForcedHibernationIsRefusedWhileATurnIsLive covers the FIRST REFUSAL:
// a hibernate stops the shim, and stopping it mid-turn throws away work the
// user is watching. The daemon never discards in-flight work to satisfy a
// hibernate — the user interrupts first.
func TestE2EAForcedHibernationIsRefusedWhileATurnIsLive(t *testing.T) {
	// Arrange — a genuinely in-flight turn.
	s := newKeepAliveSession(t, testKeepAlivePolicy())
	holdTurnOpen(t, s.conn, s.cwd, "r-hold", "sleep e2e-forced-hibernate")

	// Act
	sendHibernate(t, s.conn, "r-hibernate")

	// Assert
	ack := awaitAck(t, s.conn, "r-hibernate", "the forced hibernate")
	if ack.GetOk() {
		t.Fatal("hibernateWorkspace was acked ok while a turn was in flight: honoring it stops the shim and discards the turn the user is watching")
	}
	if ack.GetError() == "" {
		t.Error("the refusal carries no error text: a nack the user cannot read is a silent failure")
	}
}

// TestE2EAForcedHibernationIsRefusedUnderTheMergeLease covers the SECOND
// REFUSAL: the merge lease is an exclusivity claim over the workspace's shim,
// so stopping that shim would pull it out from under a merge that borrowed it
// to resolve a conflict.
func TestE2EAForcedHibernationIsRefusedUnderTheMergeLease(t *testing.T) {
	// Arrange — a live session on a conflicting worktree, merging, lease held.
	policy := testKeepAlivePolicy()
	h := keepAliveHarness(t, policy)
	repo := newMergeRepo(t)
	wsDir := repo.conflictingWorktree("feature-hibernate-lease")
	s := adoptKeepAliveSession(t, h, policy, wsDir)
	w := newMergeWatch(t, s.conn)
	sendMerge(t, s.conn, "r-merge-hibernate", mergeCmdFor(t, h.geometry, repo, wsDir, "feature-hibernate-lease"))
	w.awaitOKAck("r-merge-hibernate")
	w.awaitLeaseHeld(wsDir)

	// Act
	sendHibernate(t, s.conn, "r-hibernate")

	// Assert
	ack := w.awaitAck("r-hibernate")
	if ack.GetOk() {
		t.Fatal("hibernateWorkspace was acked ok while the merge lease was held: the shim belongs to the merge, which is still using it")
	}
	if ack.GetError() == "" {
		t.Error("the refusal carries no error text: a nack the user cannot read is a silent failure")
	}
}
