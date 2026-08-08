package ssm

import (
	"database/sql"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// A REVIVE-COMPACT TURN WHOSE COMPACTION IS OVER MUST NOT OUTLIVE A RESTART.
//
// The shim used to withhold the compaction result's turn close whenever its
// pending task-notification queue was non-empty, which a revived conversation's
// transcript replay guaranteed. The claim written under that defect is durable,
// so the daemon heals it at Open — and ONLY there, and only for the one turn
// identity whose completion a closed compacting window actually proves.
// ---------------------------------------------------------------------------

// evTurnStartedNamed is a TurnStarted carrying an explicit turn identity, which
// is what a revival's `/compact` submits under.
func evTurnStartedNamed(sid string, seq uint64, turnID string) *corev1.Event {
	return &corev1.Event{
		SessionId: sid,
		Seq:       seq,
		Plane:     corev1.Plane_PLANE_STREAM,
		// The envelope's request id IS the turn's identity; the boundary door
		// refuses the two disagreeing.
		RequestId: turnID,
		Payload:   &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: turnID}},
	}
}

// reopenAt reopens a Manager over an existing state DB, running warm() — the
// boot reconciliation the subject of these tests lives in.
func reopenAt(t *testing.T, path string, resolver Resolver) (*Manager, *capLog) {
	t.Helper()
	cl := &capLog{}
	m, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: resolver})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { m.Close() })
	return m, cl
}

// compactionClaimEnd reports one claim row's end coordinate and attribution,
// admitting the NULL that an open claim carries.
func compactionClaimEnd(t *testing.T, m *Manager, workspace, turnID string) (sql.NullInt64, string) {
	t.Helper()
	var endSeq sql.NullInt64
	var cause string
	err := m.db.QueryRow(`SELECT end_seq, end_cause FROM turn_lifecycle_claim
		WHERE workspace=? AND turn_id=?`, workspace, turnID).Scan(&endSeq, &cause)
	if err != nil {
		t.Fatalf("read claim %q: %v", turnID, err)
	}
	return endSeq, cause
}

// wedgeReviveCompactTurn arranges the observed live shape: a compact-first
// revival's turn opens, the vendor's compacting window opens and closes, and no
// `TurnEnded` ever arrives. It returns the state DB path the claim is durable in.
func wedgeReviveCompactTurn(t *testing.T, turnID string) string {
	t.Helper()
	m, _, path := openTest(t, fakeResolver{"s1": "ws1"})
	if err := applyTest(m, evTurnStartedNamed("s1", 5160, turnID)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.ApplyCompacting("ws1", true, "vendor_status:compacting"); err != nil {
		t.Fatalf("compacting open: %v", err)
	}
	if err := m.ApplyCompacting("ws1", false, "vendor_status:"); err != nil {
		t.Fatalf("compacting close: %v", err)
	}
	if err := m.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}
	return path
}

func TestReopenClosesAReviveCompactClaimWhoseCompactionCompleted(t *testing.T) {
	// Arrange — the live wedge: `revive-compact:...` open, compaction done.
	turnID := "revive-compact:s_55ed1b28363b103c:21ee716803efa2be"
	path := wedgeReviveCompactTurn(t, turnID)

	// Act — the daemon restarts.
	m, _ := reopenAt(t, path, fakeResolver{"s1": "ws1"})

	// Assert — the claim carries the reconciled end.
	endSeq, cause := compactionClaimEnd(t, m, "ws1", turnID)
	if !endSeq.Valid {
		t.Fatalf("claim %q is still open after a restart; the compaction it was submitted to perform completed", turnID)
	}
	if cause != TurnCloseCompactionCompleteReconciled {
		t.Fatalf("end_cause = %q, want %q", cause, TurnCloseCompactionCompleteReconciled)
	}
}

func TestReopenReleasesTheTurnBandOfAReconciledCompactionClaim(t *testing.T) {
	// Arrange.
	turnID := "revive-compact:s1:aaaa"
	path := wedgeReviveCompactTurn(t, turnID)

	// Act.
	m, _ := reopenAt(t, path, fakeResolver{"s1": "ws1"})

	// Assert — nothing holds the workspace in a turn any more.
	l, err := m.TurnLiveness("ws1")
	if err != nil {
		t.Fatalf("TurnLiveness: %v", err)
	}
	if l.Active() {
		t.Fatalf("liveness = %s, want no turn in flight: the claim behind the band was reconciled closed", l)
	}
}

func TestReopenWarnsWhenItReconcilesACompletedCompactionClaim(t *testing.T) {
	// Arrange.
	turnID := "revive-compact:s1:bbbb"
	path := wedgeReviveCompactTurn(t, turnID)

	// Act.
	_, cl := reopenAt(t, path, fakeResolver{"s1": "ws1"})

	// Assert — a past defect's residue is surfaced, not absorbed quietly.
	if !cl.contains("revive-compact turn claim RECONCILED CLOSED") {
		t.Fatalf("no reconciled-close record; lines=%v", cl.lines)
	}
}

func TestReopenLeavesAReviveCompactClaimWithNoCompactionEvidenceOpen(t *testing.T) {
	// Arrange — the revival's turn started and the compacting window NEVER
	// closed, so nothing proves the compaction finished.
	turnID := "revive-compact:s1:cccc"
	m, _, path := openTest(t, fakeResolver{"s1": "ws1"})
	if err := applyTest(m, evTurnStartedNamed("s1", 5160, turnID)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.ApplyCompacting("ws1", true, "vendor_status:compacting"); err != nil {
		t.Fatalf("compacting open: %v", err)
	}
	if err := m.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act.
	m2, _ := reopenAt(t, path, fakeResolver{"s1": "ws1"})

	// Assert.
	endSeq, _ := compactionClaimEnd(t, m2, "ws1", turnID)
	if endSeq.Valid {
		t.Fatalf("claim %q was closed with no evidence its compaction ever completed", turnID)
	}
}

func TestReopenLeavesAReviveCompactClaimOpenWhenTheCompactionPrecededIt(t *testing.T) {
	// Arrange — an OLDER compaction, then the revival's turn. The closed window
	// belongs to earlier work and says nothing about this turn.
	turnID := "revive-compact:s1:dddd"
	m, _, path := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyCompacting("ws1", true, "vendor_status:compacting"); err != nil {
		t.Fatalf("compacting open: %v", err)
	}
	if err := m.ApplyCompacting("ws1", false, "vendor_status:"); err != nil {
		t.Fatalf("compacting close: %v", err)
	}
	if err := applyTest(m, evTurnStartedNamed("s1", 5160, turnID)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act.
	m2, _ := reopenAt(t, path, fakeResolver{"s1": "ws1"})

	// Assert.
	endSeq, _ := compactionClaimEnd(t, m2, "ws1", turnID)
	if endSeq.Valid {
		t.Fatalf("claim %q was closed on a compaction that finished before it started", turnID)
	}
}

func TestReopenLeavesAnOrdinaryTurnsClaimOpenAcrossACompletedCompaction(t *testing.T) {
	// Arrange — an ordinary conversational turn that compacted mid-flight. The
	// turn legitimately continues after the window closes, so a closed
	// compacting window is not its boundary.
	turnID := "req-42"
	m, _, path := openTest(t, fakeResolver{"s1": "ws1"})
	if err := applyTest(m, evTurnStartedNamed("s1", 5160, turnID)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.ApplyCompacting("ws1", true, "vendor_status:compacting"); err != nil {
		t.Fatalf("compacting open: %v", err)
	}
	if err := m.ApplyCompacting("ws1", false, "vendor_status:"); err != nil {
		t.Fatalf("compacting close: %v", err)
	}
	if err := m.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act.
	m2, _ := reopenAt(t, path, fakeResolver{"s1": "ws1"})

	// Assert.
	endSeq, _ := compactionClaimEnd(t, m2, "ws1", turnID)
	if endSeq.Valid {
		t.Fatalf("ordinary turn %q was force-closed by the compaction reconciliation", turnID)
	}
}

func TestReopenRecordsTheDurableInterruptionForAReconciledCompactionClaim(t *testing.T) {
	// Arrange — without the durable end keyed on the start's STORE coordinate,
	// a replay of the same stream would reopen the very claim just retired.
	turnID := "revive-compact:s1:eeee"
	path := wedgeReviveCompactTurn(t, turnID)

	// Act.
	m, _ := reopenAt(t, path, fakeResolver{"s1": "ws1"})

	// Assert.
	var cause string
	err := m.db.QueryRow(`SELECT cause FROM turn_interruption
		WHERE workspace=? AND start_event_session_id=? AND turn_id=?`,
		"ws1", "s1", turnID).Scan(&cause)
	if err != nil {
		t.Fatalf("read turn_interruption for %q: %v", turnID, err)
	}
	if cause != TurnCloseCompactionCompleteReconciled {
		t.Fatalf("interruption cause = %q, want %q", cause, TurnCloseCompactionCompleteReconciled)
	}
}

func TestReopenSettlesTheRenderStateOfAReconciledCompactionClaim(t *testing.T) {
	// Arrange.
	turnID := "revive-compact:s1:ffff"
	path := wedgeReviveCompactTurn(t, turnID)

	// Act.
	m, _ := reopenAt(t, path, fakeResolver{"s1": "ws1"})

	// Assert — the workspace no longer renders as a running turn.
	if got := mustCurrent(t, m, "ws1").State; got == frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want anything but THINKING: the turn behind the band was reconciled closed", renderName(got))
	}
}
