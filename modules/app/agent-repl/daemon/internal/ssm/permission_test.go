package ssm

import (
	"path/filepath"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// The PERMISSION row: the agent has asked the user a canUseTool question and
// is parked until it is answered.
//
// It is the one render state the whole stack could paint and nothing could
// produce, which is why these tests are mostly about the row APPEARING at all —
// and then about the ways it must not stick around: a turn that ends holding
// the question, a rotation that retires the identity that asked it, a restart
// that takes the rendezvous with it.
// ---------------------------------------------------------------------------

// openPermTest opens a Manager on a temp DB with a capturing logger, and
// returns the db path so a REOPEN (the restart case) can be driven over the
// same log.
func openPermTest(t *testing.T, resolver Resolver) (*Manager, *capLog, string) {
	t.Helper()
	path := filepath.Join(t.TempDir(), "state.db")
	cl := &capLog{}
	m, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: resolver})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { m.Close() })
	wireAll(t, m, resolver)
	return m, cl, path
}

// startTurn puts a workspace mid-turn, which is the only place a canUseTool
// question is ever asked from.
func startTurn(t *testing.T, m *Manager) {
	t.Helper()
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
}

// ---- The opening and closing edges ----------------------------------------

func TestPermissionEdgesResolveTheirState(t *testing.T) {
	tests := []struct {
		name string
		// act drives the pending count's edges, in order.
		act  func(t *testing.T, m *Manager)
		want frontendv1.RenderState
	}{
		{
			name: "the first pending permission opens the row",
			act: func(t *testing.T, m *Manager) {
				mustApplyPermission(t, m, true, "pending=1")
			},
			want: frontendv1.RenderState_RENDER_STATE_PERMISSION,
		},
		{
			name: "a second concurrent permission changes nothing",
			act: func(t *testing.T, m *Manager) {
				mustApplyPermission(t, m, true, "pending=1")
				mustApplyPermission(t, m, true, "pending=2")
			},
			want: frontendv1.RenderState_RENDER_STATE_PERMISSION,
		},
		{
			name: "answering one of two leaves the row open",
			act: func(t *testing.T, m *Manager) {
				mustApplyPermission(t, m, true, "pending=1")
				mustApplyPermission(t, m, true, "pending=2")
				mustApplyPermission(t, m, true, "pending=1")
			},
			want: frontendv1.RenderState_RENDER_STATE_PERMISSION,
		},
		{
			name: "the count reaching zero restores the running turn",
			act: func(t *testing.T, m *Manager) {
				mustApplyPermission(t, m, true, "pending=1")
				mustApplyPermission(t, m, false, "pending=0")
			},
			want: frontendv1.RenderState_RENDER_STATE_THINKING,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — a turn in flight, which is where a permission is asked.
			m, _, _ := openPermTest(t, fakeResolver{"s1": "ws1"})
			startTurn(t, m)

			// Act.
			tc.act(t, m)

			// Assert.
			if got := mustCurrent(t, m, "ws1").GetState(); got != tc.want {
				t.Fatalf("state = %s, want %s", renderName(got), renderName(tc.want))
			}
		})
	}
}

func TestPermissionCloseRestoresTurnActive(t *testing.T) {
	// Arrange — a live turn parked on a question.
	m, _, _ := openPermTest(t, fakeResolver{"s1": "ws1"})
	startTurn(t, m)
	mustApplyPermission(t, m, true, "pending=1")

	// Act.
	mustApplyPermission(t, m, false, "pending=0")

	// Assert — the restored row must report the turn as running, or the queue
	// and the footer would read a mid-turn workspace as settled.
	if !mustCurrent(t, m, "ws1").GetTurnActive() {
		t.Fatal("turn_active = false after the permission closed; the turn that asked is still in flight")
	}
}

func TestPermissionCloseWithNoLiveTurnBeneathLeavesTheAxis(t *testing.T) {
	// Arrange — a question asked over a SETTLED session-status lifecycle. The question
	// travels the direct shim control path and its turn's TurnStarted travels
	// the store, so a question can reach this log while the newest agent row is
	// still the PREVIOUS turn's end. There is then no `thinking` to restore.
	m, cl, _ := openPermTest(t, fakeResolver{"s1": "ws1"})
	startTurn(t, m)
	if err := m.Apply(evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	mustApplyPermission(t, m, true, "pending=1")

	// Act.
	mustApplyPermission(t, m, false, "pending=0")

	// Assert — no fabricated turn; the row stands until the agent's next one.
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_PERMISSION {
		t.Fatalf("state = %s, want PERMISSION (the axis left as it stands)", renderName(got))
	}
	if !cl.contains("permission answered with no live turn beneath it") {
		t.Fatal("the refusal to fabricate a turn must be named in the log, not silent")
	}
}

// ---- The row is superseded, never latched --------------------------------

func TestTurnEndedDuringAPendingPermissionWins(t *testing.T) {
	// Arrange — a turn parked on a question.
	m, _, _ := openPermTest(t, fakeResolver{"s1": "ws1"})
	startTurn(t, m)
	mustApplyPermission(t, m, true, "pending=1")

	// Act — the turn ends while the question is still pending.
	if err := m.Apply(evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}

	// Assert — the later row wins; the question died with its turn.
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE (the turn end is the later session-status lifecycle row)", renderName(got))
	}
}

func TestPermissionCloseAfterTurnEndedIsALoudNoOp(t *testing.T) {
	// Arrange — the turn ended while the question was pending, so the row is
	// already superseded when the abandonment's close arrives.
	m, cl, _ := openPermTest(t, fakeResolver{"s1": "ws1"})
	startTurn(t, m)
	mustApplyPermission(t, m, true, "pending=1")
	if err := m.Apply(evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}

	// Act.
	mustApplyPermission(t, m, false, "pending=0")

	// Assert — nothing moves, and the no-op is named.
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE (a close with nothing open appends nothing)", renderName(got))
	}
	if !cl.contains("permission row unchanged ws=ws1 pending=false") {
		t.Fatal("a close with nothing open must be loud, not silently dropped")
	}
}

func TestDoubleOpenIsALoudNoOp(t *testing.T) {
	// Arrange.
	m, cl, _ := openPermTest(t, fakeResolver{"s1": "ws1"})
	startTurn(t, m)
	mustApplyPermission(t, m, true, "pending=1")

	// Act — a second open over an already-open row.
	mustApplyPermission(t, m, true, "pending=2")

	// Assert — one row, and the second edge is named.
	if got := permissionRowCount(t, m, "ws1"); got != 1 {
		t.Fatalf("permission rows = %d, want 1 (a second open must not stack)", got)
	}
	if !cl.contains("permission row unchanged ws=ws1 pending=true") {
		t.Fatal("a redundant open must be loud, not silently dropped")
	}
}

// ---- The row cannot outlive its rendezvous -------------------------------

func TestSessionRotationReleasesAPendingPermission(t *testing.T) {
	// Arrange — a turn parked on a question when the vendor retires the uuid.
	m, cl, _ := openPermTest(t, fakeResolver{"s1": "ws1"})
	startTurn(t, m)
	mustApplyPermission(t, m, true, "pending=1")

	// Act.
	if err := m.ApplySessionRotated("ws1", "v-old", "v-new"); err != nil {
		t.Fatalf("ApplySessionRotated: %v", err)
	}

	// Assert — the row is released AND the turn it buried is reconciled, which
	// is the whole reason the release must happen first.
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_IDLE {
		t.Fatalf("state = %s, want IDLE (the rotation reconciles the turn the row was covering)", renderName(got))
	}
	if !cl.contains("permission row released by session_rotated") {
		t.Fatal("the release must name the rotation that caused it")
	}
}

func TestReopenReleasesAPersistedPendingPermission(t *testing.T) {
	// Arrange — a permission row standing in the log when the daemon goes down.
	m, _, path := openPermTest(t, fakeResolver{"s1": "ws1"})
	startTurn(t, m)
	mustApplyPermission(t, m, true, "pending=1")
	if err := m.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act — the restart.
	cl := &capLog{}
	reopened, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: fakeResolver{"s1": "ws1"}})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { reopened.Close() })
	// A new controller generation re-establishes the session after restart.
	connectOperational(t, reopened, "ws1", "s1", "generation-reopen")

	// Assert — the rendezvous did not survive, so neither does the row; the
	// turn beneath it does, because the shim did not die with the daemon.
	if got := mustCurrent(t, reopened, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING (the row is released, the turn beneath stands)", renderName(got))
	}
	if !cl.contains("permission row released by daemon_restart") {
		t.Fatal("the restart release must be named in the log")
	}
}

func TestApplyPermissionRejectsAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	m, _, _ := openPermTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	err := m.ApplyPermission("", true, "pending=1")

	// Assert.
	if err == nil {
		t.Fatal("an empty workspace must be a loud error, not a silent no-op")
	}
}

// ---- helpers --------------------------------------------------------------

// mustApplyPermission drives one edge of the permission row, failing loudly.
func mustApplyPermission(t *testing.T, m *Manager, pending bool, reason string) {
	t.Helper()
	if err := m.ApplyPermission("ws1", pending, reason); err != nil {
		t.Fatalf("ApplyPermission(pending=%t): %v", pending, err)
	}
}

// permissionRowCount counts the `permission` rows a workspace's log holds, so
// a redundant edge that appended anyway is caught rather than hidden behind a
// resolution that happens to agree.
func permissionRowCount(t *testing.T, m *Manager, ws string) int {
	t.Helper()
	var n int
	if err := m.db.QueryRow(
		`SELECT COUNT(*) FROM workspace_state WHERE workspace = ? AND state = 'permission'`, ws).Scan(&n); err != nil {
		t.Fatalf("counting permission rows: %v", err)
	}
	return n
}
