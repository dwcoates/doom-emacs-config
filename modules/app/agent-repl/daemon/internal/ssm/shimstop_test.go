package ssm

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// CloseStaleTurn — the teardown's obligation to leave no live turn behind.
//
// The session-status lifecycle retires `thinking` on a `TurnEnded` and on nothing else, so a
// daemon-initiated shim stop kills the only process that could ever emit one.
// These tests pin each arm of the close: the write, every refusal, every
// validation error, and the guard the stale row uses to defend itself.
// ---------------------------------------------------------------------------

// CloseStaleTurn rejects each missing input separately, because a closing row
// with no workspace, no session or no reason names nothing.
func TestCloseStaleTurnRejectsMissingInputs(t *testing.T) {
	tests := []struct {
		name      string
		workspace string
		sessionID string
		reason    string
		wantErr   string
	}{
		{
			name:      "empty workspace",
			workspace: "", sessionID: "s1", reason: "hibernate",
			wantErr: "CloseStaleTurn got an empty workspace",
		},
		{
			name:      "empty session id",
			workspace: "ws1", sessionID: "", reason: "hibernate",
			wantErr: "got an empty session id",
		},
		{
			name:      "empty reason",
			workspace: "ws1", sessionID: "s1", reason: "",
			wantErr: "got an empty reason",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
			// Act.
			closed, err := m.CloseStaleTurn(tc.workspace, tc.sessionID, tc.reason, true)
			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("err = %v, want one containing %q", err, tc.wantErr)
			}
			if closed {
				t.Fatalf("closed = true, want false — a rejected call writes nothing")
			}
		})
	}
}

// The stop's whole point: a `thinking` whose session is being torn down is
// retired, and the workspace stops resolving THINKING.
func TestCloseStaleTurnClosesTheClaimantsThinking(t *testing.T) {
	// Arrange — a turn is running under s1.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("arrangement state = %s, want THINKING", renderName(got))
	}
	// Act.
	closed, err := m.CloseStaleTurn("ws1", "s1", "hibernate_session", true)
	// Assert.
	if err != nil {
		t.Fatalf("CloseStaleTurn: %v", err)
	}
	if !closed {
		t.Fatalf("closed = false, want true — the axis was latched and nothing else could retire it")
	}
	if got := mustCurrent(t, m, "ws1").State; got == frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = THINKING after the close, want anything else")
	}
	if !cl.contains("ssm: stale turn CLOSED ws=ws1 session=s1 reason=\"hibernate_session\"") {
		t.Fatalf("missing the canonical close record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// A shim that reported its own turn end before the stop landed leaves nothing
// stale, and the teardown says so rather than appending a second row.
func TestCloseStaleTurnWritesNothingOverASettledAxis(t *testing.T) {
	// Arrange — the turn ended honestly.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.Apply(evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Act.
	closed, err := m.CloseStaleTurn("ws1", "s1", "hibernate_session", true)
	// Assert.
	if err != nil {
		t.Fatalf("CloseStaleTurn: %v", err)
	}
	if closed {
		t.Fatalf("closed = true, want false — the shim's own end already closed the axis")
	}
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE preserved — the honest outcome must not be overwritten", renderName(got))
	}
	if !cl.contains("ssm: stale turn close ws=ws1 session=s1 reason=\"hibernate_session\" sole_session_controller=true — the session-status lifecycle holds no `thinking`") {
		t.Fatalf("missing the canonical no-op record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// A claim belonging to some OTHER session is not this stop's to spend, whatever
// soleSessionController says: closing it would blue out a turn the replacement is running.
func TestCloseStaleTurnDeclinesAnotherSessionsClaim(t *testing.T) {
	for _, soleSessionController := range []bool{true, false} {
		name := "sole session controller"
		if !soleSessionController {
			name = "not sole session controller"
		}
		t.Run(name, func(t *testing.T) {
			// Arrange — s2 holds the running turn.
			m, cl, _ := openTest(t, fakeResolver{"s2": "ws1"})
			if err := m.Apply(evTurnStarted("s2", 1)); err != nil {
				t.Fatalf("turn started: %v", err)
			}
			// Act — the stop is aimed at s1.
			closed, err := m.CloseStaleTurn("ws1", "s1", "hibernate_session_superseded", soleSessionController)
			// Assert.
			if err != nil {
				t.Fatalf("CloseStaleTurn: %v", err)
			}
			if closed {
				t.Fatalf("closed = true, want false — s2's live turn is not s1's stop to spend")
			}
			if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
				t.Fatalf("state = %s, want THINKING preserved", renderName(got))
			}
			if !cl.contains("ssm: stale turn close ws=ws1 session=s1 reason=\"hibernate_session_superseded\"") ||
				!cl.contains("DECLINED") {
				t.Fatalf("missing the canonical decline record; log:\n%s", strings.Join(cl.lines, "\n"))
			}
		})
	}
}

// An UNATTRIBUTED `thinking` — the row a permission close restores, and the one
// the observed wedge is made of — is the workspace's own teardown to spend, and
// nobody else's.
func TestCloseStaleTurnSpendsAnUnattributedClaimOnlyForTheSoleSessionController(t *testing.T) {
	tests := []struct {
		name                  string
		soleSessionController bool
		wantClosed            bool
		wantLog               string
	}{
		{
			name: "the workspace's own teardown closes it", soleSessionController: true, wantClosed: true,
			wantLog: "ssm: stale turn CLOSED ws=ws1 session=s1",
		},
		{
			name: "a stop aimed elsewhere leaves it for the live session controller", soleSessionController: false, wantClosed: false,
			wantLog: "DECLINED",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — a permission opened over a live turn and was answered,
			// which restores `thinking` with NO session id on it.
			m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
			if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
				t.Fatalf("turn started: %v", err)
			}
			if err := m.ApplyPermission("ws1", true, "ask"); err != nil {
				t.Fatalf("permission open: %v", err)
			}
			if err := m.ApplyPermission("ws1", false, "answered"); err != nil {
				t.Fatalf("permission close: %v", err)
			}
			if _, claimant, err := turnClaim(m.db, "ws1"); err != nil || claimant != "" {
				t.Fatalf("arrangement claimant = %q err = %v, want an unattributed claim", claimant, err)
			}
			// Act.
			closed, err := m.CloseStaleTurn("ws1", "s1", "hibernate_session", tc.soleSessionController)
			// Assert.
			if err != nil {
				t.Fatalf("CloseStaleTurn: %v", err)
			}
			if closed != tc.wantClosed {
				t.Fatalf("closed = %v, want %v", closed, tc.wantClosed)
			}
			if !cl.contains(tc.wantLog) {
				t.Fatalf("missing %q; log:\n%s", tc.wantLog, strings.Join(cl.lines, "\n"))
			}
		})
	}
}

// A pending permission cannot outlive the shim that asked the question, so the
// teardown releases it FIRST — otherwise the row buries the very `thinking` the
// close exists to retire and the workspace stays parked on a dead prompt.
func TestCloseStaleTurnReleasesAPendingPermissionAndThenTheTurnBeneathIt(t *testing.T) {
	// Arrange — a turn is running and the agent is parked on a question.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.ApplyPermission("ws1", true, "ask"); err != nil {
		t.Fatalf("permission open: %v", err)
	}
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_PERMISSION {
		t.Fatalf("arrangement state = %s, want PERMISSION", renderName(got))
	}
	// Act.
	closed, err := m.CloseStaleTurn("ws1", "s1", "hibernate_session", true)
	// Assert.
	if err != nil {
		t.Fatalf("CloseStaleTurn: %v", err)
	}
	if !closed {
		t.Fatalf("closed = false, want true — the permission buried a live turn that the stop must retire")
	}
	got := mustCurrent(t, m, "ws1").State
	if got == frontendv1.RenderState_RENDER_STATE_PERMISSION || got == frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want neither PERMISSION nor THINKING after the stop", renderName(got))
	}
	if !cl.contains("ssm: permission row released by hibernate_session ws=ws1") {
		t.Fatalf("missing the canonical permission release record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// The database failing is surfaced, never absorbed into a false "nothing to
// close".
func TestCloseStaleTurnSurfacesAStateReadFailure(t *testing.T) {
	// Arrange — the log is closed underneath the manager.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.db.Close(); err != nil {
		t.Fatalf("close db: %v", err)
	}
	// Act.
	closed, err := m.CloseStaleTurn("ws1", "s1", "hibernate_session", true)
	// Assert.
	if err == nil {
		t.Fatalf("err = nil, want the read failure surfaced")
	}
	if closed {
		t.Fatalf("closed = true, want false — nothing was written")
	}
}

// THE TRAP THIS CLOSE HAS TO SURVIVE. A stale `thinking` defends itself: the
// `ready|session_started` row that would supersede it is suppressed by the
// no-regress guard, whose `active` is read off the stale row. The closing row
// goes straight to the log rather than through Apply, so the guard never sees
// it and the next session's readiness is admitted normally.
func TestCloseStaleTurnUnblocksTheReadinessTheStaleRowWasSuppressing(t *testing.T) {
	// Arrange — a latched `thinking` suppresses readiness, as the wedge does.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.Apply(evSessionStarted("s1", 2)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	if !cl.contains("ssm: readiness suppressed (turn in flight) ws=ws1") {
		t.Fatalf("arrangement did not reproduce the suppression; log:\n%s", strings.Join(cl.lines, "\n"))
	}
	// Act — the teardown closes the axis, then the next session says hello.
	if _, err := m.CloseStaleTurn("ws1", "s1", "hibernate_session", true); err != nil {
		t.Fatalf("CloseStaleTurn: %v", err)
	}
	if err := m.Apply(evSessionStarted("s1", 3)); err != nil {
		t.Fatalf("session started after the close: %v", err)
	}
	// Assert.
	if cl.count("ssm: readiness suppressed (turn in flight) ws=ws1") != 1 {
		t.Fatalf("readiness suppressed again after the close; log:\n%s", strings.Join(cl.lines, "\n"))
	}
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY — the readiness row must be admitted over the closed axis", renderName(got))
	}
}

// The append itself failing is surfaced with closed=false: nothing landed, and
// reporting a write that did not happen would tell the teardown its obligation
// was met.
func TestCloseStaleTurnSurfacesAFailedAppend(t *testing.T) {
	// Arrange — a latched `thinking`, then the log is made unwritable.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if _, err := m.db.Exec(`CREATE TRIGGER refuse_rows BEFORE INSERT ON workspace_state
		BEGIN SELECT RAISE(ABORT, 'workspace_state is read-only'); END`); err != nil {
		t.Fatalf("install refusal trigger: %v", err)
	}
	// Act.
	closed, err := m.CloseStaleTurn("ws1", "s1", "hibernate_session", true)
	// Assert.
	if err == nil || !strings.Contains(err.Error(), "read-only") {
		t.Fatalf("err = %v, want the append failure surfaced", err)
	}
	if closed {
		t.Fatalf("closed = true, want false — no row landed")
	}
}
