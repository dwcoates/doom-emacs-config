package sessioncontroller

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/ssm"
)

// keepAlivePingClaim reports the ping claim a workspace currently holds.
func keepAlivePingClaim(t *testing.T, m *Manager, workspace string) string {
	t.Helper()
	m.mu.Lock()
	defer m.mu.Unlock()
	d, ok := m.byWS[workspace]
	if !ok {
		t.Fatalf("workspace %q has no live session controller", workspace)
	}
	return d.keepAliveTurnID
}

// submitPingThenAdvance submits one ping and moves the manager's clock forward
// by advanceMs, so the deadline is evaluated against a remembered submit instant
// rather than against a timer nobody can drive.
func submitPingThenAdvance(t *testing.T, m *Manager, advanceMs int64) string {
	t.Helper()
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	base := m.now()
	m.now = func() int64 { return base + advanceMs }
	return turnID
}

// overdueByMs is a comfortable margin past the policy's own deadline.
func overdueByMs(m *Manager) int64 {
	return m.keepAliveConfig().PingDeadline().Milliseconds() + 1
}

// THE WEDGE, CURED. A ping whose end never arrived held its claim forever, which
// declined every later ping, parked real prompts, refused hibernation and
// blocked deploys. The sweep retires it.
func TestSweepOverdueKeepAlivePingsReleasesTheStrandedClaim(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	submitPingThenAdvance(t, m, overdueByMs(m))

	// Act.
	closed := m.SweepOverdueKeepAlivePings()

	// Assert.
	if closed != 1 {
		t.Fatalf("SweepOverdueKeepAlivePings closed %d pings, want 1", closed)
	}
	if held := keepAlivePingClaim(t, m, "ws"); held != "" {
		t.Fatalf("the ping claim %q still stands after the sweep; every later ping is still declined behind it", held)
	}
}

// THE DURABLE CLAIM IS RETIRED TOO, and it is the half that matters outside this
// process: hibernation and every restart guard read the ledger, not the
// manager's memory.
func TestSweepOverdueKeepAlivePingsRetiresTheDurableClaim(t *testing.T) {
	// Arrange.
	m, applier, _ := keepAliveRig(t)
	turnID := submitPingThenAdvance(t, m, overdueByMs(m))

	// Act.
	m.SweepOverdueKeepAlivePings()

	// Assert.
	calls := applier.recordedOriginTurnCloses()
	if len(calls) != 1 {
		t.Fatalf("origin turn closes = %d, want exactly the one this ping's deadline licensed", len(calls))
	}
	got := calls[0]
	if got.workspace != "ws" || len(got.turnIDs) != 1 || got.turnIDs[0] != turnID {
		t.Fatalf("origin turn close = %+v, want the ping's own turn %q on ws", got, turnID)
	}
	if got.cause != ssm.TurnCloseKeepAliveOverdue {
		t.Fatalf("origin turn close cause = %q, want %q; a close that cannot name the lifecycle fact behind it is indistinguishable from a lost one",
			got.cause, ssm.TurnCloseKeepAliveOverdue)
	}
}

// A PING STILL INSIDE ITS DEADLINE IS LEFT ALONE. The bound exists to catch a
// lost boundary, and a sweep that reached a live ping would be killing the very
// turn it was sent to protect.
func TestSweepOverdueKeepAlivePingsLeavesALivePingAlone(t *testing.T) {
	// Arrange.
	m, applier, _ := keepAliveRig(t)
	turnID := submitPingThenAdvance(t, m, m.keepAliveConfig().PingDeadline().Milliseconds()-1)

	// Act.
	closed := m.SweepOverdueKeepAlivePings()

	// Assert.
	if closed != 0 {
		t.Fatalf("SweepOverdueKeepAlivePings closed %d pings, want 0 for one still inside its deadline", closed)
	}
	if held := keepAlivePingClaim(t, m, "ws"); held != turnID {
		t.Fatalf("ping claim = %q, want the live ping %q left exactly as it was", held, turnID)
	}
	if calls := applier.recordedOriginTurnCloses(); len(calls) != 0 {
		t.Fatalf("origin turn closes = %+v, want none; the ping's own end is still coming", calls)
	}
}

// A WORKSPACE WITH NO PING IN FLIGHT IS NOT TOUCHED. The sweep walks the whole
// fleet, and the great majority of sessions have no ping claim at all.
func TestSweepOverdueKeepAlivePingsIgnoresASessionWithNoPing(t *testing.T) {
	// Arrange.
	m, applier, _ := keepAliveRig(t)

	// Act.
	closed := m.SweepOverdueKeepAlivePings()

	// Assert.
	if closed != 0 || len(applier.recordedOriginTurnCloses()) != 0 {
		t.Fatalf("SweepOverdueKeepAlivePings touched a session with no ping (closed=%d calls=%+v)",
			closed, applier.recordedOriginTurnCloses())
	}
}

// THE WINDOW IS CLOSED WITH THE CLAIM. An open keep-alive window has no upper
// bound, so from its start onward every conversation item on the workspace is
// withheld from every rendering — the claim release alone would leave the
// session usable and the conversation invisible.
func TestSweepOverdueKeepAlivePingsClosesTheExclusionWindow(t *testing.T) {
	// Arrange.
	m, _, windows := keepAliveRig(t)
	turnID := submitPingThenAdvance(t, m, overdueByMs(m))

	// Act.
	m.SweepOverdueKeepAlivePings()

	// Assert.
	if _, ok := windows.closed[turnID]; !ok {
		t.Fatalf("the overdue ping's window %q was never closed; an unbounded window withholds every later item on this workspace from every rendering", turnID)
	}
}

// A FAILED LEDGER WRITE MUST NOT STRAND THE IN-MEMORY CLAIM ON TOP OF IT.
// Holding both halves is strictly worse than holding only the durable one: the
// queue would still park real prompts behind a ping nothing will end.
func TestSweepOverdueKeepAlivePingsReleasesTheClaimWhenTheLedgerRefuses(t *testing.T) {
	// Arrange.
	m, applier, _ := keepAliveRig(t)
	submitPingThenAdvance(t, m, overdueByMs(m))
	applier.reconcMutex.Lock()
	applier.originTurnErr = errOriginCloseRefused
	applier.reconcMutex.Unlock()

	// Act.
	closed := m.SweepOverdueKeepAlivePings()

	// Assert.
	if closed != 1 {
		t.Fatalf("SweepOverdueKeepAlivePings closed %d pings, want 1 even though the ledger refused", closed)
	}
	if held := keepAlivePingClaim(t, m, "ws"); held != "" {
		t.Fatalf("the ping claim %q survived a refused ledger write; the queue still parks real prompts behind it", held)
	}
}

// errOriginCloseRefused stands for an unwritable ledger.
var errOriginCloseRefused = errors.New("state log refused the origin turn close")

// ---------------------------------------------------------------------------
// A PLANNED BOUNCE IS NOT A LOST TURN BOUNDARY (restartepoch.go).
//
// The deadline is a wall-clock comparison, and a wall clock does not know a
// daemon was replaced. A ping in flight across a bounce accrued the whole
// window and was then declared OVERDUE — retiring its claim, closing a durable
// turn that was never lost, and logging the anomaly as a defect below this code.
// ---------------------------------------------------------------------------

// THE GAP IS GRANTED, NOT CHARGED. The ping is overdue on the raw clock and is
// left alone, because every millisecond of its lateness is the bounce's.
func TestSweepOverdueKeepAlivePingsGrantsThePlannedBounceWindow(t *testing.T) {
	// Arrange — a ping submitted before a bounce that spanned its whole deadline.
	m, _, _ := keepAliveRig(t)
	lease := &fakeLease{}
	if err := m.BindShutdownLease(lease); err != nil {
		t.Fatalf("BindShutdownLease: %v", err)
	}
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	base := m.now()
	gap := overdueByMs(m)
	lease.hold("sched-1")
	m.restartEpochNow()
	m.now = func() int64 { return base + gap }
	lease.hold("")

	// Act.
	closed := m.SweepOverdueKeepAlivePings()

	// Assert.
	if closed != 0 {
		t.Fatalf("SweepOverdueKeepAlivePings closed %d pings across a planned bounce, want none", closed)
	}
	if held := keepAlivePingClaim(t, m, "ws"); held != turnID {
		t.Fatalf("ping claim = %q after a bounce spanned the deadline, want %q still standing", held, turnID)
	}
}

// AND IT IS STILL A FAILURE BOUND. A ping that is genuinely wedged trips it one
// full deadline past the window, rather than never.
func TestSweepOverdueKeepAlivePingsStillRetiresAWedgedPingAfterTheWindow(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	lease := &fakeLease{}
	if err := m.BindShutdownLease(lease); err != nil {
		t.Fatalf("BindShutdownLease: %v", err)
	}
	if _, err := m.SubmitKeepAlivePing(context.Background(), "ws"); err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	base := m.now()
	gap := overdueByMs(m)
	lease.hold("sched-1")
	m.restartEpochNow()
	m.now = func() int64 { return base + gap }
	lease.hold("")
	m.restartEpochNow()

	// Act — a further full deadline elapses with the bounce long over.
	m.now = func() int64 { return base + gap + overdueByMs(m) }
	closed := m.SweepOverdueKeepAlivePings()

	// Assert.
	if closed != 1 {
		t.Fatalf("SweepOverdueKeepAlivePings closed %d pings a full deadline past the window, want 1", closed)
	}
}

// A PING SUBMITTED AFTER THE WINDOW IS OWED NOTHING. The grace is measured from
// the ping's own start, so an earlier bounce cannot loosen a later ping's bound.
func TestSweepOverdueKeepAlivePingsOwesNothingToAPingSubmittedAfterTheBounce(t *testing.T) {
	// Arrange — a bounce that ends BEFORE the ping is submitted.
	m, _, _ := keepAliveRig(t)
	lease := &fakeLease{}
	if err := m.BindShutdownLease(lease); err != nil {
		t.Fatalf("BindShutdownLease: %v", err)
	}
	base := m.now()
	lease.hold("sched-1")
	m.restartEpochNow()
	m.now = func() int64 { return base + 60_000 }
	lease.hold("")
	m.restartEpochNow()

	// Act.
	submitPingThenAdvance(t, m, overdueByMs(m))
	closed := m.SweepOverdueKeepAlivePings()

	// Assert.
	if closed != 1 {
		t.Fatalf("SweepOverdueKeepAlivePings closed %d pings submitted after the bounce, want 1", closed)
	}
}
