package workspace

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"testing"

	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

func TestParkedReadsTheHibernationTerminal(t *testing.T) {
	for _, tc := range []struct {
		name    string
		session *wsm.Session
		want    bool
	}{
		{
			name:    "the idle sweep stood the session down",
			session: &wsm.Session{Terminal: &wsm.SessionTerminal{Kind: wsm.TerminalHibernated}},
			want:    true,
		},
		{
			name:    "the session died some other way",
			session: &wsm.Session{Terminal: &wsm.SessionTerminal{Kind: "shim_died"}},
			want:    false,
		},
		{
			name:    "the session is alive",
			session: &wsm.Session{},
			want:    false,
		},
		{
			name:    "there is no session record at all",
			session: nil,
			want:    false,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			if tc.session != nil {
				session := *tc.session
				session.Workspace = "w1"
				f.db.sessions["w1"] = session
			}

			// Act.
			got, err := f.verbs.(*verbs).parked(context.Background(), "w1")

			// Assert.
			if err != nil {
				t.Fatalf("parked: %v", err)
			}
			if got != tc.want {
				t.Fatalf("parked = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestAnUnreadableSessionRecordIsNeverReadAsNotParked pins the discipline the
// rest of the daemon holds: "could not tell" is never the benign answer.
func TestAnUnreadableSessionRecordIsNeverReadAsNotParked(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.sessionErr = errors.New("boom")

	// Act.
	_, err := f.verbs.(*verbs).parked(context.Background(), "w1")

	// Assert.
	if err == nil {
		t.Fatal("parked() answered no error for an unreadable session record")
	}
}

func TestUnparkLiftsTheParkFromBothSessionScopedViews(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	f.verbs.(*verbs).unpark("w1")

	// Assert.
	if len(f.topbarParked) != 1 || f.topbarParked[0] {
		t.Fatalf("topbar parked = %v, want [false]", f.topbarParked)
	}
	if len(f.footer.parked) != 1 || f.footer.parked[0] {
		t.Fatalf("footer parked = %v, want [false]", f.footer.parked)
	}
}

// ---- Single flight: at most one revival in flight per workspace -----------

// reviveAsync runs reviveIfSessionless on its own goroutine and answers its
// outcome.
func reviveAsync(f *fixture, ctx context.Context, ws ids.WorkspaceID) <-chan error {
	done := make(chan error, 1)
	v := f.verbs.(*verbs)
	go func() {
		_, err := v.reviveIfSessionless(ctx, f.log.logger, opSelect, ws)
		done <- err
	}()
	return done
}

// arrangeJoinedFlight starts one leader revival of parked "w1", holds its
// Start at the gate, and starts `joiners` more callers, returning once every
// one of them has provably JOINED the leader's flight.
func arrangeJoinedFlight(t *testing.T, f *fixture, joiners int, ctx context.Context) (chan struct{}, []<-chan error) {
	t.Helper()
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")
	joined := make(chan ids.WorkspaceID, joiners)
	f.verbs.(*verbs).revivals.observeJoin = func(ws ids.WorkspaceID) { joined <- ws }
	entered, release := f.gateStarts(joiners + 1)
	outcomes := []<-chan error{reviveAsync(f, context.Background(), "w1")}
	receive(t, entered, "the leader's Start")
	for range joiners {
		outcomes = append(outcomes, reviveAsync(f, ctx, "w1"))
	}
	for range joiners {
		receive(t, joined, "a caller joining the in-flight revival")
	}
	return release, outcomes
}

func TestARevivalIsAPlainResumeAndNeverARebind(t *testing.T) {
	// Arrange: a revival brings back the conversation the workspace is already
	// on, so the shim keeps the book it persisted for it. Marking this a
	// rebind would let a rotated resume handle become the book and orphan
	// every record filed under the name it rotated away from.
	f := newFixture(t)
	release, outcomes := arrangeJoinedFlight(t, f, 0, context.Background())

	// Act.
	close(release)
	if err := receive(t, outcomes[0], "the revival's answer"); err != nil {
		t.Fatalf("reviveIfSessionless: %v", err)
	}

	// Assert.
	if len(f.fleet.rebound) != 0 {
		t.Fatalf("rebound starts = %+v, want none: a revival is a plain resume", f.fleet.rebound)
	}
}

func TestConcurrentRevivalsOfOneWorkspaceStartOneSession(t *testing.T) {
	// Arrange: one leader and four callers arriving while it runs.
	f := newFixture(t)
	release, outcomes := arrangeJoinedFlight(t, f, 4, context.Background())

	// Act.
	close(release)
	for _, outcome := range outcomes {
		if err := receive(t, outcome, "a revival's answer"); err != nil {
			t.Fatalf("reviveIfSessionless: %v", err)
		}
	}

	// Assert: exactly one restart, however many asked.
	if !slices.Equal(f.fleet.startCalls, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("starts = %v, want exactly one for w1", f.fleet.startCalls)
	}
}

func TestAJoinerAnswersTheLeadersFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.fleet.startErr = errFake
	release, outcomes := arrangeJoinedFlight(t, f, 1, context.Background())

	// Act.
	close(release)
	receive(t, outcomes[0], "the leader's answer")
	err := receive(t, outcomes[1], "the joiner's answer")

	// Assert: a failure reaches every caller that waited on it.
	if !errors.Is(err, errFake) {
		t.Fatalf("joiner answered %v, want the leader's failure", err)
	}
}

func TestAJoinerWhoseContextEndsStopsWaiting(t *testing.T) {
	// Arrange: the joiner's caller goes away while the leader still runs.
	f := newFixture(t)
	ctx, cancel := context.WithCancel(context.Background())
	release, outcomes := arrangeJoinedFlight(t, f, 1, ctx)

	// Act.
	cancel()
	err := receive(t, outcomes[1], "the joiner's answer")
	close(release)
	receive(t, outcomes[0], "the leader's answer")

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("joiner answered %v, want its own cancellation", err)
	}
}

func TestARevivalAfterTheFlightEndedLeadsAFreshOne(t *testing.T) {
	// Arrange: one completed revival, and the workspace parked again since.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")
	v := f.verbs.(*verbs)
	if _, err := v.reviveIfSessionless(context.Background(), f.log.logger, opSelect, "w1"); err != nil {
		t.Fatalf("first revival: %v", err)
	}

	// Act.
	if _, err := v.reviveIfSessionless(context.Background(), f.log.logger, opSelect, "w1"); err != nil {
		t.Fatalf("second revival: %v", err)
	}

	// Assert: a retired flight is never joined.
	if len(f.fleet.startCalls) != 2 {
		t.Fatalf("starts = %v, want two sequential revivals", f.fleet.startCalls)
	}
}

func TestRevivalsOfDifferentWorkspacesDoNotJoin(t *testing.T) {
	// Arrange: two parked workspaces, both held at the gate. Their bring-ups
	// fail once released, which keeps the two goroutines off the stub
	// session-scoped views (unpark) they would otherwise both write at once;
	// the subject here is only that both STARTS were entered.
	f := newFixture(t)
	f.fleet.startErr = errFake
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	f.hibernate("w1")
	f.hibernate("w2")
	entered, release := f.gateStarts(2)

	// Act.
	one := reviveAsync(f, context.Background(), "w1")
	two := reviveAsync(f, context.Background(), "w2")
	got := []ids.WorkspaceID{
		receive(t, entered, "a Start"),
		receive(t, entered, "the other Start"),
	}
	close(release)
	receive(t, one, "w1's answer")
	receive(t, two, "w2's answer")

	// Assert: the flight is keyed by workspace.
	slices.Sort(got)
	if !slices.Equal(got, []ids.WorkspaceID{"w1", "w2"}) {
		t.Fatalf("starts entered = %v, want one per workspace", got)
	}
}

// TestStartEndedByDaemonTellsTheDaemonLeavingFromAFailure covers the one
// classifier the register and the select revival share.
func TestStartEndedByDaemonTellsTheDaemonLeavingFromAFailure(t *testing.T) {
	tests := []struct {
		name      string
		err       error
		wantEnded bool
	}{
		{name: "the fleet's lifetime ended", err: fmt.Errorf("start: %w", context.Canceled), wantEnded: true},
		{name: "the daemon is standing down", err: fmt.Errorf("start: %w", shimclient.ErrStandingDown), wantEnded: true},
		{name: "the workspace was handed to a successor mid-start", err: fmt.Errorf("start: %w", ErrHandedOver), wantEnded: true},
		{name: "the bring-up failed", err: errFake, wantEnded: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			why, ended := startEndedByDaemon(tt.err)

			// Assert.
			if ended != tt.wantEnded || (ended && why == "") {
				t.Fatalf("startEndedByDaemon = (%q, %v), want ended=%v with a reason", why, ended, tt.wantEnded)
			}
		})
	}
}

// ---- A look starts any open workspace with no session behind it -----------

// lookAt runs reviveIfSessionless on the caller's goroutine.
func lookAt(f *fixture, ws ids.WorkspaceID) (bool, error) {
	return f.verbs.(*verbs).reviveIfSessionless(context.Background(), f.log.logger, opSelect, ws)
}

func TestALookStartsAnOpenWorkspaceWithNoSession(t *testing.T) {
	// Arrange: neither parked nor live.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	revived, err := lookAt(f, "w1")

	// Assert
	if err != nil || !revived {
		t.Fatalf("reviveIfSessionless = (%v, %v), want (true, nil)", revived, err)
	}
	if !slices.Equal(f.fleet.started, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("started = %v, want [w1]", f.fleet.started)
	}
}

func TestALookStartsNothingForAClosedWorkspace(t *testing.T) {
	// Arrange
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	ws.Closed = true
	f.db.with(ws)

	// Act
	revived, err := lookAt(f, "w1")

	// Assert
	if err != nil || revived {
		t.Fatalf("reviveIfSessionless = (%v, %v), want (false, nil)", revived, err)
	}
	if len(f.fleet.startCalls) != 0 {
		t.Fatalf("start calls = %v, want none for a closed workspace", f.fleet.startCalls)
	}
}

func TestALookAtAnUnparkedWorkspaceRaisesNoRevivingMarker(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	if _, err := lookAt(f, "w1"); err != nil {
		t.Fatalf("reviveIfSessionless: %v", err)
	}

	// Assert
	if _, _, reviving := f.sidebar.snapshot(); len(reviving) != 0 {
		t.Fatalf("reviving edges = %v, want none: the shimmer is the hibernation's", reviving)
	}
}

func TestALookWhoseStartFailsAnswersTheFailure(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.startErr = errors.New("boom")

	// Act
	revived, err := lookAt(f, "w1")

	// Assert
	if err == nil || revived {
		t.Fatalf("reviveIfSessionless = (%v, %v), want the start's failure", revived, err)
	}
	if got := recordLevel(f, "the looked-at workspace's session did not come up"); got != "error" {
		t.Fatalf("failure record level = %q, want error", got)
	}
}

func TestALookWhoseStartTheDaemonEndedRecordsNoError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.startErr = fmt.Errorf("start: %w", shimclient.ErrStandingDown)

	// Act
	if _, err := lookAt(f, "w1"); err == nil {
		t.Fatalf("reviveIfSessionless answered no error for a start that did not run")
	}

	// Assert
	if got := recordLevel(f, "the looked-at workspace's start stopped because this daemon is standing down"); got != "info" {
		t.Fatalf("record level = %q, want info", got)
	}
}

func TestALookAtAnUnreadableWorkspaceFails(t *testing.T) {
	// Arrange: no record answers for "ghost".
	f := newFixture(t)

	// Act
	revived, err := lookAt(f, "ghost")

	// Assert
	if err == nil || revived {
		t.Fatalf("reviveIfSessionless = (%v, %v), want the read's failure", revived, err)
	}
	if got := recordLevel(f, "could not read the workspace looked at with no session behind it"); got != "error" {
		t.Fatalf("failure record level = %q, want error", got)
	}
	if len(f.fleet.startCalls) != 0 {
		t.Fatalf("start calls = %v, want none for an unreadable workspace", f.fleet.startCalls)
	}
}
