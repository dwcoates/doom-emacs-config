package workspace

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
)

// underWay runs a start of WS that holds the start gate, as a real start
// does, until its context ends or LET closes, and answers when it entered and
// when it returned, with the cause its context ended on.
func underWay(t *testing.T, fleet *Fleet, ws string, let <-chan struct{}) (entered <-chan struct{}, returned <-chan error) {
	t.Helper()
	in, out := make(chan struct{}), make(chan error, 1)
	go func() {
		gate := fleet.startGate(wsID(ws))
		gate.Lock()
		defer gate.Unlock()
		ctx, done, err := fleet.enterStart(context.Background(), wsID(ws))
		if err != nil {
			close(in)
			out <- err
			return
		}
		defer done()
		close(in)
		select {
		case <-ctx.Done():
			out <- context.Cause(ctx)
		case <-let:
			out <- nil
		}
	}()
	return in, out
}

// wsID names a test workspace.
func wsID(ws string) ids.WorkspaceID { return ids.WorkspaceID(ws) }

// FenceHeldForTest reports whether WS's fence stands.
func (f *Fleet) FenceHeldForTest(ws ids.WorkspaceID) bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.fenced[ws]
}

func TestAStartAskedMidForgetFindsNoRowAndSpawnsNothing(t *testing.T) {
	// Arrange: the forget holds the fence.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	release, err := f.fleet.FenceStarts(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("FenceStarts: %v", err)
	}

	// Act: a start asked now (a revival on its own goroutine) waits on the
	// gate; the forget deletes the row, then releases.
	result := make(chan error, 1)
	go func() { result <- f.fleet.Start(context.Background(), ws.ID) }()
	if _, err := f.db.Forget(context.Background(), ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}
	release()
	err = <-result

	// Assert.
	if err == nil {
		t.Fatal("a start after the forget = nil error, want it refused")
	}
	if f.supervisor.spawnAttempts != 0 {
		t.Fatalf("spawn attempts = %d, want none for a forgotten workspace", f.supervisor.spawnAttempts)
	}
}

func TestAStartThatReadsTheFenceIsRefusedAndSpawnsNothing(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.fleet.mu.Lock()
	f.fleet.fenced[ws.ID] = true
	f.fleet.mu.Unlock()

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if !errors.Is(err, ErrWorkspaceForgotten) {
		t.Fatalf("Start = %v, want ErrWorkspaceForgotten", err)
	}
	if f.supervisor.spawnAttempts != 0 {
		t.Fatalf("spawn attempts = %d, want none for a workspace being forgotten", f.supervisor.spawnAttempts)
	}
}

func TestTheFenceEndsTheStartUnderWayAndWaitsForItToReturn(t *testing.T) {
	// Arrange: a start whose shim never attaches waits on its context alone.
	f := newFleetFixture(t)
	entered, returned := underWay(t, f.fleet, "w1", nil)
	<-entered

	// Act.
	release, err := f.fleet.FenceStarts(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("FenceStarts: %v", err)
	}
	defer release()
	select {
	case cause := <-returned:
		if !errors.Is(cause, ErrWorkspaceForgotten) {
			t.Fatalf("the start under way ended on %v, want ErrWorkspaceForgotten", cause)
		}
	default:
		t.Fatal("FenceStarts returned before the start under way had returned")
	}
}

func TestAFenceWhoseStartDoesNotReturnLiftsItselfAndSaysSo(t *testing.T) {
	// Arrange: a start that ignores its cancel, holding the gate.
	f := newFleetFixture(t)
	let := make(chan struct{})
	stubborn := make(chan struct{})
	go func() {
		gate := f.fleet.startGate("w1")
		gate.Lock()
		close(stubborn)
		<-let
		gate.Unlock()
	}()
	<-stubborn
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, err := f.fleet.FenceStarts(ctx, "w1")

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("FenceStarts = %v, want the bound's error", err)
	}
	if f.fleet.FenceHeldForTest("w1") {
		t.Fatal("a fence that gave up stands")
	}
	close(let)
	entered, returned := underWay(t, f.fleet, "w1", nil)
	<-entered
	select {
	case err := <-returned:
		t.Fatalf("a start after the lifted fence ended at once (%v), want it running", err)
	default:
	}
}

func TestTheFenceReleaseLiftsTheMark(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	release, err := f.fleet.FenceStarts(context.Background(), "w1")
	if err != nil {
		t.Fatalf("FenceStarts: %v", err)
	}

	// Act.
	release()

	// Assert.
	if f.fleet.FenceHeldForTest("w1") {
		t.Fatal("the fence stands after its release")
	}
}

func TestAStartTheFenceEndedSaysTheWorkspaceIsBeingForgotten(t *testing.T) {
	tests := []struct {
		name  string
		cause error
		err   error
		want  bool
	}{
		{name: "a start the fence cancelled", cause: ErrWorkspaceForgotten, err: context.Canceled, want: true},
		{name: "a start cancelled for another cause", cause: errors.New("the daemon is leaving"), err: context.Canceled, want: false},
		{name: "a start that succeeded", cause: ErrWorkspaceForgotten, err: nil, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ctx, cancel := context.WithCancelCause(context.Background())
			cancel(tt.cause)

			// Act
			got := endedByFence(ctx, "w1", tt.err)

			// Assert
			if errors.Is(got, ErrWorkspaceForgotten) != tt.want {
				t.Fatalf("endedByFence = %v, want ErrWorkspaceForgotten %v", got, tt.want)
			}
			if tt.err == nil && got != nil {
				t.Fatalf("endedByFence of a success = %v, want nil", got)
			}
		})
	}
}

func TestAForgottenWorkspacesStartReadsAsAStandDownThisDaemonOrdered(t *testing.T) {
	// Act
	err := endedByFence(func() context.Context {
		ctx, cancel := context.WithCancelCause(context.Background())
		cancel(ErrWorkspaceForgotten)
		return ctx
	}(), "w1", context.Canceled)

	// Assert
	if !errors.Is(err, shimclient.ErrStandDownOrdered) {
		t.Fatalf("err = %v, want it to read as a stand-down this daemon ordered", err)
	}
}
