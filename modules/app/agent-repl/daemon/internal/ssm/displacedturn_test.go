package ssm

import (
	"context"
	"errors"
	"testing"
)

// The user turn a merge lease displaces, and its automatic resumption.
//
// The condition under test is the one the refusal text used to hand back to
// the user as a chore ("resubmit once the merge finishes"): a merge takes a
// workspace's shim, the turn the user was waiting on is cut mid-sentence, and
// nothing ever puts it back. Each test pins one edge of the ledger that now
// does.

func TestAcquireRecordsTheDisplacedTurnOnTheOpenWindow(t *testing.T) {
	// Arrange.
	m, l, _, in, _ := openLeaseTest(t, "ws1")
	in.displaced = &DisplacedTurn{Prompt: "do the thing", PermissionMode: "acceptEdits"}

	// Act.
	release, err := l.Acquire(context.Background(), "ws1")
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	t.Cleanup(release)

	// Assert.
	got, err := m.DisplacedTurnFor("ws1")
	if err != nil {
		t.Fatalf("DisplacedTurnFor: %v", err)
	}
	if got == nil {
		t.Fatal("the held lease carries no displaced turn; the stop it delivered would be unrecoverable")
	}
	if got.Prompt != "do the thing" || got.PermissionMode != "acceptEdits" {
		t.Fatalf("displaced turn = %+v, want the stopped prompt and its mode", got)
	}
}

func TestAcquireRecordsNothingWhenNothingWasDisplaced(t *testing.T) {
	// Arrange. An idle workspace has no turn for the lease to take.
	m, l, _, in, _ := openLeaseTest(t, "ws1")
	in.displaced = nil

	// Act.
	release, err := l.Acquire(context.Background(), "ws1")
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	t.Cleanup(release)

	// Assert.
	got, err := m.DisplacedTurnFor("ws1")
	if err != nil {
		t.Fatalf("DisplacedTurnFor: %v", err)
	}
	if got != nil {
		t.Fatalf("displaced turn = %+v over an idle workspace, want nothing to put back", got)
	}
}

func TestReleaseResumesTheDisplacedTurn(t *testing.T) {
	// Arrange.
	_, l, _, in, _ := openLeaseTest(t, "ws1")
	in.displaced = &DisplacedTurn{Prompt: "do the thing", PermissionMode: "acceptEdits"}
	release, err := l.Acquire(context.Background(), "ws1")
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}

	// Act.
	release()

	// Assert.
	if len(in.resumed) != 1 {
		t.Fatalf("resumed %d turn(s), want exactly the one the merge stopped", len(in.resumed))
	}
	if in.resumed[0].Prompt != "do the thing" || in.resumed[0].PermissionMode != "acceptEdits" {
		t.Fatalf("resumed %+v, want the turn as it was cut", in.resumed[0])
	}
}

func TestReleaseResumesNothingWhenNothingWasDisplaced(t *testing.T) {
	// Arrange.
	_, l, _, in, _ := openLeaseTest(t, "ws1")

	// Act.
	release, err := l.Acquire(context.Background(), "ws1")
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	release()

	// Assert.
	if len(in.resumed) != 0 {
		t.Fatalf("resumed %d turn(s) over a lease that displaced nothing, want none", len(in.resumed))
	}
}

func TestReleaseResumesTheDisplacedTurnExactlyOnce(t *testing.T) {
	// Arrange. A second release must find nothing: a duplicated resume is the
	// user's work run twice, which is worse than the stall it replaces.
	_, l, _, in, _ := openLeaseTest(t, "ws1")
	in.displaced = &DisplacedTurn{Prompt: "do the thing"}
	release, err := l.Acquire(context.Background(), "ws1")
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}

	// Act.
	release()
	release()

	// Assert.
	if len(in.resumed) != 1 {
		t.Fatalf("resumed %d turn(s) across two releases, want exactly one", len(in.resumed))
	}
}

func TestReleaseClearsTheDisplacedTurnFromTheLedger(t *testing.T) {
	// Arrange. The clear is the exactly-once guarantee, not a tidy-up: it is
	// what a coordinator resuming this window after a bounce would read.
	m, l, _, in, _ := openLeaseTest(t, "ws1")
	in.displaced = &DisplacedTurn{Prompt: "do the thing"}
	release, err := l.Acquire(context.Background(), "ws1")
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}

	// Act.
	release()

	// Assert. A second acquire opens a fresh window, which must carry nothing
	// the first one left behind.
	in.displaced = nil
	release2, err := l.Acquire(context.Background(), "ws1")
	if err != nil {
		t.Fatalf("second Acquire: %v", err)
	}
	t.Cleanup(release2)
	got, err := m.DisplacedTurnFor("ws1")
	if err != nil {
		t.Fatalf("DisplacedTurnFor: %v", err)
	}
	if got != nil {
		t.Fatalf("the new window carries %+v, want the previous window's turn gone", got)
	}
}

func TestDisplacedTurnSurvivesADaemonBounce(t *testing.T) {
	// Arrange. THE CASE THE RESUME EXISTS FOR: a self-merge bounces the daemon
	// with the window still open, and the ledger is what carries the stopped
	// turn across it. A second Manager over the same database stands in for
	// the daemon that comes back.
	m, cl, path := openTest(t, fakeResolver{"s1": "ws1"})
	in := &fakeSessionAuthority{displaced: &DisplacedTurn{Prompt: "do the thing", PermissionMode: "acceptEdits"}}
	l, err := NewMergeLease(MergeLeaseConfig{Manager: m, Queue: &fakeQueue{}, Interrupter: in})
	if err != nil {
		t.Fatalf("NewMergeLease: %v", err)
	}
	if _, err := l.Acquire(context.Background(), "ws1"); err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	m.Close()

	// Act. Reopen the ledger, exactly as a restarted daemon does.
	reopened, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: fakeResolver{"s1": "ws1"}})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { reopened.Close() })

	// Assert.
	got, err := reopened.DisplacedTurnFor("ws1")
	if err != nil {
		t.Fatalf("DisplacedTurnFor after the bounce: %v", err)
	}
	if got == nil {
		t.Fatal("the displaced turn did not survive the bounce; a self-merge would lose the very turn it stopped")
	}
	if got.Prompt != "do the thing" || got.PermissionMode != "acceptEdits" {
		t.Fatalf("displaced turn after the bounce = %+v, want it unchanged", got)
	}
}

func TestReleaseStillReleasesWhenTheResumeFails(t *testing.T) {
	// Arrange. A resume that cannot be delivered must not hold the shim: the
	// lease's job is done, and refusing to release would keep every user
	// prompt refused over a merge that finished.
	m, l, _, in, cl := openLeaseTest(t, "ws1")
	in.displaced = &DisplacedTurn{Prompt: "do the thing"}
	in.resumeErr = errors.New("shim is gone")
	release, err := l.Acquire(context.Background(), "ws1")
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}

	// Act.
	release()

	// Assert.
	if m.MergeLeaseHeld("ws1") {
		t.Fatal("the lease is still held after a failed resume; user prompts would stay refused")
	}
	if !cl.contains("RESUME FAILED") {
		t.Fatal("the failed resume was not recorded; the user's turn vanished with no diagnostic")
	}
}

func TestAcquireProceedsWhenTheDisplacedTurnCannotBeRecorded(t *testing.T) {
	// Arrange. The record is best-effort BESIDE the lease, never a gate on it:
	// the merge the lease exists for must still run, and what is lost — the
	// automatic resume — is said out loud rather than inferred later from a
	// turn that never came back.
	m, l, _, in, cl := openLeaseTest(t, "ws1")
	in.displaced = &DisplacedTurn{Prompt: "do the thing"}
	// Break the store AFTER the window is open and BEFORE the record is
	// written: the interrupt is the only seam between the two.
	in.interruptHook = func(string) { m.db.Close() }

	// Act.
	release, err := l.Acquire(context.Background(), "ws1")

	// Assert.
	if err != nil {
		t.Fatalf("Acquire = %v, want the lease to stand despite the failed record", err)
	}
	if release == nil {
		t.Fatal("Acquire returned no release func")
	}
	if !cl.contains("displaced-turn record FAILED") {
		t.Fatal("the failed record was not surfaced; the lost resume would be silent")
	}
}
