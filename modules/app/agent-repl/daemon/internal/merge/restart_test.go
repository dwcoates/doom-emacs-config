package merge

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/dlog"
)

// waitingForFree admits the harness's merge into its wait for the workspace to
// fall free and answers the pump's end.
func waitingForFree(t *testing.T, h *harness) <-chan error {
	t.Helper()
	h.freeness.busy = true
	h.freeness.waiting = make(chan struct{})
	h.freeness.release = make(chan struct{})
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-h.freeness.waiting
	return done
}

// TestARestartSuspendsAMergeWaitingForItsWorkspace pins that a merge only
// waiting never blocks a restart: its wait lets go, its lease and record
// stand, and its bubble stays pinned.
func TestARestartSuspendsAMergeWaitingForItsWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	done := waitingForFree(t, h)

	// Act.
	err := h.o.SuspendForRestart(context.Background(), theWorkspace)

	// Assert.
	if err != nil {
		t.Fatalf("SuspendForRestart = %v, want the waiting merge suspended", err)
	}
	if err := <-done; err != nil {
		t.Fatalf("the pump was told %v, want no failure", err)
	}
	if _, held, _ := h.db.Lease(context.Background(), theWorkspace); !held {
		t.Fatal("the restart released the merge's lease, want it holding the workspace's prompts")
	}
	if step := recordedStep(t, h); step != TabQueue {
		t.Fatalf("the record stands at %q, want %q", step, TabQueue)
	}
	heads := h.feed.headPublications()
	if len(heads) == 0 {
		t.Fatal("the suspended waiting merge drew no bubble, want its bubble standing pinned")
	}
	if unpinned := livePublishedUnpinned(heads); len(unpinned) != 0 {
		t.Fatalf("the suspended waiting merge drew %v unpinned, want it pinned", unpinned)
	}
}

// TestAMergeWaitingThroughARestartWaitsAgainOnTheFreshSession covers the
// resume: the merge waits for freeness afresh with its bubble pinned, and
// starts and lands once the fresh session is free.
func TestAMergeWaitingThroughARestartWaitsAgainOnTheFreshSession(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	done := waitingForFree(t, h)
	if err := h.o.SuspendForRestart(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForRestart: %v", err)
	}
	<-done
	h.freeness.waiting = make(chan struct{})
	release := make(chan struct{})
	h.freeness.release = release
	h.emacsRepo()
	h.landsCleanly(mergeSHA)
	h.gatePasses("daemon")
	if err := h.o.ResumeAfterRestart(context.Background(), theWorkspace); err != nil {
		t.Fatalf("ResumeAfterRestart: %v", err)
	}

	// Act.
	resumed := admitAsync(h, context.Background())
	<-h.freeness.waiting
	headsWhileWaiting := h.feed.headPublications()
	h.freeness.busy = false
	close(release)
	err := <-resumed

	// Assert.
	if err != nil {
		t.Fatalf("the resumed merge ended on: %v", err)
	}
	if len(headsWhileWaiting) == 0 {
		t.Fatal("the resumed merge drew no bubble while it waited again, want its bubble pinned")
	}
	if unpinned := livePublishedUnpinned(headsWhileWaiting); len(unpinned) != 0 {
		t.Fatalf("the resumed merge drew %v unpinned while it waited again, want it pinned", unpinned)
	}
	if facts := h.footer.last(); facts.State != StateMerged {
		t.Fatalf("the merge stands at %q (%s), want it landed", facts.State, facts.Detail)
	}
}

// heldInGateForRestart is heldInGateForTransfer with the held gate scripted
// to pass, and released from the moment the restart asks for its boundary.
func heldInGateForRestart(t *testing.T, h *harness) <-chan error {
	t.Helper()
	release, done := heldInGateForTransfer(t, h)
	h.gatePasses("daemon")
	h.o.onBoundaryAsk = func() { close(release) }
	return done
}

// TestARestartLetsTheStepInFlightFinish pins the owner's stopping point
// (2026-10-10): a restart arriving mid-gate does not cut the tests; they
// finish, and the merge lets go at the next step's boundary.
func TestARestartLetsTheStepInFlightFinish(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	done := heldInGateForRestart(t, h)

	// Act.
	err := h.o.SuspendForRestart(context.Background(), theWorkspace)

	// Assert.
	if err != nil {
		t.Fatalf("SuspendForRestart = %v, want the merge stopped at its boundary", err)
	}
	if err := <-done; err != nil {
		t.Fatalf("the pump was told %v, want no failure", err)
	}
	if step := recordedStep(t, h); step != TabCommitting {
		t.Fatalf("the record stands at %q, want the step after the finished tests, %q", step, TabCommitting)
	}
}

// TestARestartRecordsTheBoundaryItStoppedAt pins the record: which step
// finished and which one the merge resumes at.
func TestARestartRecordsTheBoundaryItStoppedAt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	done := heldInGateForRestart(t, h)

	// Act.
	if err := h.o.SuspendForRestart(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForRestart: %v", err)
	}
	<-done

	// Assert.
	for _, record := range h.logs.Global().(*dlog.TestLogger).Records() {
		if record.Operation == "daemon.merge.restart" && record.Context["finished"] != nil {
			if record.Context["finished"] != TabTests || record.Context["step"] != TabCommitting {
				t.Fatalf("the boundary record is %+v, want tests finished and committing next", record.Context)
			}
			return
		}
	}
	t.Fatal("no record names the step boundary the merge stopped at")
}

// TestAMergeStoppedForARestartResumesAtItsBoundary covers the take-back: the
// merge goes on at the step it stopped before, in the same bubble, and the
// tests it finished are never run again.
func TestAMergeStoppedForARestartResumesAtItsBoundary(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	done := heldInGateForRestart(t, h)
	if err := h.o.SuspendForRestart(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForRestart: %v", err)
	}
	<-done
	lease, _, _ := h.db.Lease(context.Background(), theWorkspace)
	h.runner.mu.Lock()
	h.runner.before = nil
	gateRuns := len(h.runner.argv)
	h.runner.mu.Unlock()
	h.git.ancestry["0000000000000000000000000000000000000000>feature"] = true

	// Act.
	if err := h.o.ResumeAfterRestart(context.Background(), theWorkspace); err != nil {
		t.Fatalf("ResumeAfterRestart: %v", err)
	}
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the resumed merge ended on: %v", err)
	}

	// Assert.
	if facts := h.footer.last(); facts.State != StateMerged {
		t.Fatalf("the merge stands at %q (%s), want it landed", facts.State, facts.Detail)
	}
	h.runner.mu.Lock()
	rerun := len(h.runner.argv) - gateRuns
	h.runner.mu.Unlock()
	if rerun != 0 {
		t.Fatalf("the resumed merge ran the gate %d more time(s), want the finished tests never rerun", rerun)
	}
	for _, id := range h.feed.headIDs() {
		if id != headIDOf(lease.ID) {
			t.Fatalf("a head was drawn as %s, want every head the merge's own", id)
		}
	}
}

// TestARestartWhoseCallerLeavesWithdrawsItsAsk covers the restart's caller
// leaving before the boundary: the ask is taken back and the merge goes on to
// land, never stopping for a restart no one runs.
func TestARestartWhoseCallerLeavesWithdrawsItsAsk(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	release, done := heldInGateForTransfer(t, h)
	h.gatePasses("daemon")
	ctx, cancel := context.WithCancel(context.Background())
	h.o.onBoundaryAsk = cancel

	// Act.
	err := h.o.SuspendForRestart(ctx, theWorkspace)
	close(release)

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("SuspendForRestart = %v, want the caller's leaving", err)
	}
	if err := <-done; err != nil {
		t.Fatalf("the merge ended on %v, want it landed", err)
	}
	if facts := h.footer.last(); facts.State != StateMerged {
		t.Fatalf("the merge stands at %q (%s), want it landed after the withdrawn ask", facts.State, facts.Detail)
	}
}

// TestNoMergeIsAdmittedWhileItsWorkspaceRestarts pins the exclusion: between
// the suspension and the resume nothing of the workspace is admitted.
func TestNoMergeIsAdmittedWhileItsWorkspaceRestarts(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	if err := h.o.SuspendForRestart(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SuspendForRestart: %v", err)
	}
	_ = h.admit(context.Background())

	// Assert.
	if _, running := h.o.runFor(theWorkspace); running {
		t.Fatal("a merge was admitted while its workspace restarts")
	}
}
